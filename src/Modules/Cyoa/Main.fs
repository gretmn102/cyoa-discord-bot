module Cyoa.Main
open DSharpPlus
open FsharpMyExtension
open FsharpMyExtension.Either
open FsharpMyExtension.ResultExt
open DiscordBotExtensions
open DiscordBotExtensions.Types
open DiscordBotExtensions.Extensions
open IfEngine.Engine

open Model

type State<'Content,'Label> =
    {
        Users: Users.Guilds<'Content,'Label>
    }

type MainAction =
    | StartCyoa
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
[<RequireQualifiedAccess>]
module MainAction =
    module Parser =
        open FParsec

        open DiscordMessage.Parser

        type 'Result Parser = Primitives.Parser<'Result, unit>

        let startCyoa startCyoaCommand: _ Parser =
            skipStringCI startCyoaCommand .>> spaces

        let start startCyoaCommand: _ Parser =
            choice [
                startCyoa startCyoaCommand >>% StartCyoa
            ]

type Action =
    | MainActionCmd of MainAction

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
[<RequireQualifiedAccess>]
module Action =
    module Parser =
        open FParsec
        type 'Result Parser = Primitives.Parser<'Result, unit>

        let start startCyoaCommand f: _ Parser =
            choice [
                MainAction.Parser.start startCyoaCommand |>> MainActionCmd
            ]
            >>= fun msg ->
                preturn (fun x -> f x msg)

type Msg =
    | Request of EventArgs.MessageCreateEventArgs * Action
    | ComponentInteractionCreateEventHandler of DiscordClient * EventArgs.ComponentInteractionCreateEventArgs * r: AsyncReplyChannel<bool>

type Game<'Content,'Label,'CustomStatement,'CustomStatementArg,'CustomStatementOutput> =
    {
        MessageCyoaId: string
        CreateGame: IfEngine.State<'Content,'Label> -> Result<Engine<'Content,'Label,'CustomStatement,'CustomStatementArg,'CustomStatementOutput>, string>
        CustomOutputView: 'CustomStatementOutput -> Entities.DiscordMessageBuilder
        ContentToEmbed: 'Content -> Entities.DiscordEmbed
        InitGameState: IfEngine.State<'Content,'Label>
        DbCollectionName: string
        StartCyoaCommand: string
    }

let reduce
    (game: Game<'Content,'Label,'CustomStatement,'CustomStatementArg,'CustomStatementOutput>)
    (msg: Msg)
    (state: State<'Content,'Label>)
    : State<'Content,'Label> =

    match msg with
    | Request(e, action) ->
        match action with
        | MainActionCmd x ->
            match x with
            | StartCyoa ->
                let engine =
                    game.CreateGame game.InitGameState |> Result.get

                let initOutputMsg = Engine.getCurrentOutputMsg engine

                let msg =
                    IfEngine.Discord.Index.view
                        game.ContentToEmbed
                        game.MessageCyoaId
                        e.Author
                        game.CustomOutputView
                        initOutputMsg

                let state =
                    { state with
                        Users =
                            state.Users
                            |> Users.Guilds.set
                                e.Author.Id
                                (fun x ->
                                    Users.GuildData.Init
                                        (Some engine.GameState)
                                )
                    }

                awaiti <| e.Channel.SendMessageAsync msg

                state

    | ComponentInteractionCreateEventHandler(client, e, replyChannel) ->
        let commandPrefix = "."
        let result =
            IfEngine.Discord.Index.modalHandle
                game.MessageCyoaId
                (sprintf "%s%s" commandPrefix game.StartCyoaCommand)
                (fun userId gameCommand ->
                    let send msg =
                        let embed = Entities.DiscordEmbedBuilder()
                        embed.Color <- Entities.Optional.FromValue(DiscordEmbed.backgroundColorDarkTheme)
                        embed.Description <- msg

                        let b = Entities.DiscordMessageBuilder()
                        b.Embed <- embed.Build()

                        awaiti <| e.Channel.SendMessageAsync b

                    match Users.Guilds.tryFindById userId state.Users with
                    | Some user ->
                        match user.Data.GameState with
                        | Some gameState ->
                            let engine =
                                game.CreateGame gameState |> Result.get // TODO
                                |> Engine.update gameCommand |> Result.get

                            let userId = e.User.Id

                            let msg =
                                Engine.getCurrentOutputMsg engine
                                |> IfEngine.Discord.Index.view
                                    game.ContentToEmbed
                                    game.MessageCyoaId
                                    e.User
                                    game.CustomOutputView

                            let state =
                                { state with
                                    Users =
                                        state.Users
                                        |> Users.Guilds.set
                                            userId
                                            (fun _ ->
                                                Users.GuildData.Init
                                                    (Some engine.GameState)
                                            )
                                }

                            let msg2 = Entities.DiscordInteractionResponseBuilder()
                            msg2.AddEmbed msg.Embed |> ignore
                            msg2.AddComponents msg.Components |> ignore

                            awaiti <| e.Interaction.CreateResponseAsync(InteractionResponseType.UpdateMessage, msg2)

                            state

                        | None ->
                            let msg =
                                "Игра почему-то не начата."

                            send msg

                            state
                    | None ->
                        let msg =
                            "Игра почему-то не начата."

                        send msg

                        state
                )
                client
                e

        replyChannel.Reply result.IsHandled

        result.UpdatedGameState
        |> Option.defaultValue state

let reduceError msg =
    match msg with
    | Request(e, cmd) ->
        match cmd with
        | MainActionCmd x ->
            match x with
            | StartCyoa -> ()
    | ComponentInteractionCreateEventHandler(_, _, r) ->
        r.Reply true

let create (game: Game<'Content,'Label,'CustomStatement,'CustomStatementArg,'CustomStatementOutput>) db =
    let m =
        let init: State<'Content,'Label> = {
            Users = Users.Guilds.init game.DbCollectionName db
        }

        MailboxProcessor.Start (fun mail ->
            let rec loop (state: State<'Content,'Label>) =
                async {
                    let! msg = mail.Receive()
                    let state =
                        try
                            reduce game msg state
                        with e ->
                            reduceError msg
                            printfn "%A" e
                            state

                    return! loop state
                }
            loop init
        )

    { BotModule.empty with
        MessageCreateEventHandleExclude =
            let exec: _ Action.Parser.Parser =
                Action.Parser.start game.StartCyoaCommand (fun (client: DiscordClient, e: EventArgs.MessageCreateEventArgs) msg ->
                    m.Post (Request (e, msg))
                )

            Some exec

        ComponentInteractionCreateHandle =
            let exec (client, e) =
                m.PostAndReply(fun r -> ComponentInteractionCreateEventHandler(client, e, r))

            Some exec
    }
