module Cyoa.Main
open DSharpPlus
open FsharpMyExtension
open FsharpMyExtension.Either

open Types
open Extensions
open Model

type State =
    {
        Users: Users.Guilds
        MoraiGame: MoraiGame.Game
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

        module CommandNames =
            let startCyoa = "startCyoa"

        let startCyoa: _ Parser =
            skipStringCI CommandNames.startCyoa .>> spaces

        let start: _ Parser =
            choice [
                startCyoa >>% StartCyoa
            ]

type Action =
    | MainActionCmd of MainAction

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
[<RequireQualifiedAccess>]
module Action =
    module Parser =
        open FParsec
        type 'Result Parser = Primitives.Parser<'Result, unit>

        let start f: _ Parser =
            choice [
                MainAction.Parser.start |>> MainActionCmd
            ]
            >>= fun msg ->
                preturn (fun x -> f x msg)

type Msg =
    | Request of EventArgs.MessageCreateEventArgs * Action
    | ComponentInteractionCreateEventHandler of DiscordClient * EventArgs.ComponentInteractionCreateEventArgs * r: AsyncReplyChannel<bool>

let messageCyoaId = "moraiGameId"

let reduce (msg: Msg) (state: State): State =
    match msg with
    | Request(e, action) ->
        match action with
        | MainActionCmd x ->
            match x with
            | StartCyoa ->
                let msg =
                    IfEngine.Discord.Index.view
                        messageCyoaId
                        e.Author.Id
                        (fun _ ->
                            failwithf "handle custom statement not implemented"
                        )
                        state.MoraiGame.InitCommand

                let state =
                    { state with
                        Users =
                            state.Users
                            |> Users.Guilds.set
                                e.Author.Id
                                (fun x ->
                                    Users.GuildData.Init
                                        (Some state.MoraiGame.InitState)
                                )
                    }

                awaiti <| e.Channel.SendMessageAsync msg

                state

    | ComponentInteractionCreateEventHandler(client, e, replyChannel) ->
        let commandPrefix = "."
        let result =
            IfEngine.Discord.Index.modalHandle
                messageCyoaId
                (sprintf "%s%s" commandPrefix MainAction.Parser.CommandNames.startCyoa)
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
                            let command, gameState = state.MoraiGame.Update gameCommand gameState

                            let userId = e.User.Id

                            let msg =
                                command
                                |> IfEngine.Discord.Index.view
                                    messageCyoaId
                                    e.User.Id
                                    (fun _ ->
                                        failwithf "handle custom statement not implemented"
                                    )

                            let state =
                                { state with
                                    Users =
                                        state.Users
                                        |> Users.Guilds.set
                                            userId
                                            (fun x ->
                                                Users.GuildData.Init
                                                    (Some gameState)
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

let create db =
    let m =
        let init: State = {
            Users = Users.Guilds.init "cyoa" db
            MoraiGame = MoraiGame.game
        }

        MailboxProcessor.Start (fun mail ->
            let rec loop (state: State) =
                async {
                    let! msg = mail.Receive()
                    let state =
                        try
                            reduce msg state
                        with e ->
                            printfn "%A" e
                            state

                    return! loop state
                }
            loop init
        )

    { Shared.BotModule.empty with
        MessageCreateEventHandleExclude =
            let exec: _ Action.Parser.Parser =
                Action.Parser.start (fun (client: DiscordClient, e: EventArgs.MessageCreateEventArgs) msg ->
                    m.Post (Request (e, msg))
                )

            Some exec

        ComponentInteractionCreateHandle =
            let exec (client, e) =
                m.PostAndReply(fun r -> ComponentInteractionCreateEventHandler(client, e, r))

            Some exec
    }
