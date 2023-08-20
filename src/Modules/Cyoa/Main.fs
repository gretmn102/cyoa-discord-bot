module Cyoa.Main
open DSharpPlus
open FsharpMyExtension
open FsharpMyExtension.Either
open FsharpMyExtension.ResultExt
open DiscordBotExtensions
open DiscordBotExtensions.Extensions
open IfEngine.Engine

open Model

type Game<'Content,'Label,'CustomStatement,'CustomStatementArg,'CustomStatementOutput> =
    {
        MessageCyoaId: Interaction.FormId
        ContentToEmbed: 'Content -> Entities.DiscordEmbed
        CustomOutputView: 'CustomStatementOutput -> Entities.DiscordMessageBuilder

        CreateGame: IfEngine.State<'Content,'Label> -> Result<Engine<'Content,'Label,'CustomStatement,'CustomStatementArg,'CustomStatementOutput>, string>
        InitGameState: IfEngine.State<'Content,'Label>
        DbCollectionName: string
        RawCommandStart: string
        SlashCommandStart: {| Name: string; Description: string |}
    }

type State<'Content,'Label> =
    {
        Users: Users.Guilds<'Content,'Label>
        MvcState: Mvc.Controller.State
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
    | RequestSlashCommand of EventArgs.InteractionCreateEventArgs * Action
    | ComponentInteractionCreateEventHandler of DiscordClient * EventArgs.ComponentInteractionCreateEventArgs * r: AsyncReplyChannel<bool>

let interpView
    (args: IfEngine.Discord.Index.CreateViewArgs<'Content, 'CustomStatementOutput>)
    user
    (view: Model.ViewCmd<'Content, 'CustomStatementOutput>)
    : Entities.DiscordMessageBuilder =

    match view with
    | ViewCmd.StartNewGame gameMsg ->
        IfEngine.Discord.Index.view user gameMsg args

let interp
    api
    (client: DiscordClient)
    (game: Game<'Content,'Label,'CustomStatement,'CustomStatementArg,'CustomStatementOutput>)
    (req: Model.MyCmd<'Content,'Label,'CustomStatement, 'CustomStatementArg, 'CustomStatementOutput>)
    (state: State<'Content,'Label>) =

    let rec interp cmd state =
        match cmd with
        | Model.MyCmd.MvcCmd cmd ->
            let cmd, state' =
                Mvc.Controller.interp api cmd state.MvcState

            let state =
                { state with
                    MvcState = state'
                }

            interp cmd state

        | Model.MyCmd.SaveGameStateToDb((userId, gameState), next) ->
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

            interp (next ()) state

        | Model.MyCmd.LoadGameStateFromDb(userId, next) ->
            let gameState =
                state.Users
                |> Users.Guilds.tryFindById userId
                |> Option.bind (fun x -> x.Data.GameState)

            interp (next gameState) state

        | Model.MyCmd.GameReq msg ->
            match msg with
            | GameReq.StartNewGame((), next) ->
                let engine =
                    game.CreateGame game.InitGameState |> Result.get // TODO

                let gameState = engine.GameState
                let gameOutputMsg = Engine.getCurrentOutputMsg engine
                let cmd = next (gameState, gameOutputMsg)
                interp cmd state

            | GameReq.Update((currentGameState, gameInpugMsg), next) ->
                let engine =
                    game.CreateGame currentGameState |> Result.get // TODO
                    |> Engine.update gameInpugMsg |> Result.get
                let gameState = engine.GameState
                let gameOutputMsg = Engine.getCurrentOutputMsg engine
                let cmd = next (gameState, gameOutputMsg)
                interp cmd state

        | Model.MyCmd.End -> state

    interp req state

let reduce
    (game: Game<'Content,'Label,'CustomStatement,'CustomStatementArg,'CustomStatementOutput>)
    (restClient: DiscordRestClient)
    (client: DiscordClient)
    (msg: Msg)
    (state: State<'Content,'Label>)
    : State<'Content,'Label> =

    let args: IfEngine.Discord.Index.CreateViewArgs<'Content, 'CustomStatementOutput> =
        {
            MessageCyoaId = game.MessageCyoaId
            ContentToEmbed = game.ContentToEmbed
            CustomOutputView = game.CustomOutputView
        }

    match msg with
    | Request(e, action) ->
        match action with
        | MainActionCmd act ->
            let user = e.Author
            let interp =
                let api =
                    Mvc.Controller.createMessageApi
                        (interpView args user)
                        (fun state ->
                            let req = Model.MyCmd.End
                            req, state
                        )
                        restClient
                        e

                interp api client game

            match act with
            | StartCyoa ->
                let x = Model.startNewGame user.Id
                interp x state

    | RequestSlashCommand(e, action) ->
        match action with
        | MainActionCmd act ->
            let user = e.Interaction.User
            let interp =
                let api =
                    Mvc.Controller.createSlashCommandApi
                        (interpView args user)
                        (fun state ->
                            let req = Model.MyCmd.End
                            req, state
                        )
                        restClient
                        e

                interp api client game

            match act with
            | StartCyoa ->
                interp (Model.startNewGame user.Id) state

    | ComponentInteractionCreateEventHandler(client, e, replyChannel) ->
        let commandPrefix = "."
        let result =
            IfEngine.Discord.Index.modalHandle
                game.MessageCyoaId
                (sprintf "%s%s" commandPrefix game.RawCommandStart)
                (fun userId gameCommand ->
                    let user = e.Interaction.User
                    let interp =
                        let api =
                            Mvc.Controller.createComponentInteractionApi
                                (interpView args user)
                                (fun state ->
                                    let req = Model.MyCmd.End
                                    req, state
                                )
                                restClient
                                e

                        interp api client game

                    interp (Model.updateGame user.Id gameCommand) state
                )
                client
                e

        replyChannel.Reply result.IsHandled

        result.UpdatedGameState
        |> Option.defaultValue state

let reduceError msg =
    match msg with
    | Request(_, cmd)
    | RequestSlashCommand(_, cmd) ->
        match cmd with
        | MainActionCmd x ->
            match x with
            | StartCyoa -> ()
    | ComponentInteractionCreateEventHandler(_, _, r) ->
        r.Reply true

let create
    (client: DiscordClient)
    (restClient: DiscordRestClient)
    db
    (game: Game<'Content,'Label,'CustomStatement,'CustomStatementArg,'CustomStatementOutput>) =

    let m =
        let init: State<'Content,'Label> = {
            Users = Users.Guilds.init game.DbCollectionName db
            MvcState = Mvc.Controller.State.empty
        }

        MailboxProcessor.Start (fun mail ->
            let rec loop (state: State<'Content,'Label>) =
                async {
                    let! msg = mail.Receive()
                    let state =
                        try
                            reduce game restClient client msg state
                        with e ->
                            reduceError msg
                            printfn "%A" e
                            state

                    return! loop state
                }
            loop init
        )

    let commands =
        let startGame =
            let slashCommandName = game.SlashCommandStart.Name
            InteractionCommand.SlashCommand {|
                CommandName = slashCommandName
                Command =
                    new Entities.DiscordApplicationCommand(
                        slashCommandName,
                        game.SlashCommandStart.Description,
                        ``type`` = ApplicationCommandType.SlashCommand
                    )
                Handler = fun e ->
                    m.Post(RequestSlashCommand(e, MainActionCmd StartCyoa))
            |}

        [|
            startGame
        |]

    { BotModule.empty with
        MessageCreateEventHandleExclude =
            let exec: _ Action.Parser.Parser =
                Action.Parser.start game.RawCommandStart (fun (client: DiscordClient, e: EventArgs.MessageCreateEventArgs) msg ->
                    m.Post (Request (e, msg))
                )

            Some exec

        ComponentInteractionCreateHandle =
            let exec (client, e) =
                m.PostAndReply(fun r -> ComponentInteractionCreateEventHandler(client, e, r))

            Some exec

        InteractionCommands =
            Some commands
    }
