module IfEngine.Discord.Controller
open DSharpPlus
open FsharpMyExtension
open FsharpMyExtension.Either
open FsharpMyExtension.ResultExt
open DiscordBotExtensions
open DiscordBotExtensions.Extensions
open IfEngine.Engine
open DiscordBotExtensions.Types

open IfEngine.Discord
open IfEngine.Discord.View
open IfEngine.Discord.Model

type Game<'Content,'Label,'CustomStatement,'CustomStatementArg,'CustomStatementOutput> =
    {
        ViewArgs: CreateViewArgs<'Content, 'CustomStatementOutput>
        CreateGame: IfEngine.State<'Content,'Label> -> Result<Engine<'Content,'Label,'CustomStatement,'CustomStatementArg,'CustomStatementOutput>, string>
        InitGameState: IfEngine.State<'Content,'Label>
        DbCollectionName: string
        RawCommandStart: string
        SlashCommandStart: {| Name: string; Description: string |}
    }

type State<'Content,'Label> =
    {
        Users: UserGamesStorage.Guilds<'Content,'Label>
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

[<RequireQualifiedAccess>]
type ViewComponentState =
    | GameView of ComponentState

module ViewComponentStatesManager =
    let create gameViewId =
        [
            Interaction.Form.map ViewComponentState.GameView (gameViewId, ComponentState.handler gameViewId)
        ]
        |> Map.ofList

type Msg =
    | Request of EventArgs.MessageCreateEventArgs * Action
    | RequestSlashCommand of EventArgs.InteractionCreateEventArgs * Action
    // todo: refact: rename to `RequestInteraction`
    | ComponentInteractionCreateEventHandler of DiscordClient * EventArgs.ComponentInteractionCreateEventArgs * ViewComponentState

let interpView
    (args: View.CreateViewArgs<'Content, 'CustomStatementOutput>)
    user
    (view: Model.AbstractView<'Content, 'CustomStatementOutput>)
    : Entities.DiscordMessageBuilder =

    match view with
    | AbstractView.StartNewGame gameMsg ->
        View.view user gameMsg args
    | AbstractView.GameBelongsToSomeoneElse args ->
        View.gameBelongsToSomeoneElseView args

let interp
    api
    (client: DiscordClient)
    (game: Game<'Content,'Label,'CustomStatement,'CustomStatementArg,'CustomStatementOutput>)
    (req: Model.AbstractGame<'Content,'Label,'CustomStatement, 'CustomStatementArg, 'CustomStatementOutput>)
    (state: State<'Content,'Label>) =

    let rec interp cmd state =
        match cmd with
        | Model.AbstractGame.DiscordApi cmd ->
            let cmd, state' =
                Mvc.Controller.interp api cmd state.MvcState

            let state =
                { state with
                    MvcState = state'
                }

            interp cmd state

        | Model.AbstractGame.SaveGameStateToDb((userId, gameState), next) ->
            let state =
                { state with
                    Users =
                        state.Users
                        |> UserGamesStorage.Guilds.set
                            userId
                            (fun x ->
                                UserGamesStorage.GuildData.Init
                                    (Some gameState)
                            )
                }

            interp (next ()) state

        | Model.AbstractGame.LoadGameStateFromDb(userId, next) ->
            let gameState =
                state.Users
                |> UserGamesStorage.Guilds.tryFindById userId
                |> Option.bind (fun x -> x.Data.GameState)

            interp (next gameState) state

        | Model.AbstractGame.StartNewGame((), next) ->
            let engine =
                game.CreateGame game.InitGameState |> Result.get // TODO

            let gameState = engine.GameState
            let gameOutputMsg = Engine.getCurrentOutputMsg engine
            let cmd = next (gameState, gameOutputMsg)
            interp cmd state

        | Model.AbstractGame.UpdateGame((currentGameState, gameInpugMsg), next) ->
            let engine =
                game.CreateGame currentGameState |> Result.get // TODO
                |> Engine.update gameInpugMsg |> Result.get
            let gameState = engine.GameState
            let gameOutputMsg = Engine.getCurrentOutputMsg engine
            let cmd = next (gameState, gameOutputMsg)
            interp cmd state

        | Model.AbstractGame.End -> state

    interp req state

let reduce
    (game: Game<'Content,'Label,'CustomStatement,'CustomStatementArg,'CustomStatementOutput>)
    (restClient: DiscordRestClient)
    (client: DiscordClient)
    (msg: Msg)
    (state: State<'Content,'Label>)
    : State<'Content,'Label> =

    match msg with
    | Request(e, action) ->
        match action with
        | MainActionCmd act ->
            let user = e.Author
            let interp =
                let api =
                    Mvc.Controller.createMessageApi
                        (interpView game.ViewArgs user)
                        (fun state ->
                            let req = Model.AbstractGame.End
                            req, state
                        )
                        restClient
                        e

                interp api client game

            match act with
            | StartCyoa ->
                let x = AbstractGame.startNewGame user.Id
                interp x state

    | RequestSlashCommand(e, action) ->
        match action with
        | MainActionCmd act ->
            let user = e.Interaction.User
            let interp =
                let api =
                    Mvc.Controller.createSlashCommandApi
                        (interpView game.ViewArgs user)
                        (fun state ->
                            let req = AbstractGame.Helpers.end'
                            req, state
                        )
                        restClient
                        e

                interp api client game

            match act with
            | StartCyoa ->
                interp (AbstractGame.startNewGame user.Id) state

    | ComponentInteractionCreateEventHandler(client, e, replyChannel) ->
        match replyChannel with
        | ViewComponentState.GameView componentState ->
            let commandPrefix = "." // todo

            let componentId = componentState.ComponentId

            let gameCommand =
                match componentId with
                | ComponentId.NextButtonId ->
                    InputMsg.Next
                | ComponentId.SelectMenuId ->
                    let selected = int e.Values.[0]
                    InputMsg.Choice selected
                | x ->
                    failwithf "%A componentId not implemented yet!" x

            let user = e.Interaction.User
            let interp =
                let api =
                    Mvc.Controller.createComponentInteractionApi
                        (interpView game.ViewArgs user)
                        (fun state ->
                            let req = AbstractGame.Helpers.end'
                            req, state
                        )
                        restClient
                        e

                interp api client game

            interp
                (AbstractGame.updateGame
                    commandPrefix
                    game.RawCommandStart
                    componentState.Data.OwnerId
                    user.Id
                    gameCommand)
                state

let reduceError msg =
    match msg with
    | Request(_, cmd)
    | RequestSlashCommand(_, cmd) ->
        match cmd with
        | MainActionCmd x ->
            match x with
            | StartCyoa -> ()
    | ComponentInteractionCreateEventHandler(_, _, _) ->
        ()

let create
    (client: DiscordClient)
    (restClient: DiscordRestClient)
    db
    (game: Game<'Content,'Label,'CustomStatement,'CustomStatementArg,'CustomStatementOutput>) =

    let m =
        let init: State<'Content,'Label> = {
            Users = UserGamesStorage.Guilds.init game.DbCollectionName db
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

    let componentInteractionCreateHandler (client: DiscordClient, e: EventArgs.ComponentInteractionCreateEventArgs) =
        let testIsMessageBelongToBot () next =
            if e.Message.Author.Id = client.CurrentUser.Id then
                next ()
            else
                false

        let restartComponent errMsg =
            try
                DiscordMessage.Ext.clearComponents e.Message
            with e ->
                printfn "%A" e.Message

            let b = Entities.DiscordInteractionResponseBuilder()
            b.Content <-
                [
                    sprintf "Вызовите эту комманду еще раз, потому что-то пошло не так:"
                    "```"
                    sprintf "%s" errMsg
                    "```"
                ] |> String.concat "\n"
            b.IsEphemeral <- true
            awaiti <| e.Interaction.CreateResponseAsync(InteractionResponseType.ChannelMessageWithSource, b)

        pipeBackwardBuilder {
            do! testIsMessageBelongToBot ()

            let input = e.Id

            let isHandled =
                Interaction.handleForms
                    (ViewComponentStatesManager.create game.ViewArgs.MessageCyoaId)
                    restartComponent
                    (fun viewAction -> ComponentInteractionCreateEventHandler(client, e, viewAction) |> m.Post)
                    input

            return isHandled
        }

    { BotModule.empty with
        MessageCreateEventHandleExclude =
            let exec: _ Action.Parser.Parser =
                Action.Parser.start game.RawCommandStart (fun (client: DiscordClient, e: EventArgs.MessageCreateEventArgs) msg ->
                    m.Post (Request (e, msg))
                )

            Some exec

        ComponentInteractionCreateHandle =
            let exec (client, e) =
                componentInteractionCreateHandler (client, e)

            Some exec

        InteractionCommands =
            Some commands
    }
