module IfEngine.Discord.Controller

open DSharpPlus
open DiscordBotExtensions
open DiscordBotExtensions.Extensions
open IfEngine.Engine

open IfEngine.Discord
open IfEngine.Discord.View
open IfEngine.Discord.Model

type Game<'Content, 'Label, 'CustomStatement, 'CustomStatementArg, 'CustomStatementOutput> =
    { ViewArgs: CreateViewArgs<'Content, 'CustomStatementOutput>
      CreateGame:
          IfEngine.State<'Content, 'Label>
              -> Result<Engine<'Content, 'Label, 'CustomStatement, 'CustomStatementArg, 'CustomStatementOutput>, string>
      InitGameState: IfEngine.State<'Content, 'Label>
      DbCollectionName: string
      RawCommandStart: string
      SlashCommandStart: {| Name: string; Description: string |} }

type State<'Content, 'Label> =
    { Users: UserGamesStorage.Guilds<'Content, 'Label>
      MvcState: Mvc.Controller.State }

type MainAction = | StartCyoa

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
[<RequireQualifiedAccess>]
module MainAction =
    module Parser =
        open FParsec

        open DiscordMessage.Parser

        type 'Result Parser = Primitives.Parser<'Result, unit>
        val startCyoa: startCyoaCommand: string -> unit Parser
        val start: startCyoaCommand: string -> MainAction Parser

type Action = MainActionCmd of MainAction

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
[<RequireQualifiedAccess>]
module Action =
    module Parser =
        open FParsec
        type 'Result Parser = Primitives.Parser<'Result, unit>
        val start: startCyoaCommand: string -> f: ('a -> Action -> 'b) -> ('a -> 'b) Parser

[<RequireQualifiedAccess>]
type ViewComponentState = GameView of ComponentState

module ViewComponentStatesManager =
    val create:
        gameViewId: Interaction.FormId -> Map<Interaction.FormId, Interaction.ComponentStateParsers<ViewComponentState>>

type Msg =
    | Request of EventArgs.MessageCreateEventArgs * Action
    | RequestSlashCommand of EventArgs.InteractionCreateEventArgs * Action
    | ComponentInteractionCreateEventHandler of
        DiscordClient *
        EventArgs.ComponentInteractionCreateEventArgs *
        ViewComponentState

val interpView:
    args: CreateViewArgs<'Content, 'CustomStatementOutput> ->
    user: Entities.DiscordUser ->
    view: AbstractView<'Content, 'CustomStatementOutput> ->
        Entities.DiscordMessageBuilder

val interp:
    api:
        Mvc.Controller.Api<AbstractView<'Content, 'CustomStatementOutput>, AbstractGame<'Content, 'Label, 'CustomStatement, 'CustomStatementArg, 'CustomStatementOutput>> ->
    client: DiscordClient ->
    game: Game<'Content, 'Label, 'CustomStatement, 'CustomStatementArg, 'CustomStatementOutput> ->
    req: AbstractGame<'Content, 'Label, 'CustomStatement, 'CustomStatementArg, 'CustomStatementOutput> ->
    state: State<'Content, 'Label> ->
        State<'Content, 'Label>

val reduce:
    game: Game<'Content, 'Label, 'CustomStatement, 'CustomStatementArg, 'CustomStatementOutput> ->
    restClient: DiscordRestClient ->
    client: DiscordClient ->
    msg: Msg ->
    state: State<'Content, 'Label> ->
        State<'Content, 'Label>

val reduceError: msg: Msg -> unit

val create:
    client: DiscordClient ->
    restClient: DiscordRestClient ->
    db: MongoDB.Driver.IMongoDatabase ->
    game: Game<'Content, 'Label, 'CustomStatement, 'CustomStatementArg, 'CustomStatementOutput> ->
        BotModule
