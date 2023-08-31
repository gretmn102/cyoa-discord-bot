namespace IfEngine.Discord.Model

open DiscordBotExtensions.Types
open DiscordBotExtensions.Mvc.Model
open IfEngine.Engine

type GameBelongsToSomeoneElseViewArgs =
    { OwnerId: UserId
      CommandPrefix: string
      RawCommandName: string }

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
[<RequireQualifiedAccess>]
module GameBelongsToSomeoneElseViewArgs =
    val create: ownerId: UserId -> commandPrefix: string -> rawCommandName: string -> GameBelongsToSomeoneElseViewArgs

[<RequireQualifiedAccess>]
type AbstractView<'Content, 'CustomStatementOutput> =
    | StartNewGame of OutputMsg<'Content, 'CustomStatementOutput>
    | GameBelongsToSomeoneElse of GameBelongsToSomeoneElseViewArgs

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
[<RequireQualifiedAccess>]
module AbstractView =
    val createGameBelongsToSomeoneElse: x: GameBelongsToSomeoneElseViewArgs -> AbstractView<'a, 'b>

[<RequireQualifiedAccess>]
type AbstractGame<'Content, 'Label, 'CustomStatement, 'CustomStatementArg, 'CustomStatementOutput> =
    | MvcCmd of
        Cmd<AbstractView<'Content, 'CustomStatementOutput>, AbstractGame<'Content, 'Label, 'CustomStatement, 'CustomStatementArg, 'CustomStatementOutput>>
    | StartNewGame of
        Req<unit, IfEngine.State<'Content, 'Label> * OutputMsg<'Content, 'CustomStatementOutput>, AbstractGame<'Content, 'Label, 'CustomStatement, 'CustomStatementArg, 'CustomStatementOutput>>
    | UpdateGame of
        Req<IfEngine.State<'Content, 'Label> * InputMsg<'CustomStatementArg>, IfEngine.State<'Content, 'Label> *
        OutputMsg<'Content, 'CustomStatementOutput>, AbstractGame<'Content, 'Label, 'CustomStatement, 'CustomStatementArg, 'CustomStatementOutput>>
    | SaveGameStateToDb of
        Req<UserId * IfEngine.State<'Content, 'Label>, unit, AbstractGame<'Content, 'Label, 'CustomStatement, 'CustomStatementArg, 'CustomStatementOutput>>
    | LoadGameStateFromDb of
        Req<UserId, IfEngine.State<'Content, 'Label> option, AbstractGame<'Content, 'Label, 'CustomStatement, 'CustomStatementArg, 'CustomStatementOutput>>
    | End

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
[<RequireQualifiedAccess>]
module AbstractGame =
    module Helpers =
        val discordApi:
            fn: (('a -> 'b) -> Cmd<AbstractView<'Content, 'CustomStatementOutput>, AbstractGame<'Content, 'Label, 'CustomStatement, 'CustomStatementArg, 'CustomStatementOutput>>) ->
            next: ('a -> 'b) ->
                AbstractGame<'Content, 'Label, 'CustomStatement, 'CustomStatementArg, 'CustomStatementOutput>

        val startNewGame:
            arg: unit ->
            next: (IfEngine.State<'Content, 'Label> * OutputMsg<'Content, 'CustomStatementOutput> -> AbstractGame<'Content, 'Label, 'CustomStatement, 'CustomStatementArg, 'CustomStatementOutput>) ->
                AbstractGame<'Content, 'Label, 'CustomStatement, 'CustomStatementArg, 'CustomStatementOutput>

        val update:
            gameState: IfEngine.State<'Content, 'Label> ->
            inputMsg: InputMsg<'CustomStatementArg> ->
            next: (IfEngine.State<'Content, 'Label> * OutputMsg<'Content, 'CustomStatementOutput> -> AbstractGame<'Content, 'Label, 'CustomStatement, 'CustomStatementArg, 'CustomStatementOutput>) ->
                AbstractGame<'Content, 'Label, 'CustomStatement, 'CustomStatementArg, 'CustomStatementOutput>

        val saveGameStateToDb:
            userId: UserId ->
            gameState: IfEngine.State<'Content, 'Label> ->
            next: (unit -> AbstractGame<'Content, 'Label, 'CustomStatement, 'CustomStatementArg, 'CustomStatementOutput>) ->
                AbstractGame<'Content, 'Label, 'CustomStatement, 'CustomStatementArg, 'CustomStatementOutput>

        val loadGameStateFromDb:
            userId: UserId ->
            next: (IfEngine.State<'Content, 'Label> option -> AbstractGame<'Content, 'Label, 'CustomStatement, 'CustomStatementArg, 'CustomStatementOutput>) ->
                AbstractGame<'Content, 'Label, 'CustomStatement, 'CustomStatementArg, 'CustomStatementOutput>

        val end': AbstractGame<'Content,'Label,'CustomStatement, 'CustomStatementArg, 'CustomStatementOutput>

    val testUserIsOwnerGame:
        commandPrefix: string ->
        rawCommandName: string ->
        ownerId: UserId ->
        userId: UserId ->
        next: (unit -> AbstractGame<'Content, 'Label, 'CustomStatement, 'CustomStatementArg, 'CustomStatementOutput>) ->
            AbstractGame<'Content, 'Label, 'CustomStatement, 'CustomStatementArg, 'CustomStatementOutput>

    val startNewGame:
        userId: UserId -> AbstractGame<'Content, 'Label, 'CustomStatement, 'CustomStatementArg, 'CustomStatementOutput>

    val updateGame:
        commandPrefix: string ->
        rawCommandName: string ->
        ownerId: UserId ->
        userId: UserId ->
        gameInputMsg: InputMsg<'CustomStatementArg> ->
            AbstractGame<'Content, 'Label, 'CustomStatement, 'CustomStatementArg, 'CustomStatementOutput>
