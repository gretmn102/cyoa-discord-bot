namespace IfEngine.Discord.Model
open FsharpMyExtension
open DiscordBotExtensions.Types
open DiscordBotExtensions.Mvc.Model
open IfEngine.Engine

[<RequireQualifiedAccess;Struct>]
type AbstractView<'Content, 'CustomStatementOutput> =
    | StartNewGame of OutputMsg<'Content,'CustomStatementOutput>

[<RequireQualifiedAccess>]
type AbstractGame<'Content,'Label,'CustomStatement, 'CustomStatementArg, 'CustomStatementOutput> =
    | MvcCmd of Cmd<AbstractView<'Content, 'CustomStatementOutput>, AbstractGame<'Content,'Label,'CustomStatement, 'CustomStatementArg, 'CustomStatementOutput>>
    | StartNewGame of Req<unit, IfEngine.State<'Content,'Label> * OutputMsg<'Content,'CustomStatementOutput>, AbstractGame<'Content,'Label,'CustomStatement, 'CustomStatementArg, 'CustomStatementOutput>>
    | UpdateGame of Req<IfEngine.State<'Content,'Label> * InputMsg<'CustomStatementArg>, IfEngine.State<'Content,'Label> * OutputMsg<'Content,'CustomStatementOutput>, AbstractGame<'Content,'Label,'CustomStatement, 'CustomStatementArg, 'CustomStatementOutput>>
    | SaveGameStateToDb of Req<UserId * IfEngine.State<'Content,'Label>, unit, AbstractGame<'Content,'Label,'CustomStatement, 'CustomStatementArg, 'CustomStatementOutput>>
    | LoadGameStateFromDb of Req<UserId, IfEngine.State<'Content,'Label> option, AbstractGame<'Content,'Label,'CustomStatement, 'CustomStatementArg, 'CustomStatementOutput>>
    | End

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
[<RequireQualifiedAccess>]
module AbstractGame =
    module Helpers =
        let mvcCmd fn next =
            AbstractGame.MvcCmd (fn (fun res ->
                next res
            ))

        let startNewGame arg next =
            AbstractGame.StartNewGame(arg, next)

        let update gameState inputMsg next =
            AbstractGame.UpdateGame((gameState, inputMsg), next)

        let saveGameStateToDb userId gameState next =
            AbstractGame.SaveGameStateToDb((userId, gameState), next)

        let loadGameStateFromDb userId next =
            AbstractGame.LoadGameStateFromDb(userId, next)

        let end' =
            AbstractGame.End

    let startNewGame (userId: UserId) : AbstractGame<'Content,'Label,'CustomStatement, 'CustomStatementArg, 'CustomStatementOutput> =
        pipeBackwardBuilder {
            let! gameState, gameOutputMsg = Helpers.startNewGame ()
            let! _ = Helpers.mvcCmd (Cmd.responseCreateView false (AbstractView.StartNewGame gameOutputMsg))
            let! _ = Helpers.saveGameStateToDb userId gameState
            return Helpers.end'
        }

    let updateGame (userId: UserId) (gameInputMsg: InputMsg<'CustomStatementArg>) : AbstractGame<'Content,'Label,'CustomStatement, 'CustomStatementArg, 'CustomStatementOutput> =
        pipeBackwardBuilder {
            let! gameState = Helpers.loadGameStateFromDb userId
            match gameState with
            | Some gameState ->
                let! gameState, gameOutputMsg = Helpers.update gameState gameInputMsg
                let! _ = Helpers.mvcCmd (Cmd.responseUpdateCurrentView (AbstractView.StartNewGame gameOutputMsg))
                let! _ = Helpers.saveGameStateToDb userId gameState
                return Helpers.end'
            | None ->
                // TODO
                printfn "Game not exists!"
                return Helpers.end'
        }
