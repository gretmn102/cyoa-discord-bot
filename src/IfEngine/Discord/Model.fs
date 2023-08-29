namespace IfEngine.Discord.Model
open FsharpMyExtension
open DiscordBotExtensions.Types
open DiscordBotExtensions.Mvc.Model
open IfEngine.Engine

type GameBelongsToSomeoneElseViewArgs =
    {
        OwnerId: UserId
        CommandPrefix: string
        RawCommandName: string
    }
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
[<RequireQualifiedAccess>]
module GameBelongsToSomeoneElseViewArgs =
    let create ownerId commandPrefix rawCommandName =
        {
            OwnerId = ownerId
            CommandPrefix = commandPrefix
            RawCommandName = rawCommandName
        }

[<RequireQualifiedAccess>]
type AbstractView<'Content, 'CustomStatementOutput> =
    | StartNewGame of OutputMsg<'Content,'CustomStatementOutput>
    | GameBelongsToSomeoneElse of GameBelongsToSomeoneElseViewArgs

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
[<RequireQualifiedAccess>]
module AbstractView =
    let createGameBelongsToSomeoneElse x =
        AbstractView.GameBelongsToSomeoneElse x

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
        // todo: refact: rename to `discordApi`
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

    let testUserIsOwnerGame commandPrefix rawCommandName (ownerId: UserId) (userId: UserId) next =
        pipeBackwardBuilder {
            if ownerId <> userId then
                let! _ =
                    Helpers.mvcCmd
                        (Cmd.responseCreateView
                            true
                            (GameBelongsToSomeoneElseViewArgs.create ownerId commandPrefix rawCommandName
                             |> AbstractView.createGameBelongsToSomeoneElse))
                return Helpers.end'
            else
                return next ()
        }

    let startNewGame (userId: UserId) : AbstractGame<'Content,'Label,'CustomStatement, 'CustomStatementArg, 'CustomStatementOutput> =
        pipeBackwardBuilder {
            let! gameState, gameOutputMsg = Helpers.startNewGame ()
            let! _ = Helpers.mvcCmd (Cmd.responseCreateView false (AbstractView.StartNewGame gameOutputMsg))
            let! _ = Helpers.saveGameStateToDb userId gameState
            return Helpers.end'
        }

    let updateGame commandPrefix rawCommandName (ownerId: UserId) (userId: UserId) (gameInputMsg: InputMsg<'CustomStatementArg>) : AbstractGame<'Content,'Label,'CustomStatement, 'CustomStatementArg, 'CustomStatementOutput> =
        pipeBackwardBuilder {
            do! testUserIsOwnerGame commandPrefix rawCommandName ownerId userId
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
