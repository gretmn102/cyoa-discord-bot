module IfEngine.Discord.Model
open FsharpMyExtension
open DiscordBotExtensions.Types
open DiscordBotExtensions.Mvc.Model
open IfEngine.Engine

[<RequireQualifiedAccess;Struct>]
type ViewCmd<'Content, 'CustomStatementOutput> =
    | StartNewGame of OutputMsg<'Content,'CustomStatementOutput>

[<RequireQualifiedAccess>]
type MyCmd<'Content,'Label,'CustomStatement, 'CustomStatementArg, 'CustomStatementOutput> =
    | MvcCmd of Cmd<ViewCmd<'Content, 'CustomStatementOutput>, MyCmd<'Content,'Label,'CustomStatement, 'CustomStatementArg, 'CustomStatementOutput>>
    | StartNewGame of Req<unit, IfEngine.State<'Content,'Label> * OutputMsg<'Content,'CustomStatementOutput>, MyCmd<'Content,'Label,'CustomStatement, 'CustomStatementArg, 'CustomStatementOutput>>
    | UpdateGame of Req<IfEngine.State<'Content,'Label> * InputMsg<'CustomStatementArg>, IfEngine.State<'Content,'Label> * OutputMsg<'Content,'CustomStatementOutput>, MyCmd<'Content,'Label,'CustomStatement, 'CustomStatementArg, 'CustomStatementOutput>>
    | SaveGameStateToDb of Req<UserId * IfEngine.State<'Content,'Label>, unit, MyCmd<'Content,'Label,'CustomStatement, 'CustomStatementArg, 'CustomStatementOutput>>
    | LoadGameStateFromDb of Req<UserId, IfEngine.State<'Content,'Label> option, MyCmd<'Content,'Label,'CustomStatement, 'CustomStatementArg, 'CustomStatementOutput>>
    | End

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
[<RequireQualifiedAccess>]
module MyCmd =
    let mvcCmd fn next =
        MyCmd.MvcCmd (fn (fun res ->
            next res
        ))

    let startNewGame arg next =
        MyCmd.StartNewGame(arg, next)

    let update gameState inputMsg next =
        MyCmd.UpdateGame((gameState, inputMsg), next)

    let saveGameStateToDb userId gameState next =
        MyCmd.SaveGameStateToDb((userId, gameState), next)

    let loadGameStateFromDb userId next =
        MyCmd.LoadGameStateFromDb(userId, next)

let startNewGame (userId: UserId) : MyCmd<'Content,'Label,'CustomStatement, 'CustomStatementArg, 'CustomStatementOutput> =
    pipeBackwardBuilder {
        let! gameState, gameOutputMsg = MyCmd.startNewGame ()
        let! _ = MyCmd.mvcCmd (Cmd.responseCreateView false (ViewCmd.StartNewGame gameOutputMsg))
        let! _ = MyCmd.saveGameStateToDb userId gameState
        return MyCmd.End
    }

let updateGame (userId: UserId) (gameInputMsg: InputMsg<'CustomStatementArg>) : MyCmd<'Content,'Label,'CustomStatement, 'CustomStatementArg, 'CustomStatementOutput> =
    pipeBackwardBuilder {
        let! gameState = MyCmd.loadGameStateFromDb userId
        match gameState with
        | Some gameState ->
            let! gameState, gameOutputMsg = MyCmd.update gameState gameInputMsg
            let! _ = MyCmd.mvcCmd (Cmd.responseUpdateCurrentView (ViewCmd.StartNewGame gameOutputMsg))
            let! _ = MyCmd.saveGameStateToDb userId gameState
            return MyCmd.End
        | None ->
            // TODO
            printfn "Game not exists!"
            return MyCmd.End
    }