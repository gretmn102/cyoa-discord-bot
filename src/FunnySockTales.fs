module FunnySockTales
open IfEngine
open IfEngine.SyntaxTree
open IfEngine.SyntaxTree.Helpers
open Farkdown.Experimental.Helpers
open FsharpMyExtension.ResultExt
open IfEngine.Discord.SyntaxTree
open IfEngine.Engine

type CustomStatement = unit
type CustomStatementArg = unit

[<RequireQualifiedAccess>]
type Label =
    | Menu
    | SurpriseTales of SurpriseTales.Label
    | JungleTales of JungleTales.Label

let beginLoc = Label.Menu

let jungleNarrator =
    Narrator.create
        "Единолошад"
        "https://cdn.discordapp.com/attachments/912291464074117161/1144364493657358406/jungledrum700Avatar.webp"

let say content =
    CommonContentWithNarrator.createSay content

let menu content =
    CommonContentWithNarrator.createMenu content

module ScenarioConverter =
    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    [<RequireQualifiedAccess>]
    module Choice =
        let mapLabel blockMapLabel labelMapping ((description, body): Choice<_, 'OldLabel, _>) : Choice<_, 'NewLabel, _> =
            description, blockMapLabel labelMapping body

    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    [<RequireQualifiedAccess>]
    module Choices =
        let mapLabel blockMapLabel labelMapping (choices: Choices<_, 'OldLabel, _>) : Choices<_, 'NewLabel, _> =
            choices
            |> List.map (Choice.mapLabel blockMapLabel labelMapping)

    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    [<RequireQualifiedAccess>]
    module Stmt =
        let mapLabel blockMapLabel labelMapping (statement: Stmt<_, 'OldLabel, _>) : Stmt<_, 'NewLabel, _> =
            match statement with
            | Stmt.Say content ->
                Stmt.Say content
            | Stmt.InterpolationSay getContent ->
                Stmt.InterpolationSay getContent
            | Stmt.Jump label ->
                Stmt.Jump (labelMapping label)
            | Stmt.Menu(content, choices) ->
                Stmt.Menu(content, Choices.mapLabel blockMapLabel labelMapping choices)
            | Stmt.If(condition, thenBody, elseBody) ->
                Stmt.If(condition, blockMapLabel labelMapping thenBody, blockMapLabel labelMapping elseBody)
            | Stmt.ChangeVars updateVarContainer ->
                Stmt.ChangeVars updateVarContainer
            | Stmt.Addon customStatement ->
                Stmt.Addon customStatement

    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    [<RequireQualifiedAccess>]
    module Block =
        let rec mapLabel labelMapping (block: Block<_, 'OldLabel, _>) : Block<_, 'NewLabel, _> =
            block
            |> List.map (Stmt.mapLabel mapLabel labelMapping)

    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    [<RequireQualifiedAccess>]
    module NamedBlock =
        let mapLabel labelMapping ((label, block): NamedBlock<_, 'OldLabel, _>) : NamedBlock<_, 'NewLabel, _> =
            let label = labelMapping label
            label, Block.mapLabel labelMapping block

    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    [<RequireQualifiedAccess>]
    module Scenario =
        let mapLabel labelMapping (scenario: Scenario<_, 'OldLabel, _>) : Scenario<_, 'NewLabel,  _> =
            scenario
            |> Seq.fold
                (fun st (KeyValue(k, v)) ->
                    let key = labelMapping k
                    let v = NamedBlock.mapLabel labelMapping v
                    Map.add key v st
                )
                Map.empty

    let convert (labelMapping: 'OldLabel -> 'NewLabel) =
        Scenario.mapLabel labelMapping
        >> Seq.map (fun x -> x.Value)

let (scenario: Scenario<CommonContentWithNarrator, Label, CustomStatement>) =
    [
        label Label.Menu [
            menu [
                h1 [ text "Сказки «Веселого носка»" ] [
                    p [[ text "У нас много сказочниц и сказочников, кого хочешь послушать?" ]]
                ]
            ] [
                choice "Ее Скромное Носочество" [
                    jump (Label.SurpriseTales SurpriseTales.beginLoc)
                ]

                choice "Единорожью деву" [
                    jump (Label.JungleTales JungleTales.beginLoc)
                ]
            ]
        ]

        yield!
            SurpriseTales.scenario
            |> ScenarioConverter.convert Label.SurpriseTales

        yield!
            JungleTales.scenario
            |> ScenarioConverter.convert Label.JungleTales
    ]
    |> List.map (fun (labelName, body) -> labelName, (labelName, body))
    |> Map.ofList

type CustomStatementOutput = unit

let customStatementHandler : CustomStatementHandler<CommonContentWithNarrator,Label,CustomStatement,CustomStatementArg,CustomStatementOutput> =
    CustomStatementHandler.empty

let customOutputView (customOutput: CustomStatementOutput) =
    failwithf "has not implemented"
