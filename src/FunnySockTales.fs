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
    | BoomerangTales of BoomerangTales.Label

let beginLoc = Label.Menu

let say content =
    NarratorCommonContent.createSay content

let menu content =
    NarratorCommonContent.createMenu content

let (scenario: Scenario<NarratorCommonContent, Label, CustomStatement>) =
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

                choice "Бумеранга" [
                    jump (Label.BoomerangTales BoomerangTales.beginLoc)
                ]
            ]
        ]

        yield!
            SurpriseTales.scenario
            |> Scenario.mapLabel Label.SurpriseTales
            |> Scenario.toNamedBlockSeq

        yield!
            JungleTales.scenario
            |> Scenario.mapLabel Label.JungleTales
            |> Scenario.toNamedBlockSeq

        yield!
            BoomerangTales.scenario
            |> Scenario.mapLabel Label.BoomerangTales
            |> Scenario.toNamedBlockSeq
    ]
    |> List.map (fun (labelName, body) -> labelName, (labelName, body))
    |> Map.ofList

type CustomStatementOutput = unit

let customStatementHandler : CustomStatementHandler<NarratorCommonContent,Label,CustomStatement,CustomStatementArg,CustomStatementOutput> =
    CustomStatementHandler.empty

let customOutputView (customOutput: CustomStatementOutput) =
    failwithf "has not implemented"
