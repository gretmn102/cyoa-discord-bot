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
    ]
    |> List.map (fun (labelName, body) -> labelName, (labelName, body))
    |> Map.ofList

type CustomStatementOutput = unit

let customStatementHandler : CustomStatementHandler<NarratorCommonContent,Label,CustomStatement,CustomStatementArg,CustomStatementOutput> =
    CustomStatementHandler.empty

let customOutputView (customOutput: CustomStatementOutput) =
    failwithf "has not implemented"
