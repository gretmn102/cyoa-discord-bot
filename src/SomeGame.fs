module SomeGame
open IfEngine
open IfEngine.SyntaxTree
open IfEngine.SyntaxTree.Helpers
open Farkdown.Experimental.Helpers
open FsharpMyExtension.ResultExt
open IfEngine.Discord.Utils

type CustomStatement = unit
type CustomStatementArg = unit

type Label =
    | Label1
    | Label2
    | Label3

let menu caption choices =
    CommonContentWithNarrator.createMenu caption choices

let say content =
    CommonContentWithNarrator.createSay content

let beginLoc: Label = Label1

let (scenario: Scenario<CommonContentWithNarrator, Label, CustomStatement>) =
    // https://imgur.com/a/FXkyeUC
    let images =
        [|
            "https://i.imgur.com/cwyAD9v.jpg"
            "https://i.imgur.com/q6va92F.jpg"
            "https://i.imgur.com/jA5FFtS.jpg"
            "https://i.imgur.com/UCJh9S8.jpg"
            "https://i.imgur.com/vW1k32H.jpg"
            "https://i.imgur.com/M8kApsD.jpg"
            "https://i.imgur.com/uOPC36L.jpg"
        |]
    let getScreen i =
        [ p [[ img images.[i - 1] "" "" ]] ]

    [
        label Label1 [
            menu (getScreen 1) [
                choice "Войти" [
                    jump Label2
                ]
                choice "Остаться на месте" [
                    jump Label3
                ]
            ]
        ]
        label Label2 [
            menu (getScreen 2) [
                choice "Вернуться" [
                    jump Label1
                ]
                choice "Исследовать местность" [
                    say (getScreen 4)
                ]
                choice "Зайти во дворец" [
                    say (getScreen 5)
                ]
            ]
        ]
        label Label3 [
            menu (getScreen 3) [
                choice "Передумать" [
                    jump Label1
                ]
                choice "Исследовать Лес Невозврата" [
                    menu (getScreen 6) [
                        choice "Войти" [
                            jump Label2
                        ]
                        choice "Остаться на месте" [
                            jump Label3
                        ]
                    ]
                ]
                choice "Войти во Дворец Зла" [
                    say (getScreen 7)
                ]
            ]
        ]
    ]
    |> List.map (fun (labelName, body) -> labelName, (labelName, body))
    |> Map.ofList
    : Scenario<_, _, _>

open IfEngine.Engine
type State = State<CommonContentWithNarrator, Label>
type CustomStatementOutput = unit
type Engine = Engine<CommonContentWithNarrator, Label, CustomStatement, CustomStatementArg, CustomStatementOutput>

let initGameState: State =
    State.init
        beginLoc
        Map.empty

let create (state: State) : Result<Engine, string> =
    Engine.create
        CustomStatementHandler.empty
        scenario
        state
