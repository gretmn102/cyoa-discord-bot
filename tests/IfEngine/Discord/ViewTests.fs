module Tests.IfEngine.Discord.View
open Fuchu
open FsharpMyExtension

open IfEngine.Discord.View

[<Tests>]
let componentStateSerializeTests =
    testList "componentStateSerializeTests" [
        testCase "base" <| fun () ->
            let exp =
                [
                    "cid"
                    "MessageCyoaId"
                    "0"
                    "1337"
                ] |> String.concat (ShowList.show ShowList.nl)
            let act =
                let componentState: ComponentState =
                    DiscordBotExtensions.Extensions.Interaction.ComponentState.create
                        "MessageCyoaId"
                        ComponentId.NextButtonId
                        {
                            OwnerId = 1337UL
                        }
                ComponentState.serialize componentState

            Assert.Equal("", exp, act)
    ]
