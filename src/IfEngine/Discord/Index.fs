module IfEngine.Discord.Index
open IfEngine.Engine
open DSharpPlus
open DSharpPlus.Entities
open DiscordBotExtensions
open DiscordBotExtensions.Extensions
open DiscordBotExtensions.Types

type ComponentId =
    | NextButtonId = 0
    | SelectMenuId = 1

type Data =
    {
        OwnerId: UserId
    }
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
[<RequireQualifiedAccess>]
module Data =
    module Printer =
        open FsharpMyExtension.ShowList

        open Interaction

        let show (data: Data) =
            shows data.OwnerId

    module Parser =
        open FParsec

        open Interaction

        let parse: _ ComponentState.Parser.Parser =
            puint64
            |>> fun userId ->
                {
                    OwnerId = userId
                }

type ComponentState = Interaction.ComponentState<ComponentId, Data>
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
[<RequireQualifiedAccess>]
module ComponentState =
    let inline serialize (x: ComponentState) =
        Interaction.ComponentState.serialize Data.Printer.show x

    let inline tryDeserialize str: Result<ComponentState, _> option =
        Interaction.ComponentState.tryDeserialize Data.Parser.parse str

let view contentToEmbed messageCyoaId ownerId handleCustomStatement (currentCommand: OutputMsg<'Content,'CustomStatement>) =
    match currentCommand with
    | OutputMsg.Print(content) ->
        let b = DiscordMessageBuilder()

        let componentState: ComponentState =
            Interaction.ComponentState.create
                messageCyoaId
                ComponentId.NextButtonId
                {
                    OwnerId = ownerId
                }

        let nextButton =
            DiscordButtonComponent(ButtonStyle.Primary, ComponentState.serialize componentState, "...")

        b.Embed <- contentToEmbed content

        b.AddComponents nextButton |> ignore
        b
    | OutputMsg.End ->
        let b = DiscordMessageBuilder()
        let embed = DiscordEmbedBuilder()
        embed.Description <- "Конец"
        b.Embed <- embed.Build()

        b
    | OutputMsg.Choices(caption, choices) ->
        let b = DiscordMessageBuilder()
        b.Embed <- contentToEmbed caption

        let options =
            choices
            |> List.mapi (fun i label ->
                DiscordSelectComponentOption(label, string i)
            )

        let componentState: ComponentState =
            Interaction.ComponentState.create
                messageCyoaId
                ComponentId.SelectMenuId
                {
                    OwnerId = ownerId
                }

        let c =
            DiscordSelectComponent(ComponentState.serialize componentState, "select", options)
        b.AddComponents c |> ignore

        b
    | OutputMsg.CustomStatement(arg) ->
        handleCustomStatement arg currentCommand

type ModalReturn<'GameState> =
    {
        IsHandled: bool
        UpdatedGameState: 'GameState option
    }

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
[<RequireQualifiedAccess>]
module ModalReturn =
    let create isHandled updateGameState =
        {
            IsHandled = isHandled
            UpdatedGameState = updateGameState
        }

let modalHandle messageTypeId commandName updateGame (client: DiscordClient) (e: EventArgs.ComponentInteractionCreateEventArgs) =
    let restartComponent errMsg =
        DiscordMessage.Ext.clearComponents e.Message

        let b = DiscordInteractionResponseBuilder()
        b.Content <-
            [
                sprintf "Вызовите комманду `%s` еще раз, потому что-то пошло не так:" commandName
                "```"
                sprintf "%s" errMsg
                "```"
            ] |> String.concat "\n"
        b.IsEphemeral <- true
        awaiti <| e.Interaction.CreateResponseAsync(InteractionResponseType.ChannelMessageWithSource, b)

    if e.Message.Author.Id = client.CurrentUser.Id then
        match ComponentState.tryDeserialize e.Id with
        | Some res ->
            match res with
            | Ok (componentState: ComponentState) ->
                if componentState.Id = messageTypeId then
                    let createReturn result =
                        ModalReturn.create true result

                    let userId = e.User.Id
                    if componentState.Data.OwnerId = userId then
                        match componentState.ComponentId with
                        | ComponentId.NextButtonId ->
                            updateGame userId InputMsg.Next
                            |> Some
                            |> createReturn
                        | ComponentId.SelectMenuId ->
                            let selected = int e.Values.[0]
                            updateGame userId (InputMsg.Choice selected)
                            |> Some
                            |> createReturn
                        | x ->
                            sprintf "expected data.ComponentId but %A" x
                            |> restartComponent

                            createReturn None
                    else
                        let b = DiscordInteractionResponseBuilder()
                        b.Content <-
                            sprintf "Здесь играет <@%d>, чтобы самому поиграть, введите `%s`"
                                componentState.Data.OwnerId
                                commandName
                        b.IsEphemeral <- true
                        awaiti <| e.Interaction.CreateResponseAsync(InteractionResponseType.ChannelMessageWithSource, b)

                        createReturn None
                else
                    ModalReturn.create false None
            | Error errMsg ->
                // restartComponent errMsg
                // createReturn None
                ModalReturn.create false None
        | _ ->
            ModalReturn.create false None
    else
        ModalReturn.create false None
