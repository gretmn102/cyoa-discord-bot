module IfEngine.Discord.Index
open IfEngine.Engine
open DSharpPlus
open DSharpPlus.Entities
open DiscordBotExtensions.Extensions
open DiscordBotExtensions.Extensions.Interaction
open DiscordBotExtensions.Types
open FsharpMyExtension

type ComponentId =
    | NextButtonId = 0
    | SelectMenuId = 1

module FsharpMyExtension =
    module ShowList =
        open FsharpMyExtension.ShowList

        type IShow =
            abstract Shows : unit -> ShowS

    module FParsecExt =
        open FParsec

        let inline parser<'T, 'UserState when 'T : (static member GetParser: unit -> Parser<'T, 'UserState>)> =
            (^T : (static member GetParser: unit -> Parser<'T, 'UserState>) ())

    module Deserialization =
        let inline deserialize<'T when 'T : (static member Deserialize: string -> Result<'T,string>)> str =
            (^T : (static member Deserialize: string -> Result<'T,string>) str)

open FsharpMyExtension

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
[<RequireQualifiedAccess>]
module UserId =
    module Show =
        open FsharpMyExtension.ShowList

        let shows (userId: UserId) =
            shows userId

    module Parser =
        open FParsec

        let parser<'UserState> : Parser<UserId, 'UserState> =
            puint64

[<RequireQualifiedAccess>]
type Data =
    {
        OwnerId: UserId
    }

    // interface ShowList.IShow with
    //     member x.Shows() =
    //         Data.Show.shows x

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
[<RequireQualifiedAccess>]
module Data =
    let create (ownerId: UserId) : Data =
        {
            OwnerId = ownerId
        }

    module Show =
        open FsharpMyExtension.ShowList

        let shows (state: Data) : ShowS =
            UserId.Show.shows state.OwnerId

    module Parser =
        open FParsec

        let parser<'UserState> : Parser<Data, 'UserState> =
            UserId.Parser.parser |>> create

    let deserialize =
        FParsecExt.runResult Parser.parser

type Data with
    static member GetParser<'UserState> () : FParsec.Primitives.Parser<Data, 'UserState> =
        Data.Parser.parser

    static member Deserialize str =
        Data.deserialize str

[<RequireQualifiedAccess>]
type ComponentReturn =
    | NextButton of Data
    | SelectMenu of Data

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
[<RequireQualifiedAccess>]
module ComponentReturn =
    let getFormState = function
        | ComponentReturn.SelectMenu state
        | ComponentReturn.NextButton state ->
            state

    let toComponentId = function
        | ComponentReturn.SelectMenu _ -> ComponentId.SelectMenuId
        | ComponentReturn.NextButton _ -> ComponentId.NextButtonId

    let handler (viewId: FormId) : ComponentStateParsers<ComponentReturn> =
        let parse deserialize map =
            let parse (pos, str: string) =
                deserialize str.[pos..]

            ComponentStateParser.parseMap viewId parse map

        [
            int ComponentId.NextButtonId, parse Deserialization.deserialize ComponentReturn.NextButton
            int ComponentId.SelectMenuId, parse Deserialization.deserialize ComponentReturn.SelectMenu
        ]
        |> Map.ofList

type ComponentState = Interaction.ComponentState<ComponentId, Data>
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
[<RequireQualifiedAccess>]
module ComponentState =
    let inline serialize (x: ComponentState) =
        ComponentState.serialize Data.Show.shows x

    let inline tryDeserialize str: Result<ComponentState, _> option =
        ComponentState.tryDeserialize Data.Parser.parser str

type CreateViewArgs<'Content, 'CustomStatementOutput> =
    {
        MessageCyoaId: Interaction.FormId
        ContentToEmbed: 'Content -> DiscordEmbed
        CustomOutputView: 'CustomStatementOutput -> DiscordMessageBuilder
    }

let view
    (user: DiscordUser)
    (currentCommand: OutputMsg<'Content,'CustomStatementOutput>)
    (args: CreateViewArgs<'Content,'CustomStatementOutput>) =

    let addGameOwner (srcEmbed: DiscordEmbed) =
        let embed = DiscordEmbedBuilder(srcEmbed)
        embed.WithFooter(sprintf "Игра %s" user.Username) |> ignore
        embed.Build()

    match currentCommand with
    | OutputMsg.Print(content) ->
        let b = DiscordMessageBuilder()

        let componentState: ComponentState =
            Interaction.ComponentState.create
                args.MessageCyoaId
                ComponentId.NextButtonId
                {
                    OwnerId = user.Id
                }

        let nextButton =
            DiscordButtonComponent(ButtonStyle.Primary, ComponentState.serialize componentState, "...")

        b.Embed <- args.ContentToEmbed content |> addGameOwner

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
        b.Embed <- args.ContentToEmbed caption |> addGameOwner

        let options =
            choices
            |> List.mapi (fun i label ->
                DiscordSelectComponentOption(label, string i)
            )

        let componentState: ComponentState =
            Interaction.ComponentState.create
                args.MessageCyoaId
                ComponentId.SelectMenuId
                {
                    OwnerId = user.Id
                }

        let c =
            DiscordSelectComponent(ComponentState.serialize componentState, "select", options)
        b.AddComponents c |> ignore

        b
    | OutputMsg.CustomStatement(arg) ->
        args.CustomOutputView arg
