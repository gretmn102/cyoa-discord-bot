module IfEngine.Discord.View
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

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
[<RequireQualifiedAccess>]
module UserId =
    module Show =
        open FsharpMyExtension.ShowList

        let shows (userId: UserId) =
            showByToString userId

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

type ComponentState = Interaction.ComponentState<ComponentId, Data>
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
[<RequireQualifiedAccess>]
module ComponentState =
    let inline serialize (x: ComponentState) =
        ComponentState.serialize Data.Show.shows x

    let handler (viewId: FormId) : ComponentStateParsers<ComponentState> =
        let parse deserialize map =
            let parse (pos, str: string) =
                deserialize str.[pos..]

            ComponentStateParser.parseMap viewId parse map

        let create componentId =
            int componentId, parse deserialize (ComponentState.create viewId componentId)

        [
            create ComponentId.NextButtonId
            create ComponentId.SelectMenuId
        ]
        |> Map.ofList

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

let gameBelongsToSomeoneElseView (args : Model.GameBelongsToSomeoneElseViewArgs) =
    let b = DiscordMessageBuilder()
    b.Content <-
        sprintf "Здесь играет <@%d>, чтобы самому поиграть, введите `%s%s`"
            args.OwnerId
            args.CommandPrefix
            args.RawCommandName
    b
