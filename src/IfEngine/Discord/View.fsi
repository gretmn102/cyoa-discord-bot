module IfEngine.Discord.View

open IfEngine.Engine
open DSharpPlus.Entities
open DiscordBotExtensions.Extensions
open DiscordBotExtensions.Extensions.Interaction
open DiscordBotExtensions.Types

type ComponentId =
    | NextButtonId = 0
    | SelectMenuId = 1

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
[<RequireQualifiedAccess>]
module UserId =
    module Show =
        open FsharpMyExtension.ShowList
        val shows: userId: UserId -> ShowS

    module Parser =
        open FParsec
        val parser<'UserState> : Parser<UserId, 'UserState>

[<RequireQualifiedAccess>]
type Data = { OwnerId: UserId }

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
[<RequireQualifiedAccess>]
module Data =
    val create: ownerId: UserId -> Data

    module Show =
        open FsharpMyExtension.ShowList
        val shows: state: Data -> ShowS

    module Parser =
        open FParsec
        val parser<'UserState> : Parser<Data, 'UserState>

    val deserialize: (string -> Result<Data, string>)

type Data with

    static member GetParser: unit -> FParsec.Primitives.Parser<Data, 'UserState>
    static member Deserialize: str: string -> Result<Data, string>

type ComponentState = Interaction.ComponentState<ComponentId, Data>

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
[<RequireQualifiedAccess>]
module ComponentState =
    val inline serialize: x: ComponentState -> string
    val handler: viewId: FormId -> ComponentStateParsers<ComponentState>

type CreateViewArgs<'Content, 'CustomStatementOutput> =
    { MessageCyoaId: Interaction.FormId
      ContentToEmbed: 'Content -> DiscordEmbed
      CustomOutputView: 'CustomStatementOutput -> DiscordMessageBuilder }

val view:
    user: DiscordUser ->
    currentCommand: OutputMsg<'Content, 'CustomStatementOutput> ->
    args: CreateViewArgs<'Content, 'CustomStatementOutput> ->
        DiscordMessageBuilder

val gameBelongsToSomeoneElseView: args: Model.GameBelongsToSomeoneElseViewArgs -> DiscordMessageBuilder
