namespace IfEngine.Discord

module UserGamesStorage =
    open MongoDB.Driver
    open DiscordBotExtensions.Db
    open DiscordBotExtensions.Types

    type GuildData<'Content, 'Label> =
        { GameState: IfEngine.State<'Content, 'Label> option }

        static member Init: gameState: IfEngine.State<'Content, 'Label> option -> GuildData<'Content, 'Label>
        static member Empty: GuildData<'Content, 'Label>
        static member Serialize: data: GuildData<'Content, 'Label> -> string
        static member Deserialize: json: string -> Result<GuildData<'Content, 'Label>, string>

    type Version =
        | V0 = 0

    type Id = UserId
    type Guild<'Content, 'Label> = CommonDb.Data<Id, Version, GuildData<'Content, 'Label>>

    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    [<RequireQualifiedAccess>]
    module Guild =
        val create: id: Id -> data: GuildData<'Content, 'Label> -> Guild<'Content, 'Label>

    type Guilds<'Content, 'Label> = CommonDb.GuildData<Id, Version, GuildData<'Content, 'Label>>

    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    [<RequireQualifiedAccess>]
    module Guilds =
        val createData: id: Id -> Guild<'Content, 'Label>
        val init: collectionName: string -> db: IMongoDatabase -> Guilds<'Content, 'Label>

        val set:
            userId: Id ->
            setAdditionParams: (GuildData<'Content, 'Label> -> GuildData<'Content, 'Label>) ->
            guildData: Guilds<'Content, 'Label> ->
                Guilds<'Content, 'Label>

        val drop:
            db: IMongoDatabase ->
            items: Guilds<'Content, 'Label> ->
                CommonDb.GuildData<Id, Version, GuildData<'Content, 'Label>>

        val tryFindById: id: Id -> items: Guilds<'Content, 'Label> -> Guild<'Content, 'Label> option
