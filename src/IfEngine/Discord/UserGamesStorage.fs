namespace IfEngine.Discord

module UserGamesStorage =
    open FsharpMyExtension
    open MongoDB.Driver
    open MongoDB.Bson
    open DiscordBotExtensions.Db
    open DiscordBotExtensions.Types

    type GuildData<'Content,'Label> =
        {
            GameState: IfEngine.State<'Content,'Label> option
        }
        static member Init gameState : GuildData<'Content,'Label> =
            {
                GameState = gameState
            }
        static member Empty : GuildData<'Content,'Label> =
            {
                GameState = None
            }
        static member Serialize (data: GuildData<'Content,'Label>) =
            data |> Json.ser
        static member Deserialize json =
            try
                let x: GuildData<'Content,'Label> = Json.des json
                Ok x
            with e ->
                Error e.Message

    type Version =
        | V0 = 0

    type Id = UserId

    type Guild<'Content,'Label> = CommonDb.Data<Id, Version, GuildData<'Content,'Label>>
    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    [<RequireQualifiedAccess>]
    module Guild =
        let create id data: Guild<'Content,'Label> =
            CommonDb.Data.create id Version.V0 data

    type Guilds<'Content,'Label> = CommonDb.GuildData<Id, Version, GuildData<'Content,'Label>>
    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    [<RequireQualifiedAccess>]
    module Guilds =
        let createData (id: Id) : Guild<'Content,'Label> =
            Guild.create id empty

        let init collectionName (db: IMongoDatabase): Guilds<'Content,'Label> =
            MongoDB.Bson.Serialization.BsonSerializer.RegisterSerializer(
                typeof<IfEngine.State<'Content,'Label>>,
                new NewtonsoftSerializer<IfEngine.State<'Content,'Label>>()
            )

            CommonDb.GuildData.init
                createData
                (fun ver doc ->
                    match ver with
                    | Some ver ->
                        match ver with
                        | Version.V0 ->
                            None, Serialization.BsonSerializer.Deserialize<Guild<'Content,'Label>>(doc)
                        | x ->
                            failwithf "Version = %A not implemented\n%A" x doc
                    | None ->
                        failwithf "Version is empty:\n%A" doc
                )
                collectionName
                db

        let set (userId: Id) setAdditionParams (guildData: Guilds<'Content,'Label>) : Guilds<'Content,'Label> =
            CommonDb.GuildData.set
                createData
                userId
                setAdditionParams
                guildData

        let drop (db: IMongoDatabase) (items: Guilds<'Content,'Label>) =
            CommonDb.GuildData.drop db items

        let tryFindById (id: Id) (items: Guilds<'Content,'Label>): Guild<'Content,'Label> option =
            CommonDb.GuildData.tryFind id items
