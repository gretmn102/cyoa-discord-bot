module Cyoa.Model
open FsharpMyExtension
open MongoDB.Driver
open MongoDB.Bson
open DiscordBotExtensions.Types
open DiscordBotExtensions.Db

module Users =
    type GuildData =
        {
            GameState: MoraiGame.State option
        }
        static member Init gameState : GuildData =
            {
                GameState = gameState
            }
        static member Empty =
            {
                GameState = None
            }
        static member Serialize (data: GuildData) =
            data |> Json.ser
        static member Deserialize json =
            try
                let x: GuildData = Json.des json
                Ok x
            with e ->
                Error e.Message

    type Version =
        | V0 = 0

    type Id = UserId

    type Guild = CommonDb.Data<Id, Version, GuildData>
    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    [<RequireQualifiedAccess>]
    module Guild =
        let create id data: Guild =
            CommonDb.Data.create id Version.V0 data

    type Guilds = CommonDb.GuildData<Id, Version, GuildData>
    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    [<RequireQualifiedAccess>]
    module Guilds =
        let createData id =
            Guild.create id GuildData.Empty

        let init collectionName (db: IMongoDatabase): Guilds =
            MongoDB.Bson.Serialization.BsonSerializer.RegisterSerializer(
                typeof<MoraiGame.State>,
                new NewtonsoftSerializer<MoraiGame.State>()
            )

            CommonDb.GuildData.init
                createData
                (fun ver doc ->
                    match ver with
                    | Some ver ->
                        match ver with
                        | Version.V0 ->
                            None, Serialization.BsonSerializer.Deserialize<Guild>(doc)
                        | x ->
                            failwithf "Version = %A not implemented\n%A" x doc
                    | None ->
                        failwithf "Version is empty:\n%A" doc
                )
                collectionName
                db

        let set userId setAdditionParams (guildData: Guilds) : Guilds =
            CommonDb.GuildData.set
                createData
                userId
                setAdditionParams
                guildData

        let drop (db: IMongoDatabase) (items: Guilds) =
            CommonDb.GuildData.drop db items

        let tryFindById id (items: Guilds): Guild option =
            CommonDb.GuildData.tryFind id items
