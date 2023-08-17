module Cyoa.Model
open FsharpMyExtension
open MongoDB.Driver
open MongoDB.Bson
open DiscordBotExtensions.Types
open DiscordBotExtensions.Db

module Emptyable =
    let inline empty<'T when 'T : (static member Empty: 'T)> =
        (^T : (static member Empty: 'T) ())

module Users =
    type GuildData<'Content,'Label,'CustomStatement> =
        {
            GameState: IfEngine.State<'Content,'Label,'CustomStatement> option
        }
        static member Init gameState : GuildData<'Content,'Label,'CustomStatement> =
            {
                GameState = gameState
            }
        static member Empty : GuildData<'Content,'Label,'CustomStatement> =
            {
                GameState = None
            }
        static member Serialize (data: GuildData<'Content,'Label,'CustomStatement>) =
            data |> Json.ser
        static member Deserialize json =
            try
                let x: GuildData<'Content,'Label,'CustomStatement> = Json.des json
                Ok x
            with e ->
                Error e.Message

    type Version =
        | V0 = 0

    type Id = UserId

    type Guild<'Content,'Label,'CustomStatement> = CommonDb.Data<Id, Version, GuildData<'Content,'Label,'CustomStatement>>
    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    [<RequireQualifiedAccess>]
    module Guild =
        let create id data: Guild<'Content,'Label,'CustomStatement> =
            CommonDb.Data.create id Version.V0 data

    type Guilds<'Content,'Label,'CustomStatement> = CommonDb.GuildData<Id, Version, GuildData<'Content,'Label,'CustomStatement>>
    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    [<RequireQualifiedAccess>]
    module Guilds =
        let createData id : Guild<'Content,'Label,'CustomStatement> =
            Guild.create id Emptyable.empty

        let init collectionName (db: IMongoDatabase): Guilds<'Content,'Label,'CustomStatement> =
            MongoDB.Bson.Serialization.BsonSerializer.RegisterSerializer(
                typeof<IfEngine.State<'Content,'Label,'CustomStatement>>,
                new NewtonsoftSerializer<IfEngine.State<'Content,'Label,'CustomStatement>>()
            )

            CommonDb.GuildData.init
                createData
                (fun ver doc ->
                    match ver with
                    | Some ver ->
                        match ver with
                        | Version.V0 ->
                            None, Serialization.BsonSerializer.Deserialize<Guild<'Content,'Label,'CustomStatement>>(doc)
                        | x ->
                            failwithf "Version = %A not implemented\n%A" x doc
                    | None ->
                        failwithf "Version is empty:\n%A" doc
                )
                collectionName
                db

        let set userId setAdditionParams (guildData: Guilds<'Content,'Label,'CustomStatement>) : Guilds<'Content,'Label,'CustomStatement> =
            CommonDb.GuildData.set
                createData
                userId
                setAdditionParams
                guildData

        let drop (db: IMongoDatabase) (items: Guilds<'Content,'Label,'CustomStatement>) =
            CommonDb.GuildData.drop db items

        let tryFindById id (items: Guilds<'Content,'Label,'CustomStatement>): Guild<'Content,'Label,'CustomStatement> option =
            CommonDb.GuildData.tryFind id items
