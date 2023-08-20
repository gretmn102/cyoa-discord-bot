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
        let createData id : Guild<'Content,'Label> =
            Guild.create id Emptyable.empty

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

        let set userId setAdditionParams (guildData: Guilds<'Content,'Label>) : Guilds<'Content,'Label> =
            CommonDb.GuildData.set
                createData
                userId
                setAdditionParams
                guildData

        let drop (db: IMongoDatabase) (items: Guilds<'Content,'Label>) =
            CommonDb.GuildData.drop db items

        let tryFindById id (items: Guilds<'Content,'Label>): Guild<'Content,'Label> option =
            CommonDb.GuildData.tryFind id items

open DiscordBotExtensions.Mvc.Model
open IfEngine.Engine

[<RequireQualifiedAccess;Struct>]
type ViewCmd<'Content, 'CustomStatementOutput> =
    | StartNewGame of OutputMsg<'Content,'CustomStatementOutput>

[<RequireQualifiedAccess>]
type GameReq<'Content,'Label, 'CustomStatementArg,'CustomStatementOutput,'Next> =
    | StartNewGame of Req<unit, IfEngine.State<'Content,'Label> * OutputMsg<'Content,'CustomStatementOutput>, 'Next>
    | Update of Req<IfEngine.State<'Content,'Label> * InputMsg<'CustomStatementArg>, IfEngine.State<'Content,'Label> * OutputMsg<'Content,'CustomStatementOutput>, 'Next>

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
[<RequireQualifiedAccess>]
module GameReq =
    let startNewGame arg next =
        GameReq.StartNewGame(arg, next)

    let update gameState inputMsg next =
        GameReq.Update((gameState, inputMsg), next)

[<RequireQualifiedAccess>]
type MyCmd<'Content,'Label,'CustomStatement, 'CustomStatementArg, 'CustomStatementOutput> =
    | MvcCmd of Cmd<ViewCmd<'Content, 'CustomStatementOutput>, MyCmd<'Content,'Label,'CustomStatement, 'CustomStatementArg, 'CustomStatementOutput>>
    | GameReq of GameReq<'Content,'Label,'CustomStatementArg,'CustomStatementOutput,MyCmd<'Content,'Label,'CustomStatement, 'CustomStatementArg, 'CustomStatementOutput>>
    | SaveGameStateToDb of Req<UserId * IfEngine.State<'Content,'Label>, unit, MyCmd<'Content,'Label,'CustomStatement, 'CustomStatementArg, 'CustomStatementOutput>>
    | LoadGameStateFromDb of Req<UserId, IfEngine.State<'Content,'Label> option, MyCmd<'Content,'Label,'CustomStatement, 'CustomStatementArg, 'CustomStatementOutput>>
    | End

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
[<RequireQualifiedAccess>]
module MyCmd =
    let mvcCmd fn next =
        MyCmd.MvcCmd (fn (fun res ->
            next res
        ))

    let gameReq fn next =
        MyCmd.GameReq (fn (fun res ->
            next res
        ))

    let saveGameStateToDb userId gameState next =
        MyCmd.SaveGameStateToDb((userId, gameState), next)

    let loadGameStateFromDb userId next =
        MyCmd.LoadGameStateFromDb(userId, next)

let startNewGame (userId: UserId) : MyCmd<'Content,'Label,'CustomStatement, 'CustomStatementArg, 'CustomStatementOutput> =
    pipeBackwardBuilder {
        let! gameState, gameOutputMsg = MyCmd.gameReq (GameReq.startNewGame ())
        let! _ = MyCmd.mvcCmd (Cmd.responseCreateView false (ViewCmd.StartNewGame gameOutputMsg))
        let! _ = MyCmd.saveGameStateToDb userId gameState
        return MyCmd.End
    }

let updateGame (userId: UserId) (gameInputMsg: InputMsg<'CustomStatementArg>) : MyCmd<'Content,'Label,'CustomStatement, 'CustomStatementArg, 'CustomStatementOutput> =
    pipeBackwardBuilder {
        let! gameState = MyCmd.loadGameStateFromDb userId
        match gameState with
        | Some gameState ->
            let! gameState, gameOutputMsg = MyCmd.gameReq (GameReq.update gameState gameInputMsg)
            let! _ = MyCmd.mvcCmd (Cmd.responseUpdateCurrentView (ViewCmd.StartNewGame gameOutputMsg))
            let! _ = MyCmd.saveGameStateToDb userId gameState
            return MyCmd.End
        | None ->
            // TODO
            printfn "Game not exists!"
            return MyCmd.End
    }
