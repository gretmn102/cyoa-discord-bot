module App
open FsharpMyExtension
open Microsoft.Extensions.Logging
open System.Threading.Tasks
open DiscordBotExtensions
open DiscordBotExtensions.Types
open DSharpPlus

let botEventId = new EventId(42, "Bot-Event")

let startMorai =
    "startMorai"
let startSome =
    "startSome"
let startSurpriseTales =
    "сказочки"

let moraiGame: Cyoa.Main.Game<_,_,_,_,_> =
    {
        MessageCyoaId = "moraiGameId"
        CreateGame =
            Cyoa.MoraiGame.create
        ContentToEmbed =
            IfEngine.Discord.Utils.Content.ofCommon 2
        InitGameState =
            Cyoa.MoraiGame.initGameState
        DbCollectionName =
            "morai-game"
        StartCyoaCommand =
            startMorai
        CustomOutputView =
            Cyoa.MoraiGame.customOutputView
    }

let someGame: Cyoa.Main.Game<_,_,_,_,_> =
    {
        MessageCyoaId = "someGameId"
        CreateGame =
            SomeGame.create
        ContentToEmbed =
            IfEngine.Discord.Utils.Content.ofCommon 2
        InitGameState =
            SomeGame.initGameState
        DbCollectionName =
            "some-game"
        StartCyoaCommand =
            startSome
        CustomOutputView =
            SomeGame.customOutputView
    }

let surpriseTales: Cyoa.Main.Game<_,_,_,_,_> =
    {
        MessageCyoaId = "surpriseTalesId"
        CreateGame =
            SurpriseTales.create
        ContentToEmbed =
            IfEngine.Discord.Utils.Content.ofCommon 2
        InitGameState =
            SurpriseTales.initGameState
        DbCollectionName =
            "surprise_tales"
        StartCyoaCommand =
            startSurpriseTales
        CustomOutputView =
            SurpriseTales.customOutputView
    }

let initBotModules (db: MongoDB.Driver.IMongoDatabase) =
    [|
        Cyoa.Main.create moraiGame db
        Cyoa.Main.create someGame db
        Cyoa.Main.create surpriseTales db
    |]

open MongoDB.Driver
let initDb () =
    let dbConnection = EnvironmentExt.getEnvironmentVariable "DbConnection"

    let settings =
        MongoClientSettings.FromConnectionString (dbConnection)

    let client = new MongoClient(settings)
    let database =
        let dataBaseName =
            EnvironmentExt.getEnvironmentVariable "DataBaseName"

        client.GetDatabase(dataBaseName)

    database

open Saturn
open Giraffe

let startServer () =
    let port =
        match System.Environment.GetEnvironmentVariable("PORT") with
        | null -> uint16 8088
        | port -> uint16 port
    let publicPath = System.IO.Path.GetFullPath "./public"

    let app =
      application {
        use_static publicPath
        use_router (text "Hello World from Saturn")
    #if !DEBUG
        disable_diagnostics
    #endif
        url ("http://0.0.0.0:" + port.ToString() + "/")
      }

    run app

[<EntryPoint>]
let main argv =
    let tokenEnvVar = "BotToken"

    match EnvironmentExt.tryGetEnvironmentVariable tokenEnvVar with
    | None ->
        printfn "Environment variable `%s` is not set!" tokenEnvVar
        1
    | Some token ->
        let config = DSharpPlus.DiscordConfiguration()

        config.set_Token token
        config.set_TokenType DSharpPlus.TokenType.Bot
        config.set_AutoReconnect true
        config.set_Intents (
            DSharpPlus.DiscordIntents.AllUnprivileged
            ||| DSharpPlus.DiscordIntents.GuildMembers
            ||| DSharpPlus.DiscordIntents.GuildPresences
            ||| DSharpPlus.DiscordIntents.MessageContents
        )

        let client = new DSharpPlus.DiscordClient(config)

        let database = initDb ()
        let botModules = initBotModules database

        let prefix = "."

        botModules
        |> BotModule.bindToClientsEvents
            prefix
            (fun client e ->
                let b = Entities.DiscordMessageBuilder()
                let embed = Entities.DiscordEmbedBuilder()
                embed.Description <-
                    [
                        "Доступны следующие команды:"
                        startMorai
                        startSome
                        startSurpriseTales
                    ]
                    |> String.concat "\n"
                b.Embed <- embed
                awaiti <| e.Channel.SendMessageAsync(b)
            )
            (fun client e ->
                ()
            )
            (fun _ _ -> ())
            client

        client.add_Ready(Emzi0767.Utilities.AsyncEventHandler (fun client readyEventArgs ->
            client.Logger.LogInformation(botEventId, "Client is ready to process events.")

            Task.CompletedTask
        ))

        client.add_ClientErrored(Emzi0767.Utilities.AsyncEventHandler (fun client e ->
            client.Logger.LogError(botEventId, e.Exception, "Exception occured", [||])

            Task.CompletedTask
        ))

        client.add_GuildDownloadCompleted(Emzi0767.Utilities.AsyncEventHandler (fun client e ->
            let activity = DSharpPlus.Entities.DiscordActivity("Рассказываю истории!")
            awaiti <| client.UpdateStatusAsync(activity)

            Task.CompletedTask
        ))

        awaiti <| client.ConnectAsync()

        match argv with
        | [||] ->
            awaiti <| Task.Delay(-1)
        | [|"--server"|] ->
            startServer ()
        | xs ->
            printfn "unexpected args %A" xs

        0
