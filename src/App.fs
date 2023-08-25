module App
open FsharpMyExtension
open Microsoft.Extensions.Logging
open System.Threading.Tasks
open DiscordBotExtensions
open DiscordBotExtensions.Extensions
open DiscordBotExtensions.Types
open DSharpPlus

let botEventId = new EventId(42, "Bot-Event")

let startMorai =
    "startMorai"
let startSome =
    "startSome"
let startFunnySockTales =
    "сказочки"

let moraiGame: Cyoa.Main.Game<_,_,_,_,_> =
    let args: IfEngine.Discord.View.CreateViewArgs<_, _> =
        {
            MessageCyoaId =
                "moraiGameId"
            ContentToEmbed =
                IfEngine.Discord.Utils.Content.ofCommon 2
            CustomOutputView =
                Cyoa.MoraiGame.customOutputView
        }

    {
        ViewArgs = args
        CreateGame =
            IfEngine.Engine.Engine.create
                Cyoa.MoraiGame.customStatementHandler
                Cyoa.MoraiGame.scenario
        InitGameState =
            IfEngine.State.init
                Cyoa.MoraiGame.beginLoc
                Map.empty
        DbCollectionName =
            "morai-game"
        RawCommandStart =
            startMorai
        SlashCommandStart =
            {|
                Name = "start-morai-tales"
                Description = "запустить морайские сказки"
            |}
    }

let someGame: Cyoa.Main.Game<_,_,_,_,_> =
    let args: IfEngine.Discord.View.CreateViewArgs<_, _> =
        {
            MessageCyoaId =
                "someGameId"
            ContentToEmbed =
                IfEngine.Discord.Utils.Content.ofCommon 2
            CustomOutputView =
                SomeGame.customOutputView
        }

    {
        ViewArgs = args
        CreateGame =
            IfEngine.Engine.Engine.create
                SomeGame.customStatementHandler
                SomeGame.scenario
        InitGameState =
            IfEngine.State.init
                SomeGame.beginLoc
                Map.empty
        DbCollectionName =
            "some-game"
        RawCommandStart =
            startSome
        SlashCommandStart =
            {|
                Name = "start-some-cyoa"
                Description = "Запустить какое-то CYOA с картинками"
            |}
    }

let funnySockTales: Cyoa.Main.Game<_,_,_,_,_> =
    let args: IfEngine.Discord.View.CreateViewArgs<_, _> =
        {
            MessageCyoaId =
                "funnySockTalesId"
            ContentToEmbed =
                IfEngine.Discord.Utils.Content.ofCommon 2
            CustomOutputView =
                FunnySockTales.customOutputView
        }

    {
        ViewArgs = args
        CreateGame =
            IfEngine.Engine.Engine.create
                FunnySockTales.customStatementHandler
                FunnySockTales.scenario
        InitGameState =
            IfEngine.State.init
                FunnySockTales.beginLoc
                Map.empty
        DbCollectionName =
            "funny_sock_tales"
        RawCommandStart =
            startFunnySockTales
        SlashCommandStart =
            {|
                Name = "start-funny-sock-tales"
                Description = "Запустить сказочки «Веселого носка»"
            |}
    }

let initBotModules restClient client (db: MongoDB.Driver.IMongoDatabase) =
    let create game =
        Cyoa.Main.create restClient client db game
    [|
        create moraiGame
        create someGame
        create funnySockTales
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

        let prefix = "."

        let client = new DiscordClient(config)
        let restClient = new DiscordRestClient(config)

        let botModules = initBotModules client restClient database

        let modulesSlashCommandDescriptions =
            botModules
            |> Array.choose (fun botModule ->
                botModule.InteractionCommands
                |> Option.map (fun xs ->
                    xs
                    |> Array.choose (fun x ->
                        match x with
                        | InteractionCommand.SlashCommand x ->
                            let command = x.Command
                            let commandName = x.CommandName
                            let description =
                                try
                                    command.DescriptionLocalizations.["ru"]
                                with e ->
                                    command.Description
                            Some(commandName, description)
                        | _ ->
                            None
                    )
                    |> Map.ofArray
                )
                |> Option.map (fun xs ->
                    {|
                        Name = "Доступные команды" // todo: add bot module name
                        DescriptionByName = xs
                    |}
                )
            )

        botModules
        |> BotModule.bindToClientsEvents
            prefix
            (fun client e ->
                let commands =
                    await <| client.GetGlobalApplicationCommandsAsync()

                let commands =
                    modulesSlashCommandDescriptions
                    |> Array.map (fun commandDescriptions ->
                        commands
                        |> Seq.choose (fun discordApplicationCommand ->
                            if discordApplicationCommand.Type = ApplicationCommandType.SlashCommand then
                                let commandName = discordApplicationCommand.Name
                                Map.tryFind commandName commandDescriptions.DescriptionByName
                                |> Option.map(fun description ->
                                    {|
                                        Id = discordApplicationCommand.Id
                                        Name = commandName
                                        Description = description
                                    |}
                                )
                            else
                                None
                        )
                        |> Seq.map (fun x ->
                            sprintf "• </%s:%d> — %s" x.Name x.Id x.Description
                        )
                        |> String.concat "\n"
                        // |> sprintf "%s:\n%s" commandDescriptions.Name // todo: module name
                    )

                let embed = DSharpPlus.Entities.DiscordEmbedBuilder()
                embed.Color <- DiscordEmbed.backgroundColorDarkTheme
                embed.Description <-
                    sprintf  "Доступные команды:\n%s"
                        (commands |> String.concat "\n")
                awaiti <| e.Channel.SendMessageAsync embed
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
