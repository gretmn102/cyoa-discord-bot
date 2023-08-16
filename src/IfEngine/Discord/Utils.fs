module IfEngine.Discord.Utils
open IfEngine.SyntaxTree
open IfEngine.SyntaxTree.Helpers
open DSharpPlus.Entities
open DiscordBotExtensions.Extensions
open FsharpMyExtension.Either

type Content = DiscordEmbed

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
[<RequireQualifiedAccess>]
module Content =
    open Farkdown.Experimental.SyntaxTree

    module Farkdown =
        module Markdown =
            open FsharpMyExtension

            module Statement =
                let tryGetHeader = function
                    | Statement.Header(level, line, body) ->
                        Some({|
                            Level = level
                            Line = line
                            Body = body
                        |})
                    | _ ->
                        None

                let tryGetParagraph = function
                    | Statement.Paragraph body ->
                        Some body
                    | _ -> None

                let tryGetParagraphWithSingleImage = function
                    | Statement.Paragraph [[ LineElement.Image(src, title, alt) ]] ->
                        Some({|
                            Src = src
                            Title = title
                            Alt = alt
                        |})
                    | _ ->
                        None

                let isHeader = function
                    | Statement.Header _ -> true
                    | _ -> false

                let isParagraph = function
                    | Statement.Paragraph _ -> true
                    | _ -> false

                module FUniversalParser =
                    open FUniversalParser.Primitives

                    let pHeader<'UserState> : Pars<Statement,_,'UserState> =
                        satisfym
                            tryGetHeader
                            "Statement.Header"

                    let pH1<'UserState> : Pars<Statement,_,'UserState> =
                        satisfym
                            (tryGetHeader
                            >> Option.bind (fun x ->
                                if x.Level = 1 then Some x else None
                            ))
                            "Statement.Header 1"

                    let pParagraph<'UserState> : Pars<Statement,_,'UserState> =
                        satisfym
                            tryGetParagraph
                            "Statement.Paragraph"

                    let pParagraphWithSingleImage<'UserState> : Pars<Statement,_,'UserState> =
                        satisfym
                            tryGetParagraphWithSingleImage
                            "Statement.Paragraph [[ LineElement.Image _ ]]"

            module Document =
                module FUniversalParser =
                    open FUniversalParser.Primitives

                    open Statement.FUniversalParser

                    let parser<'UserState> : Pars<Statement, _, 'UserState> =
                        let pParagraphWithSingleImageEof : Pars<_, _, 'UserState> =
                            pParagraphWithSingleImage .>>? eof "eof"

                        let pStatementsLastImage : Pars<_, _, 'UserState> =
                            tuple2
                                (many (notFollowedBy pParagraphWithSingleImageEof >>. praw))
                                (opt pParagraphWithSingleImageEof)

                        let pH1 =
                            pH1
                            >>= fun header ->
                                (<|>)
                                    (eof "eof" >>= fun () ->
                                        getUserState
                                        >>= fun userState ->
                                            sub pStatementsLastImage (header.Body, userState)
                                            |>> fun (statements, image) ->
                                                {|
                                                    Title = Some header.Line
                                                    Description = statements
                                                    Image = image
                                                |})
                                    (pStatementsLastImage |>> fun (restStatements, image) ->
                                        {|
                                            Title = Some header.Line
                                            Description = header.Body @ restStatements
                                            Image = image
                                        |}
                                    )

                        let pStatementsLastImage =
                            pStatementsLastImage
                            |>> fun (description, image) ->
                                {|
                                    Title = None
                                    Description = description
                                    Image = image
                                |}

                        pH1 <|> pStatementsLastImage

                    let parse =
                        flip run parser

                let toDiscordEmbed indent (statements: Document) : DiscordEmbed =
                    let rest, result = FUniversalParser.parse statements
                    match result with
                    | Right x ->
                        let embedBuilder = DiscordEmbedBuilder()

                        x.Title
                        |> Option.iter (fun x ->
                            embedBuilder.Title <- Line.Show.show x |> ShowList.show
                        )

                        embedBuilder.Description <-
                            Document.serialize indent x.Description

                        x.Image
                        |> Option.iter (fun imageProps ->
                            embedBuilder.ImageUrl <- imageProps.Src
                        )

                        embedBuilder.Build()

                    | Left err ->
                        failwithf "%A\n%A" err rest

    let ofCommon spacesIndentSize (content: CommonContent.Content) : Content =
        Farkdown.Markdown.Document.toDiscordEmbed spacesIndentSize content

let say' (txt: string) : Content =
    let b = DiscordEmbedBuilder()
    b.Description <- txt
    b.Color <- Optional.FromValue(DiscordEmbed.backgroundColorDarkTheme)

    b.Build()

let say (txt: string) =
    Say (say' txt)

let says (txts: string list) =
    txts
    |> String.concat "\n"
    |> say'
    |> Say

let menu txts xs =
    let text =
        txts
        |> String.concat "\n"
        |> say'
    menu text xs
