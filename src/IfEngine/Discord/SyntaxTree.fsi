module IfEngine.Discord.SyntaxTree

open IfEngine.SyntaxTree
open DSharpPlus.Entities

type Content = DiscordEmbed

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
[<RequireQualifiedAccess>]
module Content =
    open Farkdown.Experimental.SyntaxTree

    module Farkdown =
        module Markdown =
            module Statement =
                val tryGetHeader:
                    Statement ->
                        {| Body: Statement list
                           Level: int
                           Line: Line |} option

                val tryGetParagraph: Statement -> Line list option

                val tryGetParagraphWithSingleImage:
                    Statement ->
                        {| Alt: string
                           Src: string
                           Title: string |} option

                val isHeader: Statement -> bool
                val isParagraph: Statement -> bool

                module FUniversalParser =
                    open FUniversalParser.Primitives

                    val pHeader<'UserState> :
                        Pars<Statement, {| Body: Statement list
                                           Level: int
                                           Line: Line |}, 'UserState>

                    val pH1<'UserState> :
                        Pars<Statement, {| Body: Statement list
                                           Level: int
                                           Line: Line |}, 'UserState>

                    val pParagraph<'UserState> : Pars<Statement, Line list, 'UserState>

                    val pParagraphWithSingleImage<'UserState> :
                        Pars<Statement, {| Alt: string
                                           Src: string
                                           Title: string |}, 'UserState>

            module Document =
                module FUniversalParser =
                    open FUniversalParser.Primitives

                    val parser<'UserState> :
                        Pars<Statement, {| Description: Statement list
                                           Image:
                                               {| Alt: string
                                                  Src: string
                                                  Title: string |} option
                                           Title: Line option |}, 'UserState>

                    val parse:
                        (CommonContent.Content
                            -> Reply<Statement, {| Description: Statement list
                                                   Image:
                                                       {| Alt: string
                                                          Src: string
                                                          Title: string |} option
                                                   Title: Line option |}, unit>)

                val toDiscordEmbed: indent: int -> statements: NarratorCommonContent -> DiscordEmbed

    val ofCommon: spacesIndentSize: int -> content: NarratorCommonContent -> Content

val say': txt: string -> Content
val say: txt: string -> Stmt<Content, 'a, 'b>
val says: txts: string list -> Stmt<Content, 'a, 'b>
val menu: txts: string seq -> xs: Choices<Content, 'a, 'b> -> Stmt<Content, 'a, 'b>
