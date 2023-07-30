module IfEngine.Discord.Utils
open IfEngine.Types
open IfEngine.Utils
open DSharpPlus.Entities
open DiscordBotExtensions.Extensions

type Text = DiscordEmbed

let say' (txt: string) : Text =
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
