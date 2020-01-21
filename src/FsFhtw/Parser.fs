module Parser

open System
open Microsoft.FSharp.Quotations

let safeEquals (it : string) (theOther : string) =
    String.Equals(it, theOther, StringComparison.OrdinalIgnoreCase)

[<Literal>]
let HelpLabel = "Help"

let (|Sell|Buy|Help|ParseFailed|) (input : string) =
    let tryParseInt (arg : string) valueConstructor =
        let (worked, arg') = Int32.TryParse arg
        if worked then valueConstructor arg' else ParseFailed

    let parts = input.Split(' ') |> List.ofArray
    match parts with
    | [ verb ] when safeEquals verb HelpLabel -> Help
    | [ verb; arg ] when safeEquals verb (nameof Domain.PlayerAction.Sell) ->
        tryParseInt arg (fun value -> Sell value)
    | [ verb; arg ] when safeEquals verb (nameof Domain.PlayerAction.Buy) ->
        tryParseInt arg (fun value -> Buy value)
    | _ -> ParseFailed
