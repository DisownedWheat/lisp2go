module main

open Newtonsoft.Json
let pipeLine = Lexer.lex >> Parser.parse //>> Transformer.transform

let rec compile filePath =
    try
        pipeLine filePath |> printfn "%A"
    with
    | Lexer.LexerException(line, column, message) -> printfn "Lexer error: %s at line %d, column %d" message line column
    | Parser.ParseException(token, line, column, message) ->
        printfn
            "Parser error: %s \n\nat line %d, column %d\n\n Token: %s"
            message
            line
            column
            (JsonConvert.SerializeObject(token, Formatting.Indented))
    | Transformer.TransformerException(line, column, message) ->
        printfn "Transformer error: %s at line %d, column %d" message line column

[<EntryPoint>]
let main argv =

    List.filter (fun (x: string) -> x.EndsWith(".scm")) (List.ofArray argv)
    |> List.head
    |> compile

    0 // return an integer exit code
