module Lexer

type Token =
    { Line: int
      Column: int
      Value: string }

type TokenType =
    | Import of Token
    | Identifier of Token
    | Define of Token
    | Lambda of Token
    | Number of Token
    | String of Token
    | LParen of Token
    | RParen of Token
    | LBrace of Token
    | RBrace of Token
    | LBracket of Token
    | RBracket of Token
    | Colon of Token
    | DoubleColon of Token
    | Quote of Token
    | Comma of Token
    | Hash of Token
    | EOF of Token
    | Error of Token

type LexerState =
    { LineState: int
      ColumnState: int
      Tokens: TokenType list
      Current: char list }

exception LexerException of (int * int * string)

let parseCurrent acc =
    match List.filter (System.Char.IsWhiteSpace >> not) acc.Current with
    | [] -> acc
    | _ ->
        let value = new string (List.rev acc.Current |> List.toArray)

        let t =
            if List.forall System.Char.IsDigit acc.Current then
                Number
            else
                match value with
                | "import" -> Import
                | "define" -> Define
                | "lambda" -> Lambda
                | _ -> Identifier

        { acc with
            Tokens =
                t (
                    { Line = acc.LineState
                      Column = acc.ColumnState
                      Value = new string (List.rev acc.Current |> List.toArray) }
                )
                :: acc.Tokens
            Current = [] }

let checkWhitespace acc a checkCurrent =
    let parseFunc = if checkCurrent then parseCurrent else id

    match a with
    | '\n' ->
        (true,
         { parseFunc acc with
             LineState = acc.LineState + 1
             ColumnState = 1 })
    | '\r' -> (true, parseFunc acc)
    | ' ' ->
        (true,
         { parseFunc acc with
             ColumnState = acc.ColumnState + 1 })
    | '\t' ->
        (true,
         { parseFunc acc with
             ColumnState = acc.ColumnState + 4 })
    | _ -> (false, acc)

let matchChar acc a =
    checkWhitespace acc a true
    |> fun (isWhitespace, acc) ->
        if isWhitespace then
            acc
        else
            match a with
            | '(' ->
                let newAcc = parseCurrent acc

                { newAcc with
                    Tokens =
                        LParen(
                            { Line = newAcc.LineState
                              Column = newAcc.ColumnState
                              Value = "(" }
                        )
                        :: newAcc.Tokens
                    ColumnState = newAcc.ColumnState + 1 }
            | ')' ->
                let newAcc = parseCurrent acc

                { newAcc with
                    Tokens =
                        RParen(
                            { Line = newAcc.LineState
                              Column = newAcc.ColumnState
                              Value = ")" }
                        )
                        :: newAcc.Tokens
                    ColumnState = newAcc.ColumnState + 1 }
            | '{' ->
                let newAcc = parseCurrent acc

                { newAcc with
                    Tokens =
                        LBrace(
                            { Line = newAcc.LineState
                              Column = newAcc.ColumnState
                              Value = "{" }
                        )
                        :: newAcc.Tokens
                    ColumnState = newAcc.ColumnState + 1 }
            | '}' ->
                let newAcc = parseCurrent acc

                { newAcc with
                    Tokens =
                        RBrace(
                            { Line = newAcc.LineState
                              Column = newAcc.ColumnState
                              Value = "}" }
                        )
                        :: newAcc.Tokens
                    ColumnState = newAcc.ColumnState + 1 }
            | '[' ->
                let newAcc = parseCurrent acc

                { newAcc with
                    Tokens =
                        LBracket(
                            { Line = newAcc.LineState
                              Column = newAcc.ColumnState
                              Value = "[" }
                        )
                        :: newAcc.Tokens
                    ColumnState = newAcc.ColumnState + 1 }
            | ']' ->
                let newAcc = parseCurrent acc

                { newAcc with
                    Tokens =
                        RBracket(
                            { Line = newAcc.LineState
                              Column = newAcc.ColumnState
                              Value = "]" }
                        )
                        :: newAcc.Tokens
                    ColumnState = newAcc.ColumnState + 1 }
            | ',' ->
                let newAcc = parseCurrent acc

                { newAcc with
                    Tokens =
                        Comma(
                            { Line = newAcc.LineState
                              Column = newAcc.ColumnState
                              Value = "," }
                        )
                        :: newAcc.Tokens
                    ColumnState = newAcc.ColumnState + 1 }
            | '#' ->
                let newAcc = parseCurrent acc

                { newAcc with
                    Tokens =
                        Hash(
                            { Line = newAcc.LineState
                              Column = newAcc.ColumnState
                              Value = "#" }
                        )
                        :: newAcc.Tokens
                    ColumnState = newAcc.ColumnState + 1 }
            | '\'' ->
                let newAcc = parseCurrent acc

                { newAcc with
                    Tokens =
                        Quote(
                            { Line = newAcc.LineState
                              Column = newAcc.ColumnState
                              Value = "'" }
                        )
                        :: newAcc.Tokens
                    ColumnState = newAcc.ColumnState + 1 }
            | _ ->
                { acc with
                    Current = a :: acc.Current
                    ColumnState = acc.ColumnState + 1 }

let parseText (acc: LexerState) (a: char) =
    // First check the current character and then match it to the appropriate token
    match acc.Current with
    | '"' :: _ ->
        match a with
        | '"' ->
            { acc with
                Tokens =
                    String
                        { Line = acc.LineState
                          Column = acc.ColumnState
                          Value = new string (List.tail acc.Current |> List.toArray) }
                    :: acc.Tokens
                ColumnState = acc.ColumnState + 1
                Current = [] }
        | _ ->
            let _, newAcc = checkWhitespace acc a false

            { newAcc with
                Current = List.append newAcc.Current [ a ] }
    | ':' :: _ ->
        match a with
        | ':' ->
            { acc with
                Tokens =
                    DoubleColon
                        { Line = acc.LineState
                          Column = acc.ColumnState
                          Value = "::" }
                    :: acc.Tokens
                ColumnState = acc.ColumnState + 2
                Current = [] }
        | _ ->
            let newAcc =
                { acc with
                    Tokens =
                        Colon
                            { Line = acc.LineState
                              Column = acc.ColumnState
                              Value = ":" }
                        :: acc.Tokens
                    ColumnState = acc.ColumnState + 1
                    Current = [] }

            matchChar newAcc a
    | _ -> matchChar acc a


let lex (filePath: string) =
    let fileText = System.IO.File.ReadAllText(filePath)

    let state =
        { LineState = 1
          ColumnState = 1
          Tokens = []
          Current = []

        }

    List.fold parseText state (List.ofSeq fileText)
    |> fun x -> EOF { Line = 0; Column = 0; Value = "EOF" } :: x.Tokens
    |> List.rev
