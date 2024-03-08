module Parser

type AST =
    | Root
    | Import
    | Identifier
    | Number
    | String
    | Define
    | Function
    | Call
    | Lambda
    | Type
    | Atom
    | List
    | Map
    | Vector
    | VectorLiteral
    | Macro

type ASTNode =
    { Type: AST
      Value: string
      Children: ASTNode list
      Line: int
      Column: int }

exception ParseException of (Lexer.Token * int * int * string)

let raiseParseError msg (token: Lexer.Token) ast =
    raise (
        ParseException(
            token,
            ast.Line,
            ast.Column,
            sprintf "Parse error: %s at line %d column %d" msg ast.Line ast.Column
        )
    )

let checkClosing token rest ast =
    match token with
    | Lexer.RParen x ->
        match ast.Type with
        | Call
        | List
        | Macro
        | Define
        | Lambda -> (rest, ast)
        | _ -> raiseParseError "Unexpected closing parenthesis" x ast
    | Lexer.RBrace x ->
        match ast.Type with
        | Map -> (rest, ast)
        | _ -> raiseParseError "Unexpected closing brace" x ast
    | Lexer.RBracket x ->
        match ast.Type with
        | VectorLiteral -> (rest, ast)
        | _ -> raiseParseError "Unexpected closing bracket" x ast
    | _ -> (rest, ast)

let rec parseTree tokens (ast: ASTNode) =
    match tokens with
    | [] -> ([], ast)
    | head :: rest ->
        match head with
        | Lexer.Import x ->
            let t, a =
                parseTree
                    rest
                    { Type = Import
                      Value = ""
                      Children = []
                      Line = x.Line
                      Column = x.Column }

            parseTree
                t
                { ast with
                    Children = a :: ast.Children }
        | Lexer.Identifier x ->
            parseTree
                rest
                { ast with
                    Children =
                        { Type = Identifier
                          Value = x.Value
                          Children = []
                          Line = x.Line
                          Column = x.Column }
                        :: ast.Children }
        | Lexer.Number x ->
            parseTree
                rest
                { ast with
                    Children =
                        { Type = Number
                          Value = x.Value
                          Children = []
                          Line = x.Line
                          Column = x.Column }
                        :: ast.Children }
        | Lexer.String x ->
            parseTree
                rest
                { ast with
                    Children =
                        { Type = String
                          Value = x.Value
                          Children = []
                          Line = x.Line
                          Column = x.Column }
                        :: ast.Children }
        | Lexer.Define x ->
            let t, a =
                parseTree
                    rest
                    { Type = Define
                      Value = x.Value
                      Children = []
                      Line = x.Line
                      Column = x.Column }

            parseTree
                t
                { ast with
                    Children = a :: ast.Children }
        | Lexer.Lambda x ->
            let t, a =
                parseTree
                    rest
                    { Type = Lambda
                      Value = x.Value
                      Children = []
                      Line = x.Line
                      Column = x.Column }

            parseTree
                t
                { ast with
                    Children = a :: ast.Children }
        | Lexer.LParen x ->
            match ast.Type with
            | List
            | Macro
            | Vector -> parseTree rest ast
            | _ ->
                match rest with
                | [] -> raiseParseError "Unexpected end of file" x ast
                | nextToken :: restTokens ->
                    match nextToken with
                    | Lexer.Identifier x ->
                        let t, a =
                            parseTree
                                rest
                                { Type = Call
                                  Value = ""
                                  Children = []
                                  Line = x.Line
                                  Column = x.Column }

                        parseTree
                            t
                            { ast with
                                Children = a :: ast.Children }
                    | Lexer.Define x ->
                        let t, a =
                            parseTree
                                restTokens
                                { Type = Define
                                  Value = ""
                                  Children = []
                                  Line = x.Line
                                  Column = x.Column }

                        parseTree
                            t
                            { ast with
                                Children = a :: ast.Children }
                    | Lexer.Lambda x ->
                        let t, a =
                            parseTree
                                rest
                                { Type = Lambda
                                  Value = ""
                                  Children = []
                                  Line = x.Line
                                  Column = x.Column }

                        parseTree
                            t
                            { ast with
                                Children = a :: ast.Children }
                    | _ -> raiseParseError "Expected identifier for function call" x ast
        | Lexer.RParen _ -> checkClosing head rest ast
        | Lexer.LBrace x ->
            let t, a =
                parseTree
                    rest
                    { Type = Map
                      Value = ""
                      Children = []
                      Line = x.Line
                      Column = x.Column }

            parseTree
                t
                { ast with
                    Children = a :: ast.Children }
        | Lexer.RBrace _ -> checkClosing head rest ast
        | Lexer.LBracket x ->
            let t, a =
                parseTree
                    rest
                    { Type = VectorLiteral
                      Value = ""
                      Children = []
                      Line = x.Line
                      Column = x.Column }

            parseTree
                t
                { ast with
                    Children = a :: ast.Children }
        | Lexer.RBracket _ -> checkClosing head rest ast
        | Lexer.Colon x ->
            match rest with
            | [] -> raiseParseError "Unexpected end of file" x ast
            | nextToken :: rest ->
                match nextToken with
                | Lexer.Identifier x ->
                    parseTree
                        rest
                        { ast with
                            Children =
                                { Type = Atom
                                  Value = x.Value
                                  Children = []
                                  Line = x.Line
                                  Column = x.Column }
                                :: ast.Children }
                | _ -> raiseParseError "Expected identifier after colon" x ast
        | Lexer.DoubleColon x ->
            match rest with
            | [] -> raiseParseError "Unexpected end of file" x ast
            | nextToken :: rest ->
                match nextToken with
                | Lexer.Identifier x ->
                    parseTree
                        rest
                        { ast with
                            Children =
                                { Type = Type
                                  Value = x.Value
                                  Children = []
                                  Line = x.Line
                                  Column = x.Column }
                                :: ast.Children }
                | _ -> raiseParseError "Expected identifier after double colon" x ast
        | Lexer.Quote x ->
            let t, a =
                parseTree
                    rest
                    { Type = List
                      Value = ""
                      Children = []
                      Line = x.Line
                      Column = x.Column }

            parseTree
                t
                { ast with
                    Children = a :: ast.Children }
        | Lexer.Comma x ->
            let t, a =
                parseTree
                    rest
                    { Type = Macro
                      Value = ""
                      Children = []
                      Line = x.Line
                      Column = x.Column }

            parseTree
                t
                { ast with
                    Children = a :: ast.Children }
        | Lexer.Hash x ->
            let t, a =
                parseTree
                    rest
                    { Type = Vector
                      Value = ""
                      Children = []
                      Line = x.Line
                      Column = x.Column }

            parseTree
                t
                { ast with
                    Children = a :: ast.Children }
        | Lexer.EOF x ->
            match ast.Type with
            | Root -> ([], ast)
            | _ -> raiseParseError "Unexpected end of file" x ast
        | Lexer.Error x -> (rest, ast)

let rec reverseChildren ast =
    { ast with
        Children = List.rev ast.Children |> List.map reverseChildren }

let parse tokens =
    let _, results =
        parseTree
            tokens
            { Type = Root
              Value = ""
              Children = []
              Line = 0
              Column = 0 }

    reverseChildren results
