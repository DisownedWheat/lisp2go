module Transformer

type RootNode = { Children: TransformerNode list }

and ImportNode = { Paths: string list }

and IdentifierNode = { Value: string }

and NumberNode = { Value: string }

and StringNode = { Value: string }

and AtomNode = { Value: string }

and TypeNode = { Value: string }

and CallNode =
    { Name: string
      Arguments: TransformerNode list
      Body: TransformerNode list }

and DefineNode =
    { Name: string; Value: TransformerNode }

and ListNode = { Items: TransformerNode list }

and VectorNode = { Items: TransformerNode list }

and MapNode =
    { Items: (TransformerNode * TransformerNode) list }

and CurryNode =
    { Name: string
      Arguments: TransformerNode list
      Body: TransformerNode list }

and TransformerNode =
    | RootNode of RootNode
    | ImportNode of ImportNode
    | Identifier of IdentifierNode
    | Number of NumberNode
    | String of StringNode
    | Atom of AtomNode
    | Type of TypeNode
    | Call of CallNode
    | Define of DefineNode
    | ListNode of ListNode
    | VectorNode of VectorNode
    | MapNode of MapNode
    | CurryNode of CurryNode

type DefinitionsState =
    { Functions: (string * TransformerNode) list
      Macros: (string * TransformerNode) list
      Types: (string * TransformerNode) list }

exception TransformerException of (int * int * string)

let rec transform (ast: Parser.ASTNode) = 0
// match ast.Type with
// | Parser.Root -> RootNode { Children = List.map transform ast.Children }
// | Parser.Import -> ImportNode { Paths = List.map (fun x -> x.Value) ast.Children }
