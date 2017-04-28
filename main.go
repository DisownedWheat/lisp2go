package main

import (
	"disownedwheat/lisp2go/lexer"
	"disownedwheat/lisp2go/parser"
	"fmt"
)

func main() {
	input := `
  (λ () (add 1 (subtract 2)))
  `
	lex := new(lexer.Lexer)
	lex.Init(input)
	parse := new(parser.Parser)
	parse.Init(lex.Lex())
	res := parse.Parse()
	fmt.Println(res.Body)
}
