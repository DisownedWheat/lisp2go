package main

import (
	"disownedwheat/lisp2go/lexer"
	"fmt"
)

func main() {
	input := `
  (λ [] (add 1 (subtract 2)))
  `
	lex := new(lexer.Lexer)
	lex.Init(input)
	fmt.Println(lex.Lex())
}
