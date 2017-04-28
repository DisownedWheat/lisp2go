package parser

import (
	"disownedwheat/lisp2go/lexer"
	"fmt"
	"strconv"
)

type NodeType string

type Node struct {
	Type  NodeType
	Body  []Node
	Value interface{}
}

type Parser struct {
	tokens []lexer.Token
	index  int
}

func (p *Parser) Init(toks []lexer.Token) {
	p.tokens = toks
	p.index = 0
}

func (p *Parser) Parse() Node {

	// Define the recursive walk function for parsing into an AST
	var walk func() Node
	walk = func() Node {
		currTok := p.currToken()
		var node Node
		switch currTok.Type {
		case "NUMBER":
			newNum, err := strconv.Atoi(currTok.Value)
			p.checkErr(err, fmt.Sprintf("Could not cast string to int: %s", currTok.Value))
			node = Node{
				Type:  "NumberLiteral",
				Body:  []Node{},
				Value: newNum,
			}
		case "STRING":
			node = Node{
				Type:  "StringLiteral",
				Body:  []Node{},
				Value: currTok.Value,
			}
		case "LPAREN":
			val := p.nextToken()
			body := []Node{}
			for p.currToken().Type != "RPAREN" {
				body = append(body, walk())
			}
			node = Node{
				Type:  "CallExpression",
				Value: val,
				Body:  body,
			}
		default:
			node = Node{
				Type:  "Ident",
				Body:  nil,
				Value: currTok.Value,
			}
		}
		p.index++
		return node
	}

	var newBody []Node

	for p.index < len(p.tokens) {
		newBody = append(newBody, walk())
	}

	// Prepare the root AST node
	return Node{
		Type:  "Program",
		Body:  newBody,
		Value: nil,
	}
}

func (p *Parser) peek(n int) lexer.Token  { return p.tokens[p.index+n] }
func (p *Parser) check(n int) lexer.Token { return p.tokens[p.index-n] }

func (p *Parser) nextToken() lexer.Token {
	p.index++
	fmt.Println(p.index)
	return p.tokens[p.index]
}
func (p *Parser) currToken() lexer.Token { return p.tokens[p.index] }

func (p *Parser) checkErr(err error, msg string) {
	if err != nil {
		panic(fmt.Sprintf("ERROR: Parser could not parse input: %s\n\n%s", msg, err.Error()))
	}
}
