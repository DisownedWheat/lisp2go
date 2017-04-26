package lexer

import (
	"fmt"
	"strings"
	"unicode"
)

var ReservedTokens = map[rune]Token{
	'(': Token{
		Type:  "LPAREN",
		Value: "(",
	},
	')': Token{
		Type:  "RPAREN",
		Value: ")",
	},
	'[': Token{
		Type:  "LPAREN",
		Value: "(",
	},
	']': Token{
		Type:  "RPAREN",
		Value: ")",
	},
	'λ': Token{
		Type:  "LAMBDA",
		Value: "λ",
	},
}

var KeyWords = map[string]TokenType{
	"defn": "FUNCDEF",
	"let":  "LET",
}

type TokenType string

type Token struct {
	Type  TokenType
	Value string
}

func (l *Lexer) Init(input string) {
	newInput := []rune(input)
	// First set the input
	l.input = newInput
	l.index = 0
	l.tokens = []Token{}
}

func (l *Lexer) nextRune() rune {
	l.index++
	return l.input[l.index]
}
func (l *Lexer) prevRune() rune {
	l.index--
	return l.input[l.index]
}

func (l *Lexer) toDelim(Type TokenType, val []string, delim func(rune) bool) {

	// Get next run in index
	currRune := l.nextRune()

	// Loop over until the passed check is true
	for delim(currRune) {
		val = append(val, string(currRune))
		currRune = l.nextRune()
	}

	// Join all the values together as a string
	totalVal := strings.Join(val, "")

	// If this is an identifier check the keywords first
	if Type == "IDENT" {
		if keyWord, ok := KeyWords[totalVal]; ok {
			Type = keyWord
		}
	}
	// Create the token now
	tok := Token{
		Type:  Type,
		Value: totalVal,
	}

	// Add token to lexer
	l.tokens = append(l.tokens, tok)
}

func (l *Lexer) Lex() []Token {
	for l.index < len(l.input) {
		currRune := l.input[l.index]
		if unicode.IsSpace(currRune) {
			l.index++
			continue
		}

		fmt.Println(string(currRune))

		// If this is one of the reserved tokens simply add the token and skip
		if tok, ok := ReservedTokens[currRune]; ok {
			l.tokens = append(l.tokens, tok)
			l.index++
			continue
		}

		// Check for strings
		if currRune == '"' {
			var val []string
			check := func(r rune) bool { return r != '"' }
			l.toDelim("STRING", val, check)
			continue
		}

		// Check for numbers
		if unicode.IsDigit(currRune) {
			var val []string
			currRune = l.prevRune()
			check := func(r rune) bool {
				return !unicode.IsSpace(r) && !unicode.IsLetter(r) && !(r == ')')
			}
			l.toDelim("NUMBER", val, check)
			continue
		}

		// Else we're just looking at identifiers
		val := []string{string(currRune)}
		check := func(r rune) bool { return !unicode.IsSpace(r) && !(r == ')') }
		l.toDelim("IDENT", val, check)
	}
	return l.tokens
}

func (l *Lexer) peek(n int) rune  { return l.input[l.index+n] }
func (l *Lexer) check(n int) rune { return l.input[l.index-n] }
