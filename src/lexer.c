#include <stdio.h>
#include <stdlib.h>
#include <ctype.h>
#include <string.h>
#include <stdbool.h>

#include "../lexer.h"
#include "../utils.h"

Token* tokens;	// single linked list of tokens
Token* lastTk;	// the last token in list

int line = 1;		// the current line in the input file

// adds a token to the end of the tokens list and returns it
// sets its code and line
Token* addTk(int code) {
	Token* tk = safeAlloc(sizeof(Token));
	tk->code = code;
	tk->line = line;
	tk->next = NULL;
	if (lastTk) {
		lastTk->next = tk;
	}
	else {
		tokens = tk;
	}
	lastTk = tk;
	return tk;
}

char* extract(const char* begin, const char* end) {
	if (begin == NULL || end == NULL || end <= begin) {
		return NULL;
	}
	const size_t length = (size_t)(end - begin);
	char* text = safeAlloc(length + 1);
	memcpy(text, begin, length);
	text[length] = '\0';
	return text;
}

Token* tokenize(const char* pch) {
	const char* start;
	Token* tk;
	for (;;) {
		switch (*pch) {
		case ' ': case '\t': pch++; break;
		case '\r':		// handles different kinds of newlines
			//(Windows: \r\n, Linux: \n, MacOS, OS X: \r or \n)
			if (pch[1] == '\n') pch++;
			// fallthrough to \n
		case '\n':
			line++;
			pch++;
			break;
		case '/':
			if (pch[1] == '/') {
				while (*pch != '\n' && *pch != '\0' && *pch != '\r') {
					pch++;
				}
			}
			else {
				addTk(DIV);
			}
			pch++;
			break;
		case '\0': addTk(END); return tokens;

		case ',': addTk(COMMA);		pch++; break;
		case ';': addTk(SEMICOLON); pch++; break;
		case '(': addTk(LPAR);		pch++; break;
		case ')': addTk(RPAR);		pch++; break;
		case '[': addTk(LBRACKET);	pch++; break;
		case ']': addTk(RBRACKET);	pch++; break;
		case '{': addTk(LACC);		pch++; break;
		case '}': addTk(RACC);		pch++; break;

		case '+': addTk(ADD); pch++; break;
		case '-': addTk(SUB); pch++; break;
		case '*': addTk(MUL); pch++; break;
		case '.': addTk(DOT); pch++; break;
		case '&':
			if (pch[1] == '&') {
				addTk(AND);
				pch += 2;
				break;
			}
		case '|':
			if (pch[1] == '|') {
				addTk(OR);
				pch += 2;
				break;
			}
		case '!':
			if (pch[1] == '=') {
				addTk(NOTEQ);
				pch += 2;
			}
			else {
				addTk(NOT);
				pch++;
			}
			break;
		case '=':
			if (pch[1] == '=') {
				addTk(EQUAL);
				pch += 2;
			}
			else {
				addTk(ASSIGN);
				pch++;
			}
			break;
		case '<':
			if (pch[1] == '=') {
				addTk(LESSEQ);
				pch += 2;
			}
			else {
				addTk(LESS);
				pch++;
			}
			break;
		case '>':
			if (pch[1] == '=') {
				addTk(GREATEREQ);
				pch += 2;
			}
			else {
				addTk(GREATER);
				pch++;
			}
			break;

		default:
			if (isalpha(*pch) || *pch == '_') {	// ID or KeyWord

				for (start = pch++; isalnum(*pch) || *pch == '_'; pch++) {}

				char* text = extract(start, pch);

				if (strcmp(text, "char") == 0)		addTk(TYPE_CHAR);
				else if (strcmp(text, "double") == 0)	addTk(TYPE_DOUBLE);
				else if (strcmp(text, "else") == 0)		addTk(ELSE);
				else if (strcmp(text, "if") == 0)		addTk(IF);
				else if (strcmp(text, "int") == 0)		addTk(TYPE_INT);
				else if (strcmp(text, "return") == 0)	addTk(RETURN);
				else if (strcmp(text, "struct") == 0)	addTk(STRUCT);
				else if (strcmp(text, "void") == 0)		addTk(VOID);
				else if (strcmp(text, "while") == 0)	addTk(WHILE);
				else {
					tk = addTk(ID);
					tk->text = text;
				}
			}
			else if (isdigit(*pch)) {	// INT or DOUBLE
				for (start = pch++; isdigit(*pch); pch++) {}
				if (*pch == '.') {
					pch++;
					for (; isdigit(*pch); pch++) {}
					if (*pch == 'e' || *pch == 'E') {
						pch++;
						if (*pch == '+' || *pch == '-') {
							pch++;
						}
						for (; isdigit(*pch); pch++) {}
					}
					char* text = extract(start, pch);
					tk = addTk(DOUBLE);
					tk->d = strtod(text, NULL);
					free(text);
				}
				else if (*pch == 'e' || *pch == 'E') {
					pch++;
					if (*pch == '+' || *pch == '-') {
						pch++;
					}
					for (; isdigit(*pch); pch++) {}
					char* text = extract(start, pch);
					tk = addTk(DOUBLE);
					tk->d = strtod(text, NULL);
					free(text);
				}
				else {
					char* text = extract(start, pch);
					tk = addTk(INT);
					tk->i = atoi(text);
					free(text);
				}
			}
			else if (*pch == '\'') {	// CHAR
				if (pch[2] == '\'') {
					tk = addTk(CHAR);
					tk->c = pch[1];
					pch += 3;
				}
				else {
					freeTokenList(tokens);
					err("did not close ' at line (%d)", line);
				}
			}
			else if (*pch == '\"') {	// STRING
				pch++;
				if (*pch == '\"') {
					char* text = safeAlloc(2);
					text[0] = '\0';
					tk = addTk(STRING);
					tk->text = text;
					pch++;
					break;
				}
				for (start = pch; (*pch != '\"') && (*pch != '\0') && (*pch != '\n'); pch++) {}
				if (*pch == '\"') {
					char* text = extract(start, pch);
					tk = addTk(STRING);
					tk->text = text;
					pch++;
				}
				else {
					freeTokenList(tokens);
					err("did not close \" at line (%d)", line);
				}
			}
			else {						// ERROR
				freeTokenList(tokens);
				err("invalid char: (%c) at line (%d)", *pch, line);
			}
		}
	}
}

void showTokens(const Token* tokens) {
	char* lista[] = {
		"ID"
		// constants
		, "INT", "DOUBLE", "CHAR", "STRING"
		// keywords
		, "TYPE_CHAR", "TYPE_DOUBLE", "ELSE", "IF", "TYPE_INT", "RETURN", "STRUCT", "VOID", "WHILE"
		// delimiters
		, "COMMA", "SEMICOLON", "LPAR", "RPAR", "LBRACKET", "RBRACKET", "LACC", "RACC", "END"
		// operators
		, "ADD", "SUB", "MUL", "DIV", "DOT", "AND", "OR", "NOT", "ASSIGN", "EQUAL", "NOTEQ", "LESS", "LESSEQ", "GREATER", "GREATEREQ"
	};

	for (const Token* tk = tokens; tk; tk = tk->next) {
		if (tk->code == ID || tk->code == STRING)
			printf("%d\t%s:%s\n", tk->line, lista[tk->code], tk->text);
		else if (tk->code == INT)		printf("%d\t%s:%d\n", tk->line, lista[tk->code], tk->i);
		else if (tk->code == DOUBLE)	printf("%d\t%s:%g\n", tk->line, lista[tk->code], tk->d);
		else if (tk->code == CHAR)		printf("%d\t%s:%c\n", tk->line, lista[tk->code], tk->c);
		else							printf("%d\t%s\n", tk->line, lista[tk->code]);
	}
}

void freeTokenList(Token* head) {
	Token* tmp;
	while (head != NULL) {
		tmp = head;
		head = head->next;
		if (tmp->code == 0 || tmp->code == 4) {
			free(tmp->text);
		}
		free(tmp);
	}
}
