#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
#include <stdbool.h>

#include "../utils.h"
#include "../parser.h"
#include "../ad.h"
#include "../at.h"
#include "../vm.h"
#include "../gc.h"

Token* list;
Token* iTk;		// the iterator in the tokens list
Token* consumedTk;		// the last consumed token

Symbol* owner;
Domain* symTable;

void tkerr(const char* fmt, ...) {
	fprintf(stderr, "error in line %d: ", iTk->line);
	va_list va;
	va_start(va, fmt);
	vfprintf(stderr, fmt, va);
	va_end(va);
	fprintf(stderr, "\n");
	freeTokenList(list);
	exit(EXIT_FAILURE);
}

bool consume(int code) {
	if (iTk->code == code) {
		consumedTk = iTk;
		iTk = iTk->next;
		return true;
	}
	return false;
}

// typeBase: TYPE_INT | TYPE_DOUBLE | TYPE_CHAR | STRUCT ID
bool typeBase(Type* t) {
	if (consume(TYPE_INT)) {
		t->tb = TB_INT;
		t->n = -1;
		return true;
	}
	if (consume(TYPE_DOUBLE)) {
		t->tb = TB_DOUBLE;
		t->n = -1;
		return true;
	}
	if (consume(TYPE_CHAR)) {
		t->tb = TB_CHAR;
		t->n = -1;
		return true;
	}
	if (consume(STRUCT)) {
		if (consume(ID)) {
			Token* tkName = consumedTk; // ID[tkName]
			t->tb = TB_STRUCT;
			t->s = findSymbol(tkName->text);
			if (!t->s) tkerr("structura nedefinita: %s", tkName->text);
			t->n = -1;
			return true;
		}
		else tkerr("missing 'identifier' after 'struct'");
	}
	return false;
}

// arrayDecl: LBRACKET INT? RBRACKET
bool arrayDecl(Type* t) {
	if (consume(LBRACKET)) {
		if (consume(INT)) {
			Token* tkSize = consumedTk;
			t->n = tkSize->i;
		}
		else {
			t->n = 0; // array fara dimensiune: int v[]
		}
		if (consume(RBRACKET)) {
			return true;
		}
		else tkerr("missing ']'");
	}
	return false;
}

// varDef: typeBase ID arrayDecl? SEMICOLON
bool varDef() {
	Type t;
	Token* start = iTk;
	Instr* startInstr = owner ? lastInstr(owner->fn.instr) : NULL;
	if (typeBase(&t)) {
		if (consume(ID)) {
			Token* tkName = consumedTk; // ID[tkName]
			if (arrayDecl(&t)) {
				if (t.n == 0) tkerr("a vector variable must have a specified dimension");
			}
			if (consume(SEMICOLON)) {
				Symbol* var = findSymbolInDomain(symTable, tkName->text);
				if (var) tkerr("symbol redefinition: %s", tkName->text);
				var = newSymbol(tkName->text, SK_VAR);
				var->type = t;
				var->owner = owner;
				addSymbolToDomain(symTable, var);
				if (owner) {
					switch (owner->kind) {
					case SK_FN:
						var->varIdx = symbolsLen(owner->fn.locals);
						addSymbolToList(&owner->fn.locals, dupSymbol(var));
						break;
					case SK_STRUCT:
						var->varIdx = typeSize(&owner->type);
						addSymbolToList(&owner->structMembers, dupSymbol(var));
						break;
					}
				}
				else {
					var->varMem = safeAlloc(typeSize(&t));
				}
				return true;
			}
		}
	}
	iTk = start;
	if (owner) delInstrAfter(startInstr);
	return false;
}

// structDef: STRUCT ID LACC varDef* RACC SEMICOLON
bool structDef() {
	Token* start = iTk;
	Instr* startInstr = owner ? lastInstr(owner->fn.instr) : NULL;
	if (consume(STRUCT)) {
		if (consume(ID)) {
			Token* tkName = consumedTk;	// ID[tkName]
			if (consume(LACC)) {
				Symbol* s = findSymbolInDomain(symTable, tkName->text);
				if (s) tkerr("symbol redefinition: %s", tkName->text);
				s = addSymbolToDomain(symTable, newSymbol(tkName->text, SK_STRUCT));
				s->type.tb = TB_STRUCT;
				s->type.s = s;
				s->type.n = -1;
				pushDomain();
				owner = s;
				while (varDef()) {}
				if (consume(RACC)) {
					if (consume(SEMICOLON)) {
						owner = NULL;
						dropDomain();
						return true;
					}
					else tkerr("missing ';' after '}' of the struct");
				}
				else tkerr("missing '}'");
			}
		}
		else tkerr("missing 'identifier' after 'struct'");
	}
	iTk = start;
	if (owner) delInstrAfter(startInstr);
	return false;
}

// fnParam: typeBase ID arrayDecl?
bool fnParam() {
	Type t;
	Token* start = iTk;
	Instr* startInstr = owner ? lastInstr(owner->fn.instr) : NULL;
	if (typeBase(&t)) {
		if (consume(ID)) {
			Token* tkName = consumedTk;	// ID[tkName]
			if (arrayDecl(&t)) {
				t.n = 0;
			}
			Symbol* param = findSymbolInDomain(symTable, tkName->text);
			if (param) tkerr("symbol redefinition: %s", tkName->text);
			param = newSymbol(tkName->text, SK_PARAM);
			param->type = t;
			param->owner = owner;
			param->paramIdx = symbolsLen(owner->fn.params);
			// parametrul este adaugat atat la domeniul curent, cat si la parametrii fn
			addSymbolToDomain(symTable, param);
			addSymbolToList(&owner->fn.params, dupSymbol(param));
			return true;
		}
		else tkerr("missing param name");
	}
	iTk = start;
	if (owner) delInstrAfter(startInstr);
	return false;
}

bool expr(Ret* r);

// exprPrimary: ID ( LPAR (expr (COMMA expr )* )? RPAR )? | INT | DOUBLE | CHAR | STRING | LPAR expr RPAR
bool exprPrimary(Ret* r) {
	if (consume(ID)) {
		Token* tkName = consumedTk;	// ID[tkName]
		Symbol* s = findSymbol(tkName->text);
		if (!s) tkerr("undefined id: %s", tkName->text);
		if (consume(LPAR)) {
			if (s->kind != SK_FN) tkerr("only a function can be called");
			Ret rArg;
			Symbol* param = s->fn.params;
			if (expr(&rArg)) {
				if (!param) tkerr("too many arguments in function call");
				if (!convTo(&rArg.type, &param->type)) tkerr("in call, cannot convert the argument type to the parameter type");
				addRVal(&owner->fn.instr, rArg.lval, &rArg.type);
				insertConvIfNeeded(lastInstr(owner->fn.instr), &rArg.type, &param->type);
				param = param->next;
				while (consume(COMMA)) {
					if (!expr(&rArg)) tkerr("invalid expression after ','");

					if (!param) tkerr("too many arguments in function call");
					if (!convTo(&rArg.type, &param->type)) tkerr("in call, cannot convert the argument type to the parameter type");
					addRVal(&owner->fn.instr, rArg.lval, &rArg.type);
					insertConvIfNeeded(lastInstr(owner->fn.instr), &rArg.type, &param->type);
					param = param->next;
				}
			}
			if (consume(RPAR)) {
				if (param) tkerr("too few arguments in function call");
				*r = (Ret){ s->type,false,true };
				if (s->fn.extFnPtr) {
					addInstr(&owner->fn.instr, OP_CALL_EXT)->arg.extFnPtr = s->fn.extFnPtr;
				}
				else {
					addInstr(&owner->fn.instr, OP_CALL)->arg.instr = s->fn.instr;
				}
				return true;
			}
			else tkerr("missing ')'");
		}
		else {
			if (s->kind == SK_FN) tkerr("a function can only be called");
			*r = (Ret){ s->type,true,s->type.n >= 0 };
			if (s->kind == SK_VAR) {
				if (s->owner == NULL) { // global variables
					addInstr(&owner->fn.instr, OP_ADDR)->arg.p = s->varMem;
				}
				else { // local variables
					switch (s->type.tb) {
					case TB_INT:	addInstrWithInt(&owner->fn.instr, OP_FPADDR_I, s->varIdx + 1); break;
					case TB_DOUBLE: addInstrWithInt(&owner->fn.instr, OP_FPADDR_F, s->varIdx + 1); break;
					}
				}
			}
			if (s->kind == SK_PARAM) {
				switch (s->type.tb) {
				case TB_INT:	addInstrWithInt(&owner->fn.instr, OP_FPADDR_I, s->paramIdx - symbolsLen(s->owner->fn.params) - 1); break;
				case TB_DOUBLE: addInstrWithInt(&owner->fn.instr, OP_FPADDR_F, s->paramIdx - symbolsLen(s->owner->fn.params) - 1); break;
				}
			}
		}
		return true;
	}
	if (consume(INT)) {
		Token* ct = consumedTk;
		addInstrWithInt(&owner->fn.instr, OP_PUSH_I, ct->i);
		*r = (Ret){ {TB_INT,NULL,-1},false,true };
		return true;
	}
	if (consume(DOUBLE)) {
		Token* ct = consumedTk;
		addInstrWithDouble(&owner->fn.instr, OP_PUSH_F, ct->d);
		*r = (Ret){ {TB_DOUBLE,NULL,-1},false,true };
		return true;
	}
	if (consume(CHAR)) {
		*r = (Ret){ {TB_CHAR,NULL,-1},false,true };
		return true;
	}
	if (consume(STRING)) {
		*r = (Ret){ {TB_CHAR,NULL,0},false,true };
		return true;
	}
	if (consume(LPAR)) {
		if (expr(r)) {
			if (consume(RPAR)) {
				return true;
			}
			else tkerr("missing ')'");
		}
		else tkerr("invalid expression inside the parenthesis");
	}
	return false;
}

// exprPostfix: exprPostfix LBRACKET expr RBRACKET | exprPostfix DOT ID | exprPrimary
// exprPostfix: exprPrimary exprPostfixPrim
// exprPostfixPrim : LBRACKET expr RBRACKET exprPostfixPrim | DOT ID exprPostfixPrim | e
bool exprPostfixPrim(Ret* r) {
	Token* start = iTk;
	Instr* startInstr = owner ? lastInstr(owner->fn.instr) : NULL;
	if (consume(LBRACKET)) {
		Ret idx;
		if (expr(&idx)) {
			if (consume(RBRACKET)) {
				if (r->type.n < 0) tkerr("only an array can be indexed");
				Type tInt = { TB_INT,NULL,-1 };
				if (!convTo(&idx.type, &tInt)) tkerr("the index is not convertible to int");
				r->type.n = -1;
				r->lval = true;
				r->ct = false;
				if (exprPostfixPrim(r)) {
					return true;
				}
			}
			else tkerr("missing ']'");
		}
		else tkerr("invalid expression inside the brackets");
	}
	iTk = start;
	if (owner) delInstrAfter(startInstr);
	if (consume(DOT)) {
		if (consume(ID)) {
			Token* tkName = consumedTk;	// ID[tkName]
			if (r->type.tb != TB_STRUCT) tkerr("a field can only be selected from a struct");
			Symbol* s = findSymbolInList(r->type.s->structMembers, tkName->text);
			if (!s) tkerr("the structure %s does not have a field % s", r->type.s->name, tkName->text);
			*r = (Ret){ s->type,true,s->type.n >= 0 };
			if (exprPostfixPrim(r)) {
				return true;
			}
		}
		else tkerr("invalid expression after '.'");
	}
	iTk = start;
	if (owner) delInstrAfter(startInstr);
	return true;
}

bool exprPostfix(Ret* r) {
	if (exprPrimary(r)) {
		return exprPostfixPrim(r);
	}
	return false;
}

// exprUnary: ( SUB | NOT ) exprUnary | exprPostfix
bool exprUnary(Ret* r) {
	if (consume(SUB)) {
		if (exprUnary(r)) {
			if (!canBeScalar(r)) tkerr("unary '-' must have a scalar operand");
			r->lval = false;
			r->ct = true;
			return true;
		}
		else tkerr("invalid expression after '-'");
	}
	if (consume(NOT)) {
		if (exprUnary(r)) {
			if (!canBeScalar(r)) tkerr("unary '!' must have a scalar operand");
			r->lval = false;
			r->ct = true;
			return true;
		}
		else tkerr("invalid expression after '!'");
	}
	return exprPostfix(r);
}

// exprCast: LPAR typeBase arrayDecl? RPAR exprCast | exprUnary
bool exprCast(Ret* r) {
	Token* start = iTk;
	Instr* startInstr = owner ? lastInstr(owner->fn.instr) : NULL;
	if (consume(LPAR)) {
		Ret op;
		Type t;
		if (typeBase(&t)) {
			arrayDecl(&t);
			if (consume(RPAR)) {
				if (exprCast(&op)) {
					if (t.tb == TB_STRUCT) tkerr("cannot convert to a struct type");
					if (op.type.tb == TB_STRUCT) tkerr("cannot convert a struct");
					if (op.type.n >= 0 && t.n < 0) tkerr("an array can be converted only to another array");
					if (op.type.n < 0 && t.n >= 0) tkerr("a scalar can be converted only to another scalar");
					*r = (Ret){ t,false,true };
					return true;
				}
			}
			else tkerr("missing ')'");
		}
	}
	iTk = start;
	if (owner) delInstrAfter(startInstr);
	return exprUnary(r);
}

// exprMul: exprMul ( MUL | DIV ) exprCast | exprCast
// exprMul: exprCast exprMulPrim
// exprMulPrim: ( MUL | DIV ) exprCast exprMulPrim | e
bool exprMulPrim(Ret* r) {
	Token* op;
	Token* start = iTk;
	Instr* startInstr = owner ? lastInstr(owner->fn.instr) : NULL;
	if (consume(MUL)) {
		op = iTk;
		Ret right;
		Instr* lastLeft = lastInstr(owner->fn.instr);
		addRVal(&owner->fn.instr, r->lval, &r->type);
		if (exprCast(&right)) {
			Type tDst;
			if (!arithTypeTo(&r->type, &right.type, &tDst)) tkerr("invalid operand type for '*'");
			addRVal(&owner->fn.instr, right.lval, &right.type);
			insertConvIfNeeded(lastLeft, &r->type, &tDst);
			insertConvIfNeeded(lastInstr(owner->fn.instr), &right.type, &tDst);
			switch (tDst.tb) {
			case TB_INT:	addInstr(&owner->fn.instr, OP_MUL_I); break;
			case TB_DOUBLE: addInstr(&owner->fn.instr, OP_MUL_F); break;
			}
			*r = (Ret){ tDst,false,true };
			if (exprMulPrim(r)) {
				return true;
			}
		}
		else tkerr("invalid expression after '*'");
	}
	iTk = start;
	if (owner) delInstrAfter(startInstr);
	if (consume(DIV)) {
		op = iTk;
		Ret right;
		Instr* lastLeft = lastInstr(owner->fn.instr);
		addRVal(&owner->fn.instr, r->lval, &r->type);
		if (exprCast(&right)) {
			Type tDst;
			if (!arithTypeTo(&r->type, &right.type, &tDst)) tkerr("invalid operand type for '/'");
			addRVal(&owner->fn.instr, right.lval, &right.type);
			insertConvIfNeeded(lastLeft, &r->type, &tDst);
			insertConvIfNeeded(lastInstr(owner->fn.instr), &right.type, &tDst);
			switch (tDst.tb) {
			case TB_INT:	addInstr(&owner->fn.instr, OP_DIV_I); break;
			case TB_DOUBLE: addInstr(&owner->fn.instr, OP_DIV_F); break;
			}
			*r = (Ret){ tDst,false,true };
			if (exprMulPrim(r)) {
				return true;
			}
		}
		else tkerr("invalid expression after '/'");
	}
	iTk = start;
	if (owner) delInstrAfter(startInstr);
	return true;
}

bool exprMul(Ret* r) {
	if (exprCast(r)) {
		return exprMulPrim(r);
	}
	return false;
}

// exprAdd: exprAdd ( ADD | SUB ) exprMul | exprMul
// exprAdd: exprMul exprAddPrim
// exprAddPrim : ( ADD | SUB ) exprMul exprAddPrim | e
bool exprAddPrim(Ret* r) {
	Token* op;
	Token* start = iTk;
	Instr* startInstr = owner ? lastInstr(owner->fn.instr) : NULL;
	if (consume(ADD)) {
		op = iTk;
		Ret right;
		Instr* lastLeft = lastInstr(owner->fn.instr);
		addRVal(&owner->fn.instr, r->lval, &r->type);
		if (exprMul(&right)) {
			Type tDst;
			if (!arithTypeTo(&r->type, &right.type, &tDst)) tkerr("invalid operand type for '+'");
			addRVal(&owner->fn.instr, right.lval, &right.type);
			insertConvIfNeeded(lastLeft, &r->type, &tDst);
			insertConvIfNeeded(lastInstr(owner->fn.instr), &right.type, &tDst);
			switch (tDst.tb) {
			case TB_INT:	addInstr(&owner->fn.instr, OP_ADD_I); break;
			case TB_DOUBLE: addInstr(&owner->fn.instr, OP_ADD_F); break;
			}
			*r = (Ret){ tDst,false,true };
			if (exprAddPrim(r)) {
				return true;
			}
		}
		else tkerr("invalid expression after '+'");
	}
	iTk = start;
	if (owner) delInstrAfter(startInstr);
	if (consume(SUB)) {
		op = iTk;
		Ret right;
		Instr* lastLeft = lastInstr(owner->fn.instr);
		addRVal(&owner->fn.instr, r->lval, &r->type);
		if (exprMul(&right)) {
			Type tDst;
			if (!arithTypeTo(&r->type, &right.type, &tDst)) tkerr("invalid operand type for '-'");
			addRVal(&owner->fn.instr, right.lval, &right.type);
			insertConvIfNeeded(lastLeft, &r->type, &tDst);
			insertConvIfNeeded(lastInstr(owner->fn.instr), &right.type, &tDst);
			switch (tDst.tb) {
			case TB_INT:	addInstr(&owner->fn.instr, OP_SUB_I); break;
			case TB_DOUBLE: addInstr(&owner->fn.instr, OP_SUB_F); break;
			}
			*r = (Ret){ tDst,false,true };
			if (exprAddPrim(r)) {
				return true;
			}
		}
		else tkerr("invalid expression after '-'");
	}
	iTk = start;
	if (owner) delInstrAfter(startInstr);
	return true;
}

bool exprAdd(Ret* r) {
	if (exprMul(r)) {
		return exprAddPrim(r);
	}
	return false;
}

// exprRel: exprRel ( LESS | LESSEQ | GREATER | GREATEREQ ) exprAdd | exprAdd
// exprRel: exprAdd exprRelPrim
// exprRelPrim : ( LESS | LESSEQ | GREATER | GREATEREQ ) exprAdd exprRelPrim | e
bool exprRelPrim(Ret* r) {
	Token* op;
	Token* start = iTk;
	Instr* startInstr = owner ? lastInstr(owner->fn.instr) : NULL;
	if (consume(LESS)) {
		op = iTk;
		Ret right;
		Instr* lastLeft = lastInstr(owner->fn.instr);
		addRVal(&owner->fn.instr, r->lval, &r->type);
		if (exprAdd(&right)) {
			Type tDst;
			if (!arithTypeTo(&r->type, &right.type, &tDst)) tkerr("invalid operand type for '<'");
			addRVal(&owner->fn.instr, right.lval, &right.type);
			insertConvIfNeeded(lastLeft, &r->type, &tDst);
			insertConvIfNeeded(lastInstr(owner->fn.instr), &right.type, &tDst);
			switch (tDst.tb) {
			case TB_INT:	addInstr(&owner->fn.instr, OP_LESS_I); break;
			case TB_DOUBLE: addInstr(&owner->fn.instr, OP_LESS_F); break;
			}
			*r = (Ret){ {TB_INT,NULL,-1},false,true };
			if (exprRelPrim(r)) {
				return true;
			}
		}
		else tkerr("invalid expression after '<'");
	}
	iTk = start;
	if (owner) delInstrAfter(startInstr);
	if (consume(LESSEQ)) {
		op = iTk;
		Ret right;
		Instr* lastLeft = lastInstr(owner->fn.instr);
		addRVal(&owner->fn.instr, r->lval, &r->type);
		if (exprAdd(&right)) {
			Type tDst;
			if (!arithTypeTo(&r->type, &right.type, &tDst)) tkerr("invalid operand type for '<='");
			addRVal(&owner->fn.instr, right.lval, &right.type);
			insertConvIfNeeded(lastLeft, &r->type, &tDst);
			insertConvIfNeeded(lastInstr(owner->fn.instr), &right.type, &tDst);
			switch (tDst.tb) {
			case TB_INT:	addInstr(&owner->fn.instr, OP_LESSEQ_I); break;
			case TB_DOUBLE: addInstr(&owner->fn.instr, OP_LESSEQ_F); break;
			}
			*r = (Ret){ {TB_INT,NULL,-1},false,true };
			if (exprRelPrim(r)) {
				return true;
			}
		}
		else tkerr("invalid expression after '<='");
	}
	iTk = start;
	if (owner) delInstrAfter(startInstr);
	if (consume(GREATER)) {
		op = iTk;
		Ret right;
		Instr* lastLeft = lastInstr(owner->fn.instr);
		addRVal(&owner->fn.instr, r->lval, &r->type);
		if (exprAdd(&right)) {
			Type tDst;
			if (!arithTypeTo(&r->type, &right.type, &tDst)) tkerr("invalid operand type for '>'");
			addRVal(&owner->fn.instr, right.lval, &right.type);
			insertConvIfNeeded(lastLeft, &r->type, &tDst);
			insertConvIfNeeded(lastInstr(owner->fn.instr), &right.type, &tDst);
			switch (tDst.tb) {
			case TB_INT:	addInstr(&owner->fn.instr, OP_GREATER_I); break;
			case TB_DOUBLE: addInstr(&owner->fn.instr, OP_GREATER_F); break;
			}
			*r = (Ret){ {TB_INT,NULL,-1},false,true };
			if (exprRelPrim(r)) {
				return true;
			}
		}
		else tkerr("invalid expression after '>'");
	}
	iTk = start;
	if (owner) delInstrAfter(startInstr);
	if (consume(GREATEREQ)) {
		op = iTk;
		Ret right;
		Instr* lastLeft = lastInstr(owner->fn.instr);
		addRVal(&owner->fn.instr, r->lval, &r->type);
		if (exprAdd(&right)) {
			Type tDst;
			if (!arithTypeTo(&r->type, &right.type, &tDst)) tkerr("invalid operand type for '>='");
			addRVal(&owner->fn.instr, right.lval, &right.type);
			insertConvIfNeeded(lastLeft, &r->type, &tDst);
			insertConvIfNeeded(lastInstr(owner->fn.instr), &right.type, &tDst);
			switch (tDst.tb) {
			case TB_INT:	addInstr(&owner->fn.instr, OP_GREATEREQ_I); break;
			case TB_DOUBLE: addInstr(&owner->fn.instr, OP_GREATEREQ_F); break;
			}
			*r = (Ret){ {TB_INT,NULL,-1},false,true };
			if (exprRelPrim(r)) {
				return true;
			}
		}
		else tkerr("invalid expression after '>='");
	}
	iTk = start;
	if (owner) delInstrAfter(startInstr);
	return true;
}

bool exprRel(Ret* r) {
	if (exprAdd(r)) {
		return exprRelPrim(r);
	}
	return false;
}

// exprEq: exprEq ( EQUAL | NOTEQ ) exprRel | exprRel
// exprEq: exprRel exprEqPrim
// exprEqPrim : ( EQUAL | NOTEQ ) exprRel exprEqPrim | e
bool exprEqPrim(Ret* r) {
	Token* start = iTk;
	Instr* startInstr = owner ? lastInstr(owner->fn.instr) : NULL;
	if (consume(EQUAL)) {
		Ret right;
		if (exprRel(&right)) {
			Type tDst;
			if (!arithTypeTo(&r->type, &right.type, &tDst)) tkerr("invalid operand type for '=='");
			*r = (Ret){ {TB_INT,NULL,-1},false,true };
			if (exprEqPrim(r)) {
				return true;
			}
		}
		else tkerr("invalid expression after '=='");
	}
	iTk = start;
	if (owner) delInstrAfter(startInstr);
	if (consume(NOTEQ)) {
		Ret right;
		if (exprRel(&right)) {
			Type tDst;
			if (!arithTypeTo(&r->type, &right.type, &tDst)) tkerr("invalid operand type for '!='");
			*r = (Ret){ {TB_INT,NULL,-1},false,true };
			if (exprEqPrim(r)) {
				return true;
			}
		}
		else tkerr("invalid expression after '!='");
	}
	iTk = start;
	if (owner) delInstrAfter(startInstr);
	return true;
}

bool exprEq(Ret* r) {
	if (exprRel(r)) {
		return exprEqPrim(r);
	}
	return false;
}

// exprAnd: exprAnd AND exprEq | exprEq
// exprAnd: exprEq exprAndPrim
// exprAndPrim: AND exprEq exprAndPrim | e
bool exprAndPrim(Ret* r) {
	Token* start = iTk;
	Instr* startInstr = owner ? lastInstr(owner->fn.instr) : NULL;
	if (consume(AND)) {
		Ret right;
		if (exprEq(&right)) {
			Type tDst;
			if (!arithTypeTo(&r->type, &right.type, &tDst)) tkerr("invalid operand type for '&&'");
			*r = (Ret){ {TB_INT,NULL,-1},false,true };
			if (exprAndPrim(r)) {
				return true;
			}
		}
		else tkerr("invalid expression after '&&'");
	}
	iTk = start;
	if (owner) delInstrAfter(startInstr);
	return true;
}

bool exprAnd(Ret* r) {
	if (exprEq(r)) {
		return exprAndPrim(r);
	}
	return false;
}

// exprOr: exprOr OR exprAnd | exprAnd
// A: A a1 | ... | A am | B1 | ... | Bn // regula initiala
// A = exprOr
// a1 = Or exprAnd
// B1 = exprAnd

// A: B1 A' | ... | Bn A'
// exprOr: exprAnd exprOrPrim

// A': a1 A' | ... | am A' | e
// exprOrPrim: OR exprAnd exprOrPrim | e
// exprOrPrim: ( OR exprAnd exprOrPrim ) ?
bool exprOrPrim(Ret* r) {
	if (consume(OR)) {
		Ret right;
		if (exprAnd(&right)) {
			Type tDst;
			if (!arithTypeTo(&r->type, &right.type, &tDst)) tkerr("invalid operand type for '||'");
			*r = (Ret){ {TB_INT,NULL,-1},false,true };
			if (exprOrPrim(r)) {
				return true;
			}
		}
		else tkerr("invalid expression after '||'");
	}
	return true;
}

bool exprOr(Ret* r) {
	if (exprAnd(r)) {
		return exprOrPrim(r);
	}
	return false;
}

// exprAssign: exprUnary ASSIGN exprAssign | exprOr
bool exprAssign(Ret* r) {
	Ret rDst;
	Token* start = iTk;
	Instr* startInstr = owner ? lastInstr(owner->fn.instr) : NULL;
	if (exprUnary(&rDst)) {
		if (consume(ASSIGN)) {
			if (exprAssign(r)) {
				if (!rDst.lval) tkerr("the assign destination must be a left-value");
				if (rDst.ct) tkerr("the assign destination cannot be constant");
				if (!canBeScalar(&rDst)) tkerr("the assign destination must be scalar");
				if (!canBeScalar(r)) tkerr("the assign source must be scalar");
				if (!convTo(&r->type, &rDst.type)) tkerr("the assign source cannot be converted to destination");
				r->lval = false;
				r->ct = true;

				addRVal(&owner->fn.instr, r->lval, &r->type);
				insertConvIfNeeded(lastInstr(owner->fn.instr), &r->type, &rDst.type);
				switch (rDst.type.tb) {
				case TB_INT:	addInstr(&owner->fn.instr, OP_STORE_I); break;
				case TB_DOUBLE: addInstr(&owner->fn.instr, OP_STORE_F); break;
				}

				return true;
			}
			else tkerr("invalid expression after '='");
		}
	}
	iTk = start;
	if (owner) delInstrAfter(startInstr);
	return exprOr(r);
}

// expr: exprAssign
bool expr(Ret* r) {
	return exprAssign(r);
}

bool stm();

// stmCompound: LACC ( varDef | stm )* RACC
bool stmCompound(bool newDomain) {
	if (consume(LACC)) {
		if (newDomain) pushDomain();
		while (varDef() || stm()) {}
		if (consume(RACC)) {
			if (newDomain) dropDomain();
			return true;
		}
		else tkerr("missing '}'");
	}
	return false;
}

// stm: stmCompound | IF LPAR expr RPAR stm ( ELSE stm )? | WHILE LPAR expr RPAR stm | RETURN expr? SEMICOLON | expr? SEMICOLON
bool stm() {
	Ret rCond, rExpr;
	Token* start = iTk;
	Instr* startInstr = owner ? lastInstr(owner->fn.instr) : NULL;
	if (stmCompound(true)) {
		return true;
	}
	if (consume(IF)) {
		if (consume(LPAR)) {
			if (expr(&rCond)) {
				if (!canBeScalar(&rCond)) {
					tkerr("the if condition must be a scalar value");
				}
				if (consume(RPAR)) {
					addRVal(&owner->fn.instr, rCond.lval, &rCond.type);
					Type intType = { TB_INT,NULL,-1 };
					insertConvIfNeeded(lastInstr(owner->fn.instr), &rCond.type, &intType);
					Instr* ifJF = addInstr(&owner->fn.instr, OP_JF);
					if (stm()) {
						if (consume(ELSE)) {
							Instr* ifJMP = addInstr(&owner->fn.instr, OP_JMP);
							ifJF->arg.instr = addInstr(&owner->fn.instr, OP_NOP);
							if (!stm()) tkerr("invalid expression after 'else'");
							ifJMP->arg.instr = addInstr(&owner->fn.instr, OP_NOP);
						}
						else {
							ifJF->arg.instr = addInstr(&owner->fn.instr, OP_NOP);
						}
						return true;
					}
				}
				else tkerr("missing ')' of 'if'");
			}
			else tkerr("missing 'if' condition");
		}
		else tkerr("missing '(' after 'if'");
	}
	iTk = start;
	if (owner) delInstrAfter(startInstr);
	if (consume(WHILE)) {
		Instr* beforeWhileCond = lastInstr(owner->fn.instr);
		if (consume(LPAR)) {
			if (expr(&rCond)) {
				if (!canBeScalar(&rCond)) {
					tkerr("the while condition must be a scalar value");
				}
				if (consume(RPAR)) {
					addRVal(&owner->fn.instr, rCond.lval, &rCond.type);
					Type intType = { TB_INT,NULL,-1 };
					insertConvIfNeeded(lastInstr(owner->fn.instr), &rCond.type, &intType);
					Instr* whileJF = addInstr(&owner->fn.instr, OP_JF);
					if (stm()) {
						addInstr(&owner->fn.instr, OP_JMP)->arg.instr = beforeWhileCond->next;
						whileJF->arg.instr = addInstr(&owner->fn.instr, OP_NOP);
						return true;
					}
				}
				else tkerr("missing ')'");
			}
			else tkerr("missing 'while' condition");
		}
		else tkerr("missing '(' after 'while'");
	}
	iTk = start;
	if (owner) delInstrAfter(startInstr);
	if (consume(RETURN)) {
		if (expr(&rExpr)) {
			if (owner->type.tb == TB_VOID) tkerr("a void function cannot return a value");
			if (!canBeScalar(&rExpr)) tkerr("the return value must be a scalar value");
			if (!convTo(&rExpr.type, &owner->type)) tkerr("cannot convert the return expression type to the function return type");

			addRVal(&owner->fn.instr, rExpr.lval, &rExpr.type);
			insertConvIfNeeded(lastInstr(owner->fn.instr), &rExpr.type, &owner->type);
			addInstrWithInt(&owner->fn.instr, OP_RET, symbolsLen(owner->fn.params));
		}
		else {
			if (owner->type.tb != TB_VOID) tkerr("a non-void function must return a value");
			addInstr(&owner->fn.instr, OP_RET_VOID);
		}
		if (consume(SEMICOLON)) {
			return true;
		}
		else tkerr("missing ';' after 'return'");
	}
	iTk = start;
	if (owner) delInstrAfter(startInstr);
	if (expr(&rExpr)) {
		if (rExpr.type.tb != TB_VOID) {
			addInstr(&owner->fn.instr, OP_DROP);
		}
		if (consume(SEMICOLON)) {
			return true;
		}
		else tkerr("missing ';' after expression");
	}
	iTk = start;
	if (owner) delInstrAfter(startInstr);
	if (consume(SEMICOLON)) {
		return true;
	}
	return false;
}

// fnDef: ( typeBase | VOID ) ID LPAR ( fnParam ( COMMA fnParam )* )? RPAR stmCompound
bool fnDef() {
	Type t;
	Token* start = iTk;
	Instr* startInstr = owner ? lastInstr(owner->fn.instr) : NULL;
	if (typeBase(&t)) {
		if (consume(ID)) {
			Token* tkName = consumedTk;
			if (consume(LPAR)) {
				Symbol* fn = findSymbolInDomain(symTable, tkName->text);
				if (fn) tkerr("symbol redefinition: %s", tkName->text);
				fn = newSymbol(tkName->text, SK_FN);
				fn->type = t;
				addSymbolToDomain(symTable, fn);
				owner = fn;
				pushDomain();
				if (fnParam()) {
					while (consume(COMMA)) {
						if (!fnParam()) tkerr("invalid expression after ','");
					}
				}
				if (consume(RPAR)) {
					addInstr(&fn->fn.instr, OP_ENTER);
					if (stmCompound(false)) {
						fn->fn.instr->arg.i = symbolsLen(fn->fn.locals);
						if (fn->type.tb == TB_VOID)
							addInstrWithInt(&fn->fn.instr, OP_RET_VOID, symbolsLen(fn->fn.params));
						dropDomain();
						owner = NULL;
						return true;
					}
				}
			}
		}
	}
	iTk = start;
	if (owner) delInstrAfter(startInstr);
	if (consume(VOID)) {
		t.tb = TB_VOID;
		if (consume(ID)) {
			Token* tkName = consumedTk;
			if (consume(LPAR)) {
				Symbol* fn = findSymbolInDomain(symTable, tkName->text);
				if (fn) tkerr("symbol redefinition: %s", tkName->text);
				fn = newSymbol(tkName->text, SK_FN);
				fn->type = t;
				addSymbolToDomain(symTable, fn);
				owner = fn;
				pushDomain();
				if (fnParam()) {
					while (consume(COMMA)) {
						if (!fnParam()) tkerr("invalid expression after ','");
					}
				}
				if (consume(RPAR)) {
					addInstr(&fn->fn.instr, OP_ENTER);
					if (stmCompound(false)) {
						fn->fn.instr->arg.i = symbolsLen(fn->fn.locals);
						if (fn->type.tb == TB_VOID)
							addInstrWithInt(&fn->fn.instr, OP_RET_VOID, symbolsLen(fn->fn.params));
						dropDomain();
						owner = NULL;
						return true;
					}
				}
			}
		}
	}
	iTk = start;
	if (owner) delInstrAfter(startInstr);
	return false;
}

// unit: ( structDef | fnDef | varDef )* END
bool unit() {
	while (structDef() || fnDef() || varDef()) {}
	return consume(END);
}

void parse(Token* tokens) {
	list = tokens;
	iTk = tokens;
	if (!unit()) tkerr("syntax error");
}
