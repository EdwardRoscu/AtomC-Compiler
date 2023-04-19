#include <stdio.h>
#include <stdlib.h>

#include "../lexer.h"
#include "../utils.h"
#include "../parser.h"
#include "../ad.h"
#include "../vm.h"

int main() {

    // char* inbuf = loadFile("test/testat.c");
    // puts(inbuf);
    // Token* list = tokenize(inbuf);
    // free(inbuf);
    // showTokens(list); // afisare atomi lexicali (sfarsitul analizei lexicale)
    pushDomain(); // creaza domeniul global in tabela de simboluri
    vmInit(); // initializare masina virtuala
    // parse(list); // analiza sintactica (apelare analizor sintactic)
    // showDomain(symTable, "global"); // afisare domeniu global
    Instr* testCode = genTestProgram(); // genereaza cod de test pentru masina virtuala
    run(testCode); // executie cod masina virtuala

    dropDomain(); // sterge domeniul global
    // freeTokenList(list);
    return 0;
}
