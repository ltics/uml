grammar μml;

file: stat EOF;

stat: def
    | def SEMICOLON2
    | expr
    | expr SEMICOLON2
    | def SEMICOLON2 stat
    | expr SEMICOLON2 stat;

def: LET VAR EQUAL expr;

expr: atom
    | LPAREN expr RPAREN
    | expr expr
    | expr TIMES expr
    | expr PLUS expr
    | expr MINUS expr
    | expr EQUAL expr
    | expr LESS expr
    | IF expr THEN expr ELSE expr
    | FUN VAR LPAREN VAR COLON type RPAREN COLON type IS expr
    ;

atom: var
    | bool
    | int
    ;

var: VAR;
bool: TRUE | FALSE;
int: INT;

type: boolT
    | intT
    | type ARROWT type
    | LPAREN type LPAREN
    ;

intT: INTT;
boolT: BOOLT;

PLUS:  'plus'  | '+';
MINUS: 'minus' | '-';
TIMES: 'times' | '*';
EQUAL: '=';
LESS: '<';

FUN: 'fun';
IS: 'is';
LET: 'let';
IF: 'if';
THEN: 'then';
ELSE: 'else';

SEMICOLON2: ';;';
COLON: ':';

TRUE: 'true';
FALSE: 'false';

BOOLT: 'bool';
INTT: 'int';
ARROWT: '->';

INT: '-'? [0-9]+;
VAR: [0-9a-zA-Z]+;
LPAREN: '(';
RPAREN: ')';

WS: ('\t' | ' ' | '\r' | '\n'| '\u000C')+ -> skip;