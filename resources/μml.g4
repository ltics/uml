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
    | true
    | false
    | number
    ;

var: VAR;
true: TRUE;
false: FALSE;
number: NUMBER;

type: BOOLT
    | INTT
    | type ARROWT type
    | LPAREN type LPAREN
    ;

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

NUMBER: '-'? [0-9]+;
VAR: [a-zA-Z]+;
LPAREN: '(';
RPAREN: ')';

WS: ('\t' | ' ' | '\r' | '\n'| '\u000C')+ -> skip;