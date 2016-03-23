grammar μml;

file: expr* EOF;

expr: BR_OPEN expr BR_CLOSE
    | expr TIMES expr
    | expr PLUS expr
    | expr MINUS expr
    | IF expr THEN expr ELSE expr
    | number
    ;

number: NUMBER;

PLUS:  'plus'  | '+';
MINUS: 'minus' | '-';
TIMES: 'times' | '*';
IF: 'if';
THEN: 'then';
ELSE: 'else';

NUMBER: '-'? [0-9]+;
BR_OPEN: '(';
BR_CLOSE: ')';

WS: ('\t' | ' ' | '\r' | '\n'| '\u000C')+ -> skip;