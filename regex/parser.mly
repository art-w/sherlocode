%{
  open Ast
%}

%token EOF
%token ANY
%token <char> CHAR
%token PAR_OPEN PAR_CLOSE
%token RANGE_START RANGE_EXCL_START RANGE RANGE_END
%token OR
%token STAR PLUS OPTIONAL
%token ALPHANUM
%token LINE_START LINE_END
%token WORD_BOUNDARY

%start main
%type <Ast.t> main

%%

main:
| r=regex EOF { r }

regex:
| r = disjunction { r }

disjunction:
| a = disjunction OR b = concat { Or(a, b) }
| a = disjunction OR EOF { a }
| r = concat {r}

concat:
| a = concat b = repetition { And(a, b) }
| r = repetition {r}

repetition:
| r = repetition STAR { Star0 r }
| r = repetition PLUS { Star1 r }
| r = repetition OPTIONAL { Maybe r }
| r = atom { r }

atom:
| PAR_OPEN PAR_CLOSE { Empty }
| PAR_OPEN r = regex PAR_CLOSE { r }
| PAR_OPEN EOF { Empty }
| PAR_OPEN r = regex EOF { r }
| c = char { Char c }
| ANY { Any }
| r=range { r }
| LINE_START { Line_start }
| LINE_END { Line_end }
| WORD_BOUNDARY { Word_boundary }

range:
| ALPHANUM { Char_set Ast.alphanum }
| RANGE_START      r=range_contents { Char_set r }
| RANGE_EXCL_START r=range_contents { Char_set (inv r) }

range_contents:
| a=CHAR RANGE b=CHAR xs=range_contents { Char_set.union (enum a b) xs }
| a=char xs=range_contents { Char_set.add a xs }
| RANGE_END { Char_set.empty }

char:
| a=CHAR { a }
| RANGE { '-' }
