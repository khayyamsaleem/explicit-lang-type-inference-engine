
(* The type of tokens. *)

type token = 
  | UNITTYPE
  | TIMES
  | THEN
  | SETREF
  | SET
  | SEMICOLON
  | RPAREN
  | REFTYPE
  | RBRACE
  | PROC
  | PLUS
  | NEWREF
  | MINUS
  | LPAREN
  | LETREC
  | LET
  | LBRACE
  | ISZERO
  | INTTYPE
  | INT of (int)
  | IN
  | IF
  | ID of (string)
  | EQUALS
  | EOF
  | END
  | ELSE
  | DIVIDED
  | DEREF
  | COMMA
  | COLON
  | BOOLTYPE
  | BEGIN
  | ARROW

(* This exception is raised by the monolithic API functions. *)

exception Error

(* The monolithic API. *)

val prog: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (Ast.prog)
