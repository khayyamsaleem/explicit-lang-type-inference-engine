open Unification
open Subs
open Ast


type 'a error = OK of 'a | Error of string

type typing_judgement = subst*expr*texpr

let rec infer' (e:expr) (n:int): (int*typing_judgement) error =
  match e with
  | Unit -> OK (n, (create (), Unit, UnitType))
  | Var x -> OK (n, (create (), Var x, VarType x))
  | Int x -> OK (n, (create (), Int x, IntType))
  | Add (a, b) -> OK (n,
                      (
                        join [getTypingContext a n; getTypingContext b n],
                        Add(a, b),
                        IntType
                      ))
  | Sub (a, b) -> OK (n,
                      (
                        join [getTypingContext a n; getTypingContext b n],
                        Add(a, b),
                        IntType
                      ))
  | Mul (a, b) -> OK (n,
                      (
                        join [getTypingContext a n; getTypingContext b n],
                        Add(a, b),
                        IntType
                      ))
  | Div (a, b) -> OK (n,
                      (
                        join [getTypingContext a n; getTypingContext b n],
                        Add(a, b),
                        IntType
                      ))
  | _ -> failwith "infer': undefined"

and

getTypingContext (e:expr) (n:int): subst =
  match (infer' e n) with
  |OK (_, (s,_,_)) -> s
  |Error err -> failwith err


let string_of_typing_judgement = function
  | ht,e,t -> string_of_subs ht ^" |- "^ string_of_expr e ^ " : " ^ string_of_texpr t

let infer_type (AProg e) =
  match infer' e 0 with
  | OK (_, tj) -> string_of_typing_judgement tj
  | Error s -> "Error! "^ s


let parse s =
  let lexbuf = Lexing.from_string s in
  let ast = Parser.prog Lexer.read lexbuf in
  ast


(* Interpret an expression *)
let inf (e:string) : string =
  e |> parse |> infer_type

let test (n:int) : string =
  Examples.expr n |> parse |> infer_type
