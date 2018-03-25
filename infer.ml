open Unification
open Subs
open Ast


type 'a error = OK of 'a | Error of string

type typing_judgement = subst*expr*texpr

let rec infer' (e:expr) (n:int): (int*typing_judgement) error =
  let rec get_type expression num type_environment =
      match expression with
      | Unit -> OK (num, (create (), Unit, UnitType))
      | Int x -> OK (num, (create (), Int x, IntType))
      | Var x when (lookup type_environment x) = None ->
          (*variable has no known type, give it a FRESH type variable*)
            let num = num+1 in
            OK (num,
                let s = create () in
                extend s x (VarType (string_of_int num)); (s, expression, (VarType(string_of_int num)))
               )
      | Var x ->
          (*extend type environment with binding because variable is already in there*)
            OK (num,
                 let s = create () in
                 match lookup type_environment x with
                 | Some type_expression -> extend s x type_expression; (s, expression, type_expression)
                 |_ -> failwith("shouldn't happen")
               )
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
      | Proc(arg, type_variable, body) ->
        (match (get_type body num type_environment) with
        | OK(num, (s, body, t)) ->
          remove s arg;
          let type_variable = (match lookup s arg with
                                |Some t -> t
                                |None -> type_variable)
          in OK(n, (s, Proc(arg, type_variable, body), FuncType(type_variable, t)))
        | Error err -> Error err)
      | ProcUntyped(arg, body) ->
            get_type (Proc(arg, VarType((string_of_int (num+1))), body)) num type_environment
      | _ -> failwith "infer': undefined"
  in get_type e 0 (create ())

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
