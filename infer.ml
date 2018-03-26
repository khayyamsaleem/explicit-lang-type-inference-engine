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
      | Add (a, b) ->
        (match (get_type a num type_environment) with
         | OK (num, (s1, a, ty1)) ->
           (match (get_type b num type_environment) with
            | OK(num, (s2, b, ty2)) ->
                let s_low = join [s1; s2] in
                (match mgu [(ty1, Ast.IntType);(ty2, Ast.IntType)] with
                 | UOk(s) -> apply_to_env s s_low; OK(n, (s_low, Add(a,b), Ast.IntType))
                 | UError(ty1, ty2) ->
                      Error((string_of_texpr ty1) ^ " not unifiable with " ^ (string_of_texpr ty2))
                )
            | err -> err
           )
         | err -> err)
      | Sub (a, b) ->
        (match (get_type a num type_environment) with
         | OK (num, (s1, a, ty1)) ->
           (match (get_type b num type_environment) with
            | OK(num, (s2, b, ty2)) ->
                let s_low = join [s1; s2] in
                (match mgu [(ty1, Ast.IntType);(ty2, Ast.IntType)] with
                 | UOk(s) -> apply_to_env s s_low; OK(n, (s_low, Sub(a,b), Ast.IntType))
                 | UError(ty1, ty2) ->
                      Error((string_of_texpr ty1) ^ " not unifiable with " ^ (string_of_texpr ty2))
                )
            | err -> err
           )
         | err -> err)
      | Mul (a, b) ->
        (match (get_type a num type_environment) with
         | OK (num, (s1, a, ty1)) ->
           (match (get_type b num type_environment) with
            | OK(num, (s2, b, ty2)) ->
                let s_low = join [s1; s2] in
                (match mgu [(ty1, Ast.IntType);(ty2, Ast.IntType)] with
                 | UOk(s) -> apply_to_env s s_low; OK(n, (s_low, Mul(a,b), Ast.IntType))
                 | UError(ty1, ty2) ->
                      Error((string_of_texpr ty1) ^ " not unifiable with " ^ (string_of_texpr ty2))
                )
            | err -> err
           )
         | err -> err)
      | Div (a, b) ->
        (match (get_type a num type_environment) with
         | OK (num, (s1, a, ty1)) ->
           (match (get_type b num type_environment) with
            | OK(num, (s2, b, ty2)) ->
                let s_low = join [s1; s2] in
                (match mgu [(ty1, Ast.IntType);(ty2, Ast.IntType)] with
                 | UOk(s) -> apply_to_env s s_low; OK(n, (s_low, Div(a,b), Ast.IntType))
                 | UError(ty1, ty2) ->
                      Error((string_of_texpr ty1) ^ " not unifiable with " ^ (string_of_texpr ty2))
                )
            | err -> err
           )
         | err -> err)
      | Proc(arg, type_variable, body) ->
        (match (get_type body num type_environment) with
        | OK(num, (s, body, t)) ->
          remove s arg;
          let type_variable = (match lookup s arg with
                                |Some t -> t
                                |None -> type_variable)
          in OK(num, (s, Proc(arg, type_variable, body), FuncType(type_variable, t)))
        | Error err -> Error err)
      | ProcUntyped(arg, body) ->
            (*just strip away type spec and reduce to proc case*)
            get_type (Proc(arg, VarType((string_of_int (num+1))), body)) num type_environment
      | IsZero(a) ->
        (match (get_type a num type_environment) with
         | OK(num, (s_low, a, ty)) ->
           (match mgu [ty, IntType] with
            | UOk s -> apply_to_env s s_low; OK(num, (s_low, (IsZero(a)), BoolType))
            | UError (ty1, ty2) ->
                Error((string_of_texpr ty1) ^ " not unifiable with " ^ (string_of_texpr ty2)))
         | err -> err)
      | Let(id, expression, body) ->
            (match (get_type expression num type_environment) with
             | OK(num, (s1, expression, ty_expression)) ->
               let temp_tenv = type_environment in
               extend temp_tenv id ty_expression;
               (match get_type body num temp_tenv with
                | OK(num, (s2, body, ty_body)) ->
                  remove s1 id; remove s2 id; OK(num, (join [s1;s2], Let(id, expression, body), ty_body))
                | err -> err)
             | err -> err)
      | App(f, arg) ->
            (match get_type f num type_environment with
             |OK(num, (s, f, ty_f)) ->
               let temp_tenv = join [s; type_environment] in
               (match get_type arg num temp_tenv with
                | OK(num, (s, arg, ty_arg)) ->
                      let temp_tenv = join [s;temp_tenv] in
                      let out_ty = (match ty_f with
                                    | FuncType (_, out_ty) -> out_ty
                                    | _ -> VarType((string_of_int (num+1))))
                      in (match mgu [(ty_f, FuncType(ty_arg, out_ty))] with
                          | UOk(s) -> apply_to_env s temp_tenv;
                                     OK(num+1, (temp_tenv, App(f, arg), apply_to_texpr s out_ty))
                          | UError(ty1, ty2) ->
                              Error((string_of_texpr ty1) ^ " not unifiable with " ^ (string_of_texpr ty2)))
                | err -> err)
             | err -> err)
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
