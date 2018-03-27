(* The type of the abstract syntax tree (AST). *)

type expr =
  | Unit
  | Var of string
  | Int of int
  | Add of expr*expr
  | Sub of expr*expr
  | Mul of expr*expr
  | Div of expr*expr
  | Let of string*expr*expr
  | IsZero of expr
  | ITE of expr*expr*expr
  | Proc of string*texpr*expr
  | ProcUntyped of string*expr
  | App of expr*expr
  | Letrec of texpr*string*string*texpr*expr*expr
  | LetrecUntyped of string*string*expr*expr
  | Set of string*expr
  | BeginEnd of expr list
  | NewRef of expr
  | DeRef of expr
  | SetRef of expr*expr
and
  texpr =
  | IntType
  | BoolType
  | UnitType
  | VarType of string
  | FuncType of texpr*texpr
  | RefType of texpr

type prog = AProg of expr


let rec string_of_expr e =
  match e with
  | Unit  -> "()"
  | Var s -> s
  | Int n -> string_of_int n
  | Add(e1,e2) -> "Add(" ^ (string_of_expr e1) ^ "," ^ string_of_expr e2 ^ ")"
  | Sub(e1,e2) -> "Sub(" ^ (string_of_expr e1) ^ "," ^ string_of_expr e2 ^ ")"
  | Mul(e1,e2) -> "Mul(" ^ (string_of_expr e1) ^ "," ^ string_of_expr e2 ^ ")"
  | Div(e1,e2) -> "Div(" ^ (string_of_expr e1) ^ "," ^ string_of_expr e2 ^ ")"
  | NewRef(e) -> "NewRef(" ^ (string_of_expr e) ^ ")"
  | DeRef(e) -> "DeRef(" ^ (string_of_expr e) ^ ")"
  | SetRef(e1,e2) -> "SetRef(" ^ (string_of_expr e1) ^ "," ^ string_of_expr e2 ^ ")"
  | Let(x,def,body) -> "Let("^x^","^string_of_expr def ^","^ string_of_expr body ^")"
  | Proc(x,t,body) -> "Proc("^x^":"^string_of_texpr t^"," ^ string_of_expr body ^")"
  | ProcUntyped(x,body) -> "ProcU("^x^"," ^ string_of_expr body ^")"
  | App(e1,e2) -> "App("^string_of_expr e1 ^"," ^ string_of_expr e2^")"
  | IsZero(e) -> "Zero?("^string_of_expr e ^")"
  | ITE(e1,e2,e3) -> "IfThenElse("^string_of_expr e1^"," ^ string_of_expr e2^"," ^ string_of_expr e3  ^")"
  | Letrec(tRes,x,param,tPara, def,body) -> "Letrec("^string_of_texpr
  tRes^" "^x^","^param^":"^string_of_texpr tPara ^","^ string_of_expr def ^","^ string_of_expr body ^")"
  | LetrecUntyped(x,param,def,body) -> "Letrec("^x^","^param^","^ string_of_expr def ^","^ string_of_expr body ^")"
  | Set(x,rhs) -> "Set("^x^","^string_of_expr rhs^")"
  | BeginEnd(es) -> "BeginEnd(" ^ List.fold_left (fun x y -> x^","^y)
                      "" (List.map string_of_expr es) ^")"
and string_of_texpr = function
  | IntType -> "int"
  | BoolType -> "bool"
  | UnitType -> "unit"
  | VarType id -> "_V"^id
  | FuncType(t1,t2) -> "("^string_of_texpr t1^"->"^string_of_texpr t2^")"
  | RefType(t) -> "Ref("^string_of_texpr t^")"


let string_of_prog (AProg e)  = string_of_expr e

module SetStr = Set.Make(struct type t = string let compare = compare end)

let rec fv = function
  | Unit -> SetStr.empty
  | Var x -> SetStr.add x @@ SetStr.empty
  | Int n -> SetStr.empty
  | Add(e1,e2) -> SetStr.union (fv e1) (fv e2)
  | Sub(e1,e2) -> SetStr.union (fv e1) (fv e2)
  | Mul(e1,e2) -> SetStr.union (fv e1) (fv e2)
  | Div(e1,e2) -> SetStr.union (fv e1) (fv e2)
  | NewRef(e) -> fv e
  | DeRef(e) -> fv e
  | SetRef(e1,e2) -> SetStr.union (fv e1) (fv e2)
  | Let(x,def,body) -> SetStr.union (fv def) (SetStr.remove x (fv body))
  | Proc(x,t,body) -> SetStr.remove x (fv body)
  | ProcUntyped(x,body) -> SetStr.remove x (fv body)
  | App(e1,e2) -> SetStr.union (fv e1) (fv e2)
  | IsZero(e) -> fv e
  | ITE(e1,e2,e3) -> SetStr.union (SetStr.union (fv e1) (fv e2)) (fv e3)
  | Letrec(tRes,x,param,tPara, def,body) ->
    SetStr.union (SetStr.remove x (fv def)) (SetStr.remove x (fv body))
  | LetrecUntyped(x,param,def,body) ->
    SetStr.union (SetStr.remove x (fv def)) (SetStr.remove x (fv body))
  | Set(x,rhs) -> SetStr.add x (fv rhs)
  | BeginEnd(es) ->  List.fold_left (fun s e -> SetStr.union s (fv e))
                      SetStr.empty  es

let fv_of_prog (AProg e) = fv e

let rec fv_of_type = function
  | IntType | BoolType | UnitType -> SetStr.empty
  | VarType id -> SetStr.singleton id
  | FuncType(t1,t2) -> SetStr.union (fv_of_type t1) (fv_of_type t2)
  | RefType(t) -> fv_of_type t
