
type subst = (string,Ast.texpr) Hashtbl.t

val create : unit -> subst

val extend : subst -> string -> Ast.texpr -> unit

val remove : subst -> string -> unit

val lookup : subst -> string -> Ast.texpr option

val apply_to_texpr : subst -> Ast.texpr -> Ast.texpr

val apply_to_expr : subst -> Ast.expr -> Ast.expr

val apply_to_env : subst -> subst -> unit

val string_of_subs : subst -> string

val domain : subst -> string list

val join : subst list -> subst
