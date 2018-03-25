open Ast

type  unif_result = UOk of Subs.subst | UError of texpr*texpr

let mgu lst:(Ast.texpr*Ast.texpr) list = failwith("later")
(*   let rec unify lst_of_pairs subs = match lst_of_pairs with
*     | [] -> UOk(subs)
*     | (tya,tyb)::rest -> failwith("later")
*   in *)
