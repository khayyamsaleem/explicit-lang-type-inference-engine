open Ast
open Subs

type  unif_result = UOk of Subs.subst | UError of texpr*texpr


let rec mgu = function
  | [] -> UOk (create ())
  | (IntType, IntType) :: rest | (BoolType, BoolType) :: rest -> mgu(rest)
  | (VarType a, VarType b) :: rest ->
    (match mgu rest with
     | UOk substitution_env when a == b -> UOk(substitution_env)
     | UOk substitution_env -> extend substitution_env a (VarType(b)); UOk(substitution_env)
     | UError (ty1, ty2) -> UError (ty1, ty2))
  | (ty, VarType a)::rest
    when (match SetStr.find_opt a (fv_of_type ty) with |Some _ -> false | _ -> true)
      ->
    (match mgu rest with
     | UOk substitution_env -> extend substitution_env a ty; UOk(substitution_env)
     | UError(ty1, ty2) -> UError(ty1, ty2))
  | (VarType a, ty)::rest
    when (match SetStr.find_opt a (fv_of_type ty) with |Some _ -> false | _ -> true)
    ->
    (match mgu rest with
     | UOk substitution_env -> extend substitution_env a ty; UOk(substitution_env)
     | UError(ty1, ty2) -> UError(ty1, ty2))
  | (VarType a, ty)::rest | (ty, VarType a)::rest -> UError(ty, VarType a)
  | (RefType a, RefType b) ::rest -> mgu((a,b)::rest)
  | (FuncType(arg1, body1), FuncType(arg2, body2))::rest ->
    (match mgu [(arg1,arg2)] with
     | UOk se_a ->
       (match mgu [(body1,body2)] with
        | UOk se_b ->
          (match mgu rest with
           | UOk substitution_env -> UOk(join [se_a; se_b; substitution_env])
           | err -> err)
        | err -> err)
     | err -> err)
  |(ty1, ty2)::_ -> UError(ty1, ty2)

