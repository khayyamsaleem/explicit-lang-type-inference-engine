

type  unif_result = UOk of Subs.subst | UError of Ast.texpr*Ast.texpr
  
val mgu : (Ast.texpr*Ast.texpr) list -> unif_result
