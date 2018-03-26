open Ast

type subst = (string,texpr) Hashtbl.t

let create = fun () -> Hashtbl.create 10

let extend (orig:subst) (new_id:string) (new_type:texpr) =
  Hashtbl.add orig new_id new_type

let remove (ht:subst) (id:string) =
  Hashtbl.remove ht id

let lookup (ht:subst) (id:string) = Hashtbl.find_opt ht id

let string_of_subs (ht:subst) =
  if Hashtbl.length ht == 0 then "{}" else
  let orig = (fun h -> Hashtbl.fold (fun k v acc -> k ^ ":=" ^ (string_of_texpr v) ^ ", " ^acc) h "") ht
  in "{"^(String.sub orig 0 (String.length orig - 2))^"}"

let domain = fun h -> Hashtbl.fold (fun k v acc -> k :: acc) h []

let rec apply_to_expr s = function
  | Proc(arg, arg_ty, body) ->
        let out_ty = (match lookup s arg with
                      | Some ty -> ty
                      | None -> arg_ty)
        in Proc(arg, out_ty, (apply_to_expr s body))
  | e -> e

let rec apply_to_texpr s = function
  | VarType id when (lookup s id) = None -> VarType id
  | VarType(id) -> (match (lookup s id) with |Some ty -> ty | _ -> failwith "impossible")
  | FuncType(arg, body) -> FuncType(apply_to_texpr s arg, apply_to_texpr s body)
  | RefType(r) -> RefType(apply_to_texpr s r)
  | e -> e

let apply_to_env s1 s2 =
  Hashtbl.iter
  (fun k v ->
      (Hashtbl.filter_map_inplace
        (fun k1 v1 -> if k == k1 then Some(v) else Some(apply_to_texpr s1 v1))
        s2))
  s1

let rec join = function
    | [] -> create ()
    | h::t -> let s = join t in
              Hashtbl.iter (fun k v -> match lookup s k with
                                       | None -> extend s k v
                                       | Some ty -> remove s k; extend s k (apply_to_texpr h ty)) h; s

