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
  let orig = (fun h -> Hashtbl.fold (fun k v acc -> k ^ ": " ^ (string_of_texpr v) ^ ", " ^acc) h "") ht
  in "{"^(String.sub orig 0 (String.length orig - 2))^"}"

let domain = fun h -> Hashtbl.fold (fun k v acc -> k :: acc) h []

let join (lst:subst list):subst = match lst with
    | [] -> create ()
    | h::t -> List.fold_right (fun ht1 ht2 -> Hashtbl.iter (fun k v -> Hashtbl.add ht2 k v) ht2; ht1) t h;;

(* use unify for join??? *)
