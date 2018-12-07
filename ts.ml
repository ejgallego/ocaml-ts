let oget x = match x with | None -> raise Not_found | Some x -> x

(* Ocaml_common.Ast_iterator.iterator *)
(* Parsetree.type_declaration *)
(* Parsetree.core_type_desc *)

open Parsetree

let dctyp (t : core_type_desc) =
  match t with
  | Ptyp_any -> "any"
  | Ptyp_var _ -> "var"
  | Ptyp_arrow (_, _, _) -> "arrow"
  | Ptyp_tuple _ -> "tuple"
  | Ptyp_constr (_, _) -> "constr"
  | Ptyp_object (_, _) -> "object"
  | Ptyp_class (_, _) -> "class"
  | Ptyp_alias (_, _) -> "alias"
  | Ptyp_variant (_, _, _) -> "variant"
  | Ptyp_poly (_, _) -> "poly"
  | Ptyp_package _ -> "package"
  | Ptyp_extension _ -> "extension"

let process_type_declaration { ptype_name ; ptype_manifest; _ } =
  let s = ptype_name.txt in
  let ptype_manifest = oget ptype_manifest in
  let m = dctyp ptype_manifest.ptyp_desc in
  Format.eprintf "Detected type %s := %s@\n%!" s m

let grab_types_iter acc =
  let type_declaration _iter pt = acc := pt :: !acc in
  Ocaml_common.Ast_iterator.{
    default_iterator with
    type_declaration;
  }

let dummy_loc = Warnings.{loc_start = Lexing.dummy_pos; loc_end = Lexing.dummy_pos; loc_ghost = true}

let _ =
  let ibuf = Lexing.from_string "
type a = int
type b = int
type c = a * b
" in
  let mod_struct = Ocaml_common.Parse.implementation ibuf in
  let acc = ref [] in
  let iter = grab_types_iter acc in
  let () = iter.structure iter mod_struct in
  let acc = List.rev !acc in
  List.iter process_type_declaration acc;

  (* Typing (optional) *)
  let env = Env.initial_safe_string in
  let _tstruct, _sigst, _env =
    try
      Ocaml_common.Typemod.type_structure env mod_struct dummy_loc
    with | e -> raise e
  in
  ()
