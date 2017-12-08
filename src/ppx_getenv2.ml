open Ppx_core

let name = "getenv"

let foo loc = [%expr 18 ]
(* let x : Ppx_ast__Ast_helper.lid = 5 *) 

let is_general what = function
  | Longident.Lident name when String.equal what name -> true
  | _ -> false

let is_par  = is_general "|||"
let is_asgn = is_general ":="
let is_antiquote = is_general "??"
let is_gt = is_general ">"

let classify_name name =
  let helper str =
    try let pos = String.rindex_exn name '_' in

      let name = String.sub name 0 pos in
      let _mo_str = String.sub name (pos+1) (String.length name - pos -1) in
      let loc = Location.none in
      `Location (name, [%expr MemOrder.ACQ ])
    with Not_found -> `Register
       | Invalid_argument msg ->
         raise (Invalid_argument (Printf.sprintf "Got Invalid_argument `%s` while classifying name `%s`" msg name))
  in
  helper name

let expand ~loc ~path:_ (root_expr : expression) =
  let rec helper root k =
    match root.pexp_desc with
    | Pexp_let (_,[vb], body) ->
      do_vb vb.pvb_pat vb.pvb_expr k
    | Pexp_sequence (e1,e2) -> helper e1 (fun l -> helper e2 (fun r -> k [%expr (L.seq [%e l] [%e r]) ] ))
    | Pexp_apply ({pexp_desc=Pexp_ident foo }, [_,l; _,r]) when is_par foo.Location.txt ->
      [%expr L.par [%e helper l k] [%e helper r (fun x -> x)]]
    | Pexp_apply ({pexp_desc=Pexp_ident foo }, [_,l; _,r]) when is_gt  foo.Location.txt ->
      helper l (fun l -> helper r (fun r -> k [%expr (L.binop Op.GT [%e l] [%e r]) ] ))
    | Pexp_apply ({pexp_desc=Pexp_ident foo }, [_,l; _,r]) when is_asgn foo.Location.txt ->
      (* TODO: support locations (with modifiers) and registers (without) there *)
      helper l (fun l -> helper r (fun r -> k [%expr (L.asgn [%e l] [%e r]) ] ))
    | Pexp_apply ({pexp_desc=Pexp_ident foo }, [_,l]) when is_antiquote foo.Location.txt -> k l
    | Pexp_ident {txt=Lident ident} -> begin
        match classify_name ident with
        | `Location (name, expr) -> [%expr L.read MemOrder.SC [%e root]]
        | `Register ->
          (* k root *)               
          [%expr L.var [%e Ast_helper.(Exp.ident (Location.({txt=(Lident ident); loc=none})))]]
      end
    | Pexp_ifthenelse (condb,thenb,Some elseb) ->
      helper condb (fun condb -> helper thenb (fun thenb -> helper elseb (fun elseb -> k [%expr L.if' [%e condb] [%e thenb] [%e elseb]])))
    | _ -> k root
  and do_vb pat rhs k =
    match pat.ppat_desc, rhs.pexp_desc with
    | (Ppat_var varname, Pexp_constant (Pconst_integer (s,_))) ->
      let name = varname.Location.txt in
      (* k [%expr L.(asgn (var [%e name]) (const 18) ] *)
      k [%expr 18 ]
    | _ -> assert false
  in
  helper root_expr (fun x -> x)
    (* [%expr 1] *)
  (* match Caml.Sys.getenv env with
   * | s -> [%expr Some ([%e Ast_builder.Default.estring s ~loc])]
   * | exception Not_found -> [%expr None] *)

let ext =
  Extension.declare
    name
    Extension.Context.expression
    Ast_pattern.(single_expr_payload __)
    expand

let () =
  Ppx_driver.register_transformation name ~extensions:[ext]
