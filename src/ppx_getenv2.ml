open Ppx_core

let name = "getenv"

let is_general what = function
  | Longident.Lident name when String.equal what name -> true
  | _ -> false

let is_par  = is_general "|||"
let is_asgn = is_general ":="
let is_antiquote = is_general "??"
let is_gt = is_general ">"
let is_known_binop e =
  (is_general "+" e)  || (is_general "*" e) || (is_general "=" e) ||
  (is_general "<>" e) || (is_general "<" e) || (is_general ">" e)

let binop_name_to_wtf ?(loc = Location.none) = function
| _ -> (* TODO: implement all operations *)
    [%expr Op.ADD]

let classify_name name =
  let helper str =
    try let pos = String.rindex_exn name '_' in
      assert (pos > 0);
      let name = String.sub name 0 pos in
      (* let _mo_str = String.sub name (pos+1) (String.length name - pos -1) in *)
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
    | Pexp_constant (Pconst_integer (n,_)) -> k [%expr L.const [%e root]]
    | Pexp_sequence (e1,e2) ->
      helper e1 (fun l -> helper e2 (fun r -> k [%expr (L.seq [%e l] [%e r]) ] ))
    | Pexp_apply ({pexp_desc=Pexp_ident foo }, [_,l; _,r]) when is_par foo.Location.txt ->
      [%expr L.par [%e helper l k] [%e helper r (fun x -> x)]]
    | Pexp_apply ({pexp_desc=Pexp_ident foo }, [_,l; _,r]) when is_gt  foo.Location.txt ->
      helper l (fun l -> helper r (fun r -> k [%expr (L.binop Op.GT [%e l] [%e r]) ] ))
    | Pexp_apply ({pexp_desc=Pexp_ident foo }, [_,l; _,r]) when is_known_binop foo.Location.txt ->
        let op = binop_name_to_wtf foo.Location.txt in
        helper l (fun l -> helper r (fun r -> k [%expr (L.binop [%e op] [%e l] [%e r]) ] ))
    | Pexp_apply ({pexp_desc=Pexp_ident foo }, [_,l; _,r]) when is_asgn foo.Location.txt ->
      (* TODO: support locations (with modifiers) and registers (without) there *)
      helper l (fun l -> helper r (fun r -> k [%expr (L.asgn [%e l] [%e r]) ] ))
    | Pexp_apply ({pexp_desc=Pexp_ident foo }, [_,l]) when is_antiquote foo.Location.txt -> k l
    | Pexp_ident {txt=Lident ident} -> begin
        match classify_name ident with
        | `Location (name, expr) ->
            k [%expr L.read MemOrder.SC (Loc.loc
                [%e Ast_helper.Exp.constant (Pconst_string (ident,None)) ])]
        | `Register ->
          k [%expr L.var [%e Ast_helper.(Exp.constant (Pconst_string(ident,None)))]]
      end
    | Pexp_ifthenelse (condb,thenb,Some elseb) ->
        helper condb (fun condb ->
          helper thenb (fun thenb ->
            helper elseb (fun elseb ->
              k [%expr L.if' [%e condb] [%e thenb] [%e elseb]])))
    (* repeat (statements) until expr *)
    | Pexp_apply ({pexp_desc=Pexp_ident {txt=Lident "repeat"}},
                  [ _, body_expr
                  ; _, {pexp_desc=Pexp_ident {txt=Lident"until"}}
                  ; _, cond_expr
                  ]) ->
        helper body_expr (fun body ->
          helper cond_expr (fun cond_expr ->
            k [%expr L.while' [%e body] [%e cond_expr]] ))
    | _ -> k root
  (* and do_vb pat rhs k = *)
  (*   match pat.ppat_desc, rhs.pexp_desc with *)
  (*   | (Ppat_var varname, Pexp_constant (Pconst_integer (s,_))) -> *)
  (*     let name = varname.Location.txt in *)
  (*     k [%expr 18 ] *)
  (*   | _ -> assert false *)
  in
  helper root_expr (fun x -> x)

let ext =
  Extension.declare
    name
    Extension.Context.expression
    Ast_pattern.(single_expr_payload __)
    expand

let () =
  Ppx_driver.register_transformation name ~extensions:[ext]
