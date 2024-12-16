(**pp -syntax camlp5o $(IMPORT_OCAMLCFLAGS) *)
[@@@ocaml.warning "@partial-match"]
module SRC = Reorg_ast.Ast_5_2
module DST = Reorg_ast.Ast_5_3

let src_loc_none =
  let open SRC in
  let open SRC in
  let loc = {
    pos_fname = "";
    pos_lnum = 1;
    pos_bol = 0;
    pos_cnum = -1;
  } in
  { loc_start = loc; loc_end = loc; loc_ghost = true }

let dst_loc_none =
  let open DST in
  let open DST in
  let loc = {
    pos_fname = "";
    pos_lnum = 1;
    pos_bol = 0;
    pos_cnum = -1;
  } in
  { loc_start = loc; loc_end = loc; loc_ghost = true }

let wrap_loc inh v =
  let loc = match inh with
      None -> src_loc_none
    | Some loc -> loc in
  let open SRC in
  { txt = v ; loc = loc }

let map_loc f v =
  let open SRC in
  { txt = f v.txt ; loc = v.loc }

let unwrap_loc v = v.SRC.txt

exception Migration_error of string * SRC.location_t option

let migration_error location feature =
  raise (Migration_error (feature, location))

let _migrate_list subrw0 __dt__ __inh__ l =
  List.map (subrw0 __dt__ __inh__) l

[%%import: Reorg_ast.Ast_5_2.attribute]
(*
[%%import: Migrate_520_510.attribute]
 *)
[@@deriving migrate
    { inherit_type = [%typ: location_t option]
    ; dispatch_type = dispatch_table_t
    ; dispatch_table_constructor = make_dt
    ; default_dispatchers = [
        {
          srcmod = Reorg_ast.Ast_5_2
        ; dstmod = DST
        ; types = [
            lexing_position
          ; location_t
          ; location_loc
          ; longident_t
          ]
        }
      ; {
        srcmod = Reorg_ast.Ast_5_2
      ; dstmod = DST
      ; types = [
          arg_label
        ; closed_flag
        ; direction_flag
        ; label
        ; mutable_flag
        ; override_flag
        ; private_flag
        ; rec_flag
        ; virtual_flag
        ; variance
        ; injectivity
        ]
      }
      ; {
        srcmod = Reorg_ast.Ast_5_2
      ; dstmod = DST
      ; types = [
          attribute
        ; attributes
        ; binding_op
        ; case
        ; class_declaration
        ; class_description
        ; class_expr
        ; class_expr_desc
        ; class_field
        ; class_field_desc
        ; class_field_kind
        ; class_signature
        ; class_structure
        ; class_type
        ; class_type_declaration
        ; class_type_desc
        ; class_type_field
        ; class_type_field_desc
        ; constructor_arguments
        ; constructor_declaration
        ; core_type
        ; core_type_desc
        ; expression
        ; expression_desc
        ; extension
        ; extension_constructor
        ; extension_constructor_kind
        ; function_body
        ; function_param
        ; function_param_desc
        ; functor_parameter
        ; include_declaration
        ; include_description
        ; label_declaration
        ; letop
        ; location_stack
        ; module_binding
        ; module_declaration
        ; module_expr
        ; module_expr_desc
        ; module_substitution
        ; module_type
        ; module_type_declaration
        ; module_type_desc
        ; object_field
        ; object_field_desc
        ; open_declaration
        ; open_description
        ; package_type
        ; pattern
        ; pattern_desc
        ; payload
        ; row_field
        ; row_field_desc
        ; signature
        ; signature_item
        ; signature_item_desc
        ; structure
        ; structure_item
        ; structure_item_desc
        ; type_constraint
        ; type_declaration
        ; type_exception
        ; type_extension
        ; type_immediacy_t
        ; type_kind
        ; value_binding
        ; value_constraint
        ; value_description
        ; with_constraint
        ]
      ; inherit_code = {
          class_expr = Some pcl_loc
        ; class_field = Some pcf_loc
        ; class_type_field = Some pctf_loc
        ; class_type = Some pcty_loc
        ; extension_constructor = Some pext_loc
        ; label_declaration = Some pld_loc
        ; module_expr = Some pmod_loc
        ; module_type_declaration = Some pmtd_loc
        ; module_type = Some pmty_loc
        ; signature_item = Some psig_loc
        ; structure_item = Some pstr_loc
        ; type_declaration = Some ptype_loc
        ; value_description = Some pval_loc
        }
      }
      ; {
          srcmod = Reorg_ast.Ast_5_2
        ; dstmod = DST
        ; types = [
          out_attribute
        ; out_class_sig_item
        ; out_class_type
        ; out_constructor
        ; out_extension_constructor
        ; out_ext_status
        ; out_ident
        ; out_module_type
        ; out_name
        ; out_phrase
        ; out_rec_status
        ; out_sig_item
        ; out_string
        ; out_type_decl
        ; out_type_param
        ; out_val_decl
        ; out_variant
        ]
      }
      ]
    ; dispatchers = {
        migrate_option = {
          srctype = [%typ: 'a option]
        ; dsttype = [%typ: 'b option]
        ; subs = [ ([%typ: 'a], [%typ: 'b]) ]
        ; code = (fun subrw __dt__ __inh__ x -> Option.map (subrw __dt__ __inh__) x)
        }
      ; migrate_list = {
          srctype = [%typ: 'a list]
        ; dsttype = [%typ: 'b list]
        ; code = _migrate_list
        ; subs = [ ([%typ: 'a], [%typ: 'b]) ]
        }
      ; migrate_printer = {
          srctype = [%typ: (Format.formatter -> unit)]
        ; dsttype = [%typ: (Format.formatter -> unit)]
        ; code = fun _ _ x -> x
        }
      ; migrate_exn = {
          srctype = [%typ: exn]
        ; dsttype = [%typ: exn]
        ; code = fun _ _ x -> x
        }
      ; migrate_constant = {
          srctype = [%typ: constant]
        ; dsttype = [%typ: DST.constant]
        ; code = fun __dt__ __inh__ -> function
            Pconst_integer (v1, v2) ->
            DST.{ pconst_desc = Pconst_integer (v1, v2)
                ; pconst_loc = dst_loc_none }
          | Pconst_char v1 ->
            DST.{ pconst_desc = Pconst_char v1
                ; pconst_loc = dst_loc_none }
          | Pconst_string (v1, v2, v3) ->
            let v2 = __dt__.migrate_location_t __dt__ __inh__ v2 in
            DST.{ pconst_desc = Pconst_string (v1, v2, v3)
                ; pconst_loc = dst_loc_none }
          | Pconst_float (v1, v2) ->
            DST.{ pconst_desc = Pconst_float (v1, v2)
                ; pconst_loc = dst_loc_none }
        }
      ; migrate_out_type = {
          srctype = [%typ: out_type]
        ; dsttype = [%typ: DST.out_type]
        ; custom_branches_code = function
    | Otyp_record v_0 ->
        let open DST in
        Otyp_record
          ((fun __dt__ __inh__ ->
              __dt__.migrate_list
                (fun __dt__ __inh__ (v_0, v_1, v_2) ->
                  DST.{ olab_name = v_0
                      ; olab_mut = if v_1 then Mutable else Immutable
                      ; olab_type = __dt__.migrate_out_type __dt__ __inh__ v_2
                  }
                   )
                __dt__ __inh__)
             __dt__ __inh__ v_0)
        }
      ; migrate_out_value = {
          srctype = [%typ: out_value]
        ; dsttype = [%typ: DST.out_value]
        ; custom_branches_code = function
        | Oval_printer v_0 ->
           migration_error None "cannot migrate (5.2.0 -> 5.3.0) an Oval_printer of type out_value"
        }
      }
    }
]
