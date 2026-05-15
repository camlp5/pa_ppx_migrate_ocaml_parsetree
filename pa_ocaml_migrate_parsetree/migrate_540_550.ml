(**pp -syntax camlp5o $(IMPORT_OCAMLCFLAGS) *)
[@@@ocaml.warning "@partial-match"]
module SRC = Reorg_ast.Ast_5_4
module DST = Reorg_ast.Ast_5_5

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

module Format_doc = Reorg_ast.Ast_5_4.Format_doc
[%%typedecls
  [%%import: Reorg_ast.Ast_5_4.attribute]
]
[@@deriving migrate
    { inherit_type = [%typ: SRC.location_t option]
    ; dispatch_type = dispatch_table_t
    ; dispatch_table_constructor = make_dt
    ; default_dispatchers = [
        {
          srcmod = Reorg_ast.Ast_5_4
        ; dstmod = DST
        ; types = [
            lexing_position
          ; location_t
          ; location_loc
          ; longident_t
          ]
        }
      ; {
        srcmod = Reorg_ast.Ast_5_4
      ; dstmod = DST
      ; types = [
          arg_label
        ; atomic_flag
        ; closed_flag
        ; direction_flag
        ; label
        ; mutable_flag
        ; override_flag
        ; private_flag
        ; rec_flag
        ; virtual_flag
        ; injectivity
        ; variance
        ]
      }
      ; {
        srcmod = Reorg_ast.Ast_5_4
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
        ; constant
        ; constant_desc
        ; constructor_arguments
        ; constructor_declaration
        ; core_type
        ; core_type_desc
        ; expression
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
        ; pattern
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
        ; value_description = Some pval_loc
        }
      }
      ; {
          srcmod = Reorg_ast.Ast_5_4
        ; dstmod = DST
        ; types = [
            out_attribute
          ; out_class_sig_item
          ; out_class_type
          ; out_constructor
          ; out_ext_status
          ; out_ident
          ; out_label
          ; out_module_type
          ; out_name
          ; out_phrase
          ; out_rec_status
          ; out_sig_item
          ; out_string
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
      ; migrate_floatarray = {
          srctype = [%typ: floatarray]
        ; dsttype = [%typ: floatarray]
        ; code = (fun __dt__ __inh__ x -> x)
        }
      ; migrate_out_value = {
          srctype = [%typ: out_value]
        ; dsttype = [%typ: DST.out_value]
        ; custom_branches_code = function
        | Oval_printer v_0 ->
           migration_error None "cannot migrate (5.4.0 -> 5.5.0) an Oval_printer of type out_value"
        }
      ; migrate_out_extension_constructor = {
          srctype = [%typ: out_extension_constructor]
        ; dsttype = [%typ: DST.out_extension_constructor]
        ; skip_fields = [ oext_type_params ]
        ; custom_fields_code = {
            oext_type_params =
              let migr1 s = 
                DST.{ ot_non_gen = false
                    ; ot_name = s
                    ; ot_variance = (NoVariance,NoInjectivity)
                } in
              List.map migr1 oext_type_params
          }
        }
      ; migrate_out_package = {
          srctype = [%typ: out_package]
        ; dsttype = [%typ: DST.out_package]
        ; skip_fields = [ opack_cstrs ]
        ; custom_fields_code = {
            opack_constraints =
              __dt__.migrate_list
                (fun __dt__ __inh__ (v_0, v_1) ->
                  v_0,
                  __dt__.migrate_out_type __dt__ __inh__ v_1)
                __dt__ __inh__ opack_cstrs
          }
        }
      ; migrate_out_type = {
          srctype = [%typ: out_type]
        ; dsttype = [%typ: DST.out_type]
        ; custom_branches_code = function
        | Otyp_object {fields = fields; open_row = open_row} ->
           let row =
             if open_row then
               DST.Orow_open_anonymous
             else DST.Orow_closed in
           DST.Otyp_object
             {fields =
                __dt__.migrate_list
                  (fun __dt__ __inh__ (v_0, v_1) ->
                    v_0,
                    __dt__.migrate_out_type __dt__ __inh__ v_1)
                  __dt__ __inh__ fields;
              row = row}
        }
      ; migrate_package_type = {
          srctype = [%typ: package_type]
        ; dsttype = [%typ: DST.package_type]
        ; skip_fields = [ ppt_cstrs ]
        ; custom_fields_code = {
            ppt_constraints =
              __dt__.migrate_list
                (fun __dt__ __inh__ (v_0, v_1) ->
                  __dt__.migrate_location_loc
                    __dt__.migrate_longident_t
                    __dt__ __inh__ v_0,
                  __dt__.migrate_core_type __dt__ __inh__ v_1)
                __dt__ __inh__ ppt_cstrs
          }
        }
      ; migrate_out_type_decl = {
          srctype = [%typ: out_type_decl]
        ; dsttype = [%typ: DST.out_type_decl]
        ; skip_fields = [ otype_cstrs ]
        ; custom_fields_code = {
            otype_constraints =
              (fun __dt__ __inh__ ->
                __dt__.migrate_list
                  (fun __dt__ __inh__ (v_0, v_1) ->
                    __dt__.migrate_out_type __dt__ __inh__ v_0,
                    __dt__.migrate_out_type __dt__ __inh__ v_1)
                  __dt__ __inh__)
                __dt__ __inh__ otype_cstrs
          }
        }
      ; migrate_type_declaration = {
          srctype = [%typ: type_declaration]
        ; dsttype = [%typ: DST.type_declaration]
        ; skip_fields = [ ptype_cstrs ]
        ; custom_fields_code = {
            ptype_constraints =
              __dt__.migrate_list
                (fun __dt__ __inh__ (v_0, v_1, v_2) ->
                  __dt__.migrate_core_type __dt__ __inh__ v_0,
                  __dt__.migrate_core_type __dt__ __inh__ v_1,
                  __dt__.migrate_location_t __dt__ __inh__ v_2)
                __dt__ __inh__ ptype_cstrs
          }
        }
      ; migrate_expression_desc = {
          srctype = [%typ: expression_desc]
        ; dsttype = [%typ: DST.expression_desc]
        ; custom_branches_code = function
        | Pexp_letmodule (v_0, v_1, v_2) ->
           let v_0 =
             __dt__.migrate_location_loc (fun __dt__ __inh__ x -> x)
               __dt__ __inh__ v_0 in
           let loc = v_0.DST.loc in
           let v_1 = __dt__.migrate_module_expr __dt__ __inh__ v_1 in
           let v_2 = __dt__.migrate_expression __dt__ __inh__ v_2 in
           let mb = DST.{
                 pmb_name = v_0
               ; pmb_expr = v_1
               ; pmb_attributes = []
               ; pmb_loc = loc
                    } in
           let sidesc = DST.Pstr_module mb in
           let si = DST.{
                 pstr_desc = sidesc
               ; pstr_loc = loc
                    } in
           DST.Pexp_struct_item(si, v_2)

        | Pexp_letexception (v_0, v_1) ->
           let v_0 = __dt__.migrate_extension_constructor __dt__ __inh__ v_0 in
           let loc = v_0.DST.pext_loc in
           let te = DST.{
                 ptyexn_constructor = v_0
               ; ptyexn_loc = loc
               ; ptyexn_attributes = []
                    } in
           let sidesc = DST.Pstr_exception te in
           let si = DST.{ 
                 pstr_desc = sidesc
               ; pstr_loc = loc
                     } in
           let v_1 = __dt__.migrate_expression __dt__ __inh__ v_1 in
           DST.Pexp_struct_item(si, v_1)
        | Pexp_open (v_0, v_1) ->
           let v_0 = __dt__.migrate_open_declaration __dt__ __inh__ v_0 in
           let loc = v_0.DST.popen_loc in
           let sidesc = DST.Pstr_open v_0 in
           let si = DST.{ 
                 pstr_desc = sidesc
               ; pstr_loc = loc
                     } in
           let v_1 = __dt__.migrate_expression __dt__ __inh__ v_1 in
           DST.Pexp_struct_item(si, v_1)
        }
      ; migrate_pattern_desc = {
          srctype = [%typ: pattern_desc]
        ; dsttype = [%typ: DST.pattern_desc]
        ; custom_branches_code = function
        | Ppat_unpack v_0 ->
           DST.Ppat_unpack
             (__dt__.migrate_location_loc
                (__dt__.migrate_option (fun __dt__ __inh__ x -> x))
                __dt__ __inh__ v_0,
              None)
        }
      }
    }
]
