(**pp -syntax camlp5o $(IMPORT_OCAMLCFLAGS) *)
module SRC = Reorg_ast.Ast_5_4
module DST = Reorg_ast.Ast_5_3

include (sig open Reorg_ast end)
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
          ]
        }
      ; {
        srcmod = Reorg_ast.Ast_5_4
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
        ; injectivity
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
          srcmod = Reorg_ast.Ast_5_4
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
      ; migrate_out_value = {
          srctype = [%typ: out_value]
        ; dsttype = [%typ: DST.out_value]
        ; custom_branches_code = function
        | Oval_printer v_0 ->
           migration_error None "cannot migrate (5.4.0 -> 5.3.0) an Oval_printer of type out_value"
        | Oval_array (v_0, v_1) ->
           (match v_1 with
              SRC.Mutable ->
               DST.Oval_array
                 (__dt__.migrate_list
                    __dt__.migrate_out_value __dt__ __inh__ v_0)
            | _ -> 
               migration_error None "cannot migrate (5.4.0 -> 5.3.0) an immutable Oval_array")

        | Oval_tuple v_0 ->
           DST.Oval_tuple
             (__dt__.migrate_list
                (fun __dt__ __inh__ (_, v_1) ->
                  (__dt__.migrate_out_value __dt__ __inh__ v_1))
                __dt__ __inh__ v_0)

        | Oval_floatarray _ ->
           migration_error None "cannot migrate (5.4.0 -> 5.3.0) an Oval_floatarray of type out_value"
        }
      ; migrate_longident_t = {
          srctype = [%typ: longident_t]
        ; dsttype = [%typ: DST.longident_t]
        ; custom_branches_code = function
    | Ldot (v_0, v_1) ->
       DST.Ldot
         (__dt__.migrate_longident_t __dt__ __inh__ v_0.txt,
          v_1.txt)

    | Lapply (v_0, v_1) ->
        DST.Lapply
          (__dt__.migrate_longident_t __dt__ __inh__ v_0.txt,
           __dt__.migrate_longident_t __dt__ __inh__ v_1.txt)
        }
      ; migrate_variance = {
          srctype = [%typ: variance]
        ; dsttype = [%typ: DST.variance]
        ; custom_branches_code = function
        | Bivariant ->
           migration_error None "cannot migrate (5.4.0 -> 5.3.0) a variance=Bivariant"
        }
      ; migrate_package_type = {
          srctype = [%typ: package_type]
        ; dsttype = [%typ: DST.package_type]
        ; code = fun __dt__ __inh__
                   {ppt_path = ppt_path; ppt_cstrs = ppt_cstrs} ->
          (__dt__.migrate_location_loc
             __dt__.migrate_longident_t
             __dt__ __inh__ ppt_path,
           __dt__.migrate_list
               (fun __dt__ __inh__ (v_0, v_1) ->
                 __dt__.migrate_location_loc
                     __dt__.migrate_longident_t
                   __dt__ __inh__ v_0,
                 __dt__.migrate_core_type
                   __dt__ __inh__ v_1)
               __dt__ __inh__ ppt_cstrs)
        }
      ; migrate_expression_desc = {
          srctype = [%typ: expression_desc]
        ; dsttype = [%typ: DST.expression_desc]
        ; custom_branches_code = function
        | Pexp_tuple v_0 ->
           DST.Pexp_tuple
             (__dt__.migrate_list
                 (fun __dt__ __inh__ (_, v_1) ->
                   __dt__.migrate_expression
                     __dt__ __inh__ v_1)
                __dt__ __inh__ v_0)
        | Pexp_pack (v_0, v_1) ->
           DST.Pexp_pack
             (match v_1 with
                Some _ ->
                 migration_error None "cannot migrate (5.4.0 -> 5.3.0) a Pexp_pack with non-empty package_type"
              | None -> 
                 (__dt__.migrate_module_expr
                    __dt__ __inh__ v_0))
        }
      ; migrate_pattern_desc = {
          srctype = [%typ: pattern_desc]
        ; dsttype = [%typ: DST.pattern_desc]
        ; custom_branches_code = function
        | Ppat_tuple (v_0, v_1) ->
           DST.Ppat_tuple
             (__dt__.migrate_list
                (fun __dt__ __inh__ (_, v_1) ->
                  __dt__.migrate_pattern
                    __dt__ __inh__ v_1)
                __dt__ __inh__ v_0)
        }
      ; migrate_core_type_desc = {
          srctype = [%typ: core_type_desc]
        ; dsttype = [%typ: DST.core_type_desc]
        ; custom_branches_code = function
        | Ptyp_tuple v_0 ->
           DST.Ptyp_tuple
             (__dt__.migrate_list
                (fun __dt__ __inh__ (_, v_1) ->
                  __dt__.migrate_core_type
                    __dt__ __inh__ v_1)
                __dt__ __inh__ v_0)
        }
      ; migrate_out_label = {
          srctype = [%typ: out_label]
        ; dsttype = [%typ: DST.out_label]
        ; skip_fields = [ olab_atomic ]
        }
      ; migrate_out_type = {
          srctype = [%typ: out_type]
        ; dsttype = [%typ: DST.out_type]
        ; custom_branches_code = function
        | Otyp_tuple v_0 ->
           DST.Otyp_tuple
             (__dt__.migrate_list
                (fun __dt__ __inh__ (_, v_1) -> (__dt__.migrate_out_type __dt__ __inh__ v_1))
                __dt__ __inh__ v_0)
        | Otyp_module v_0 ->
           let {opack_path=v_0; opack_cstrs=v_1} = v_0 in
           DST.Otyp_module           
             (__dt__.migrate_out_ident __dt__ __inh__ v_0,
              __dt__.migrate_list
                (fun __dt__ __inh__ (v_0, v_1) ->
                  (v_0, __dt__.migrate_out_type __dt__ __inh__ v_1))
                __dt__ __inh__ v_1)
        }
      }
    }
]
