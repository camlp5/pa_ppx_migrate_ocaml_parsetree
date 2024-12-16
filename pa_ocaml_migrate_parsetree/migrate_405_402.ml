(**pp -syntax camlp5o $(IMPORT_OCAMLCFLAGS) *)
[@@@ocaml.warning "@partial-match"]
module SRC = Reorg_ast.Ast_4_05
module DST = Reorg_ast.Ast_4_02

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

let unwrap_loc v = v.SRC.txt

exception Migration_error of string * SRC.location_t option

let migration_error location feature =
  raise (Migration_error (feature, location))

let _migrate_list subrw0 __dt__ __inh__ l =
  List.map (subrw0 __dt__ __inh__) l

let migrate_arg_label_label :
  'a -> 'b -> SRC.arg_label -> DST.label
  =
  fun __dt__ __inh__ -> function
  | SRC.Nolabel  -> ""
  | SRC.Labelled x0 -> x0
  | SRC.Optional x0 -> "?" ^ x0

let migrate_Parsetree_constant_Asttypes_constant :
  'a -> SRC.location_t option -> SRC.constant -> DST.constant =
  fun __dt__ __inh__ -> function
  | SRC.Pconst_integer (x0,x1) ->
     begin match x1 with
     | None -> DST.Const_int (int_of_string x0)
     | Some 'l' ->
         DST.Const_int32 (Int32.of_string x0)
     | Some 'L' ->
         DST.Const_int64 (Int64.of_string x0)
     | Some 'n' ->
         DST.Const_nativeint (Nativeint.of_string x0)
     | Some _ -> migration_error __inh__ "Pconst_integer"
     end
  | SRC.Pconst_char x0 ->
      DST.Const_char x0
  | SRC.Pconst_string (x0,x1) ->
      DST.Const_string (x0,x1)
  | SRC.Pconst_float (x0,x1) ->
      begin match x1 with
      | None -> DST.Const_float x0
      | Some _ -> migration_error __inh__ "Pconst_float"
      end

[%%import: Migrate_405_402.attribute]
[@@deriving migrate
    { inherit_type = [%typ: location_t option]
    ; dispatch_type = dispatch_table_t
    ; dispatch_table_constructor = make_dt
    ; default_dispatchers = [
        {
          srcmod = Reorg_ast.Ast_4_05
        ; dstmod = DST
        ; types = [
            lexing_position
          ; location_t
          ; location_loc
          ; longident_t
          ]
        }
      ; {
        srcmod = Reorg_ast.Ast_4_05
      ; dstmod = DST
      ; types = [
          closed_flag
        ; direction_flag
        ; label
        ; mutable_flag
        ; override_flag
        ; private_flag
        ; rec_flag
        ; variance
        ; virtual_flag
        ]
      }
      ; {
        srcmod = Reorg_ast.Ast_4_05
      ; dstmod = DST
      ; types = [
          attribute
        ; attributes
        ; case
        ; class_declaration
        ; class_description
        ; class_expr
        ; class_expr_desc
        ; class_field
        ; class_field_kind
        ; class_infos
        ; class_signature
        ; class_structure
        ; class_type
        ; class_type_declaration
        ; class_type_desc
        ; class_type_field
        ; constructor_declaration
        ; core_type
        ; expression
        ; extension
        ; extension_constructor
        ; extension_constructor_kind
        ; include_declaration
        ; include_description
        ; include_infos
        ; label_declaration
        ; location_stack
        ; module_binding
        ; module_declaration
        ; module_expr
        ; module_expr_desc
        ; module_type
        ; module_type_declaration
        ; module_type_desc
        ; open_description
        ; package_type
        ; pattern
        ; row_field
        ; signature
        ; signature_item
        ; structure
        ; structure_item
        ; type_declaration
        ; type_extension
        ; type_kind
        ; value_binding
        ; value_description
        ; with_constraint
        ]
      ; inherit_code = {
          class_expr = Some pcl_loc
        ; class_field = Some pcf_loc
        ; class_infos = Some pci_loc
        ; class_type_field = Some pctf_loc
        ; class_type = Some pcty_loc
        ; constructor_declaration = Some pcd_loc
        ; core_type = Some ptyp_loc
        ; expression = Some pexp_loc
        ; extension_constructor = Some pext_loc
        ; include_infos = Some pincl_loc
        ; label_declaration = Some pld_loc
        ; module_binding = Some pmb_loc
        ; module_declaration = Some pmd_loc
        ; module_expr = Some pmod_loc
        ; module_type_declaration = Some pmtd_loc
        ; module_type = Some pmty_loc
        ; open_description = Some popen_loc
        ; pattern = Some ppat_loc
        ; signature_item = Some psig_loc
        ; structure_item = Some pstr_loc
        ; type_declaration = Some ptype_loc
        ; value_binding = Some pvb_loc
        ; value_description = Some pval_loc
        }
      }
      ; {
        srcmod = Reorg_ast.Ast_4_05
      ; dstmod = DST
      ; types = [
          out_class_sig_item
        ; out_class_type
        ; out_extension_constructor
        ; out_ext_status
        ; out_ident
        ; out_module_type
        ; out_phrase
        ; out_rec_status
        ; out_type_extension
        ; out_value
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
      ; migrate_arg_label = {
          srctype = [%typ: arg_label]
        ; dsttype = [%typ: DST.label]
        ; code = migrate_arg_label_label
        }
      ; migrate_constant = {
          srctype = [%typ: constant]
        ; dsttype = [%typ: DST.constant]
        ; code = migrate_Parsetree_constant_Asttypes_constant
        }
      ; migrate_list = {
          srctype = [%typ: 'a list]
        ; dsttype = [%typ: 'b list]
        ; code = _migrate_list
        ; subs = [ ([%typ: 'a], [%typ: 'b]) ]
        }
      ; migrate_payload = {
          srctype = [%typ: payload]
        ; dsttype = [%typ: DST.payload]
        ; custom_branches_code = function
              PSig _x0 ->
              migration_error __inh__ "PSig"
        }
      ; migrate_core_type_desc = {
          srctype = [%typ: core_type_desc]
        ; dsttype = [%typ: DST.core_type_desc]
        ; custom_branches_code = function
            | Ptyp_object (v_0, v_1) ->
              let open DST in
              Ptyp_object
                ((fun __dt__ __inh__ ->
                    __dt__.migrate_list
                      (fun __dt__ __inh__ (v_0, v_1, v_2) ->
                         unwrap_loc v_0,
                         __dt__.migrate_attributes __dt__ __inh__ v_1,
                         __dt__.migrate_core_type __dt__ __inh__ v_2)
                      __dt__ __inh__)
                   __dt__ __inh__ v_0,
                 __dt__.migrate_closed_flag __dt__ __inh__ v_1)
            | Ptyp_poly (v_0, v_1) ->
              let open DST in
              Ptyp_poly
                (List.map unwrap_loc v_0,
                 __dt__.migrate_core_type __dt__ __inh__ v_1)
        }
      ; migrate_pattern_desc = {
          srctype = [%typ: pattern_desc]
        ; dsttype = [%typ: DST.pattern_desc]
        ; custom_branches_code = function
              Ppat_open _ -> migration_error __inh__ "Ppat_open"
        }
      ; migrate_expression_desc = {
          srctype = [%typ: expression_desc]
        ; dsttype = [%typ: DST.expression_desc]
        ; custom_branches_code = function
            | Pexp_letexception _ -> migration_error __inh__ "Pexp_letexception"
            | Pexp_unreachable  ->
              migration_error __inh__ "Pexp_unreachable"
            | Pexp_send (v_0, v_1) ->
              let open DST in
              Pexp_send
                (__dt__.migrate_expression __dt__ __inh__ v_0,
                 unwrap_loc v_1)
            | Pexp_newtype (v_0, v_1) ->
              let open DST in
              Pexp_newtype
                (unwrap_loc v_0,
                 __dt__.migrate_expression __dt__ __inh__ v_1)
        }
      ; migrate_constructor_arguments = {
          srctype = [%typ: constructor_arguments]
        ; dsttype = [%typ: DST.core_type list]
        ; custom_branches_code = function
              Pcstr_tuple pcd_args ->
              List.map (__dt__.migrate_core_type __dt__ __inh__) pcd_args
            | Pcstr_record _ -> migration_error __inh__ "Pcstr_record"
        }
      ; migrate_class_type_field_desc = {
          srctype = [%typ: class_type_field_desc]
        ; dsttype = [%typ: DST.class_type_field_desc]
        ; custom_branches_code = function
            | Pctf_val v_0 ->
              let open DST in
              Pctf_val
                ((fun (v_0, v_1, v_2, v_3) ->
                    unwrap_loc v_0,
                    __dt__.migrate_mutable_flag __dt__ __inh__ v_1,
                    __dt__.migrate_virtual_flag __dt__ __inh__ v_2,
                    __dt__.migrate_core_type __dt__ __inh__ v_3) v_0)
            | Pctf_method v_0 ->
              let open DST in
              Pctf_method
                ((fun (v_0, v_1, v_2, v_3) ->
                    unwrap_loc v_0,
                    __dt__.migrate_private_flag __dt__ __inh__ v_1,
                    __dt__.migrate_virtual_flag __dt__ __inh__ v_2,
                    __dt__.migrate_core_type __dt__ __inh__ v_3) v_0)
        }
      ; migrate_class_field_desc = {
          srctype = [%typ: class_field_desc]
        ; dsttype = [%typ: DST.class_field_desc]
        ; custom_branches_code = function
              Pcf_inherit (v_0, v_1, v_2) ->
              let open DST in
              Pcf_inherit
                (__dt__.migrate_override_flag __dt__ __inh__ v_0,
                 __dt__.migrate_class_expr __dt__ __inh__ v_1,
                 Option.map unwrap_loc v_2)
        }
      ; migrate_signature_item_desc = {
          srctype = [%typ: signature_item_desc]
        ; dsttype = [%typ: DST.signature_item_desc]
        ; custom_branches_code = function
              Psig_type (Recursive, v_0) ->
              Psig_type (List.map (__dt__.migrate_type_declaration __dt__ __inh__) v_0)
            | Psig_type (Nonrecursive, []) -> Psig_type []
            | Psig_type (Nonrecursive, h::t) ->
              let h = { h with ptype_attributes = ({txt="nonrec"; loc=src_loc_none}, PStr[]) :: h.ptype_attributes } in
              Psig_type (List.map (__dt__.migrate_type_declaration __dt__ __inh__) (h::t))
        }
      ; migrate_structure_item_desc = {
          srctype = [%typ: structure_item_desc]
        ; dsttype = [%typ: DST.structure_item_desc]
        ; custom_branches_code = function
              Pstr_type (Recursive, v_0) ->
              Pstr_type (List.map (__dt__.migrate_type_declaration __dt__ __inh__) v_0)
            | Pstr_type (Nonrecursive, []) -> Pstr_type []
            | Pstr_type (Nonrecursive, h::t) ->
              let h = { h with ptype_attributes = ({txt="nonrec"; loc=src_loc_none}, PStr[]) :: h.ptype_attributes } in
              Pstr_type (List.map (__dt__.migrate_type_declaration __dt__ __inh__) (h::t))
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
      ; migrate_out_type = {
          srctype = [%typ: out_type]
        ; dsttype = [%typ: DST.out_type]
        ; custom_branches_code = function
              Otyp_attribute _ -> migration_error __inh__ "Otyp_attribute"
        }
      ; migrate_out_variant = {
          srctype = [%typ: out_variant]
        ; dsttype = [%typ: DST.out_variant]
        ; custom_branches_code = function
            | Ovar_typ (Otyp_constr (id,tyl)) ->
              Ovar_name (__dt__.migrate_out_ident __dt__ __inh__ id,
                         List.map (__dt__.migrate_out_type __dt__ __inh__) tyl)
            | Ovar_typ x0 ->
              Ovar_name
                (Oide_ident "", [__dt__.migrate_out_type __dt__ __inh__ x0])
        }
      ; migrate_out_sig_item = {
          srctype = [%typ: out_sig_item]
        ; dsttype = [%typ: DST.out_sig_item]
        ; custom_branches_code = function
              Osig_value ovd ->
              let open DST in
              Osig_value
                (ovd.oval_name,
                 __dt__.migrate_out_type __dt__ __inh__ ovd.oval_type,
                 ovd.oval_prims)
            | Osig_ellipsis -> migration_error __inh__ "Osig_ellipsis"
        }
      ; migrate_out_type_decl = {
          srctype = [%typ: out_type_decl]
        ; dsttype = [%typ: DST.out_type_decl]
        ; skip_fields = [ otype_unboxed; otype_immediate ]
        }
      }
    }
]
