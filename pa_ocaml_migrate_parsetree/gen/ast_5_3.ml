module Lexing =
  struct
    type position =
      { pos_fname : string; pos_lnum : int; pos_bol : int; pos_cnum : int }
  end
module Warnings =
  struct
    type loc =
      { loc_start : Lexing.position;
        loc_end : Lexing.position;
        loc_ghost : bool }
  end
module Location =
  struct
    type t =
      Warnings.loc =
        { loc_start : Lexing.position;
          loc_end : Lexing.position;
          loc_ghost : bool }
    type 'a loc = { txt : 'a; loc : t }
  end
module Longident =
  struct
    type t =
        Lident of string
      | Ldot of t * string
      | Lapply of t * t
  end
module Asttypes =
  struct
    type 'a loc = 'a Location.loc = { txt : 'a; loc : Location.t }
    type arg_label =
        Nolabel
      | Labelled of string
      | Optional of string
    type label = string
    type closed_flag = Closed | Open
    type rec_flag = Nonrecursive | Recursive
    type direction_flag = Upto | Downto
    type private_flag = Private | Public
    type mutable_flag = Immutable | Mutable
    type virtual_flag = Virtual | Concrete
    type override_flag = Override | Fresh
    type variance = Covariant | Contravariant | NoVariance
    type injectivity = Injective | NoInjectivity
  end
module Parsetree =
  struct
    open Asttypes
    type constant = { pconst_desc : constant_desc; pconst_loc : Location.t }
    and constant_desc =
        Pconst_integer of string * char option
      | Pconst_char of char
      | Pconst_string of string * Location.t * string option
      | Pconst_float of string * char option
    type location_stack = Location.t list
    type attribute =
      { attr_name : string loc;
        attr_payload : payload;
        attr_loc : Location.t }
    and extension = string loc * payload
    and attributes = attribute list
    and payload =
        PStr of structure
      | PSig of signature
      | PTyp of core_type
      | PPat of pattern * expression option
    and core_type =
      { ptyp_desc : core_type_desc;
        ptyp_loc : Location.t;
        ptyp_loc_stack : location_stack;
        ptyp_attributes : attributes }
    and core_type_desc =
        Ptyp_any
      | Ptyp_var of string
      | Ptyp_arrow of arg_label * core_type * core_type
      | Ptyp_tuple of core_type list
      | Ptyp_constr of Longident.t loc * core_type list
      | Ptyp_object of object_field list * closed_flag
      | Ptyp_class of Longident.t loc * core_type list
      | Ptyp_alias of core_type * string loc
      | Ptyp_variant of row_field list * closed_flag * label list option
      | Ptyp_poly of string loc list * core_type
      | Ptyp_package of package_type
      | Ptyp_open of Longident.t loc * core_type
      | Ptyp_extension of extension
    and package_type = Longident.t loc * (Longident.t loc * core_type) list
    and row_field =
      { prf_desc : row_field_desc;
        prf_loc : Location.t;
        prf_attributes : attributes }
    and row_field_desc =
        Rtag of label loc * bool * core_type list
      | Rinherit of core_type
    and object_field =
      { pof_desc : object_field_desc;
        pof_loc : Location.t;
        pof_attributes : attributes }
    and object_field_desc =
        Otag of label loc * core_type
      | Oinherit of core_type
    and pattern =
      { ppat_desc : pattern_desc;
        ppat_loc : Location.t;
        ppat_loc_stack : location_stack;
        ppat_attributes : attributes }
    and pattern_desc =
        Ppat_any
      | Ppat_var of string loc
      | Ppat_alias of pattern * string loc
      | Ppat_constant of constant
      | Ppat_interval of constant * constant
      | Ppat_tuple of pattern list
      | Ppat_construct of Longident.t loc * (string loc list * pattern) option
      | Ppat_variant of label * pattern option
      | Ppat_record of (Longident.t loc * pattern) list * closed_flag
      | Ppat_array of pattern list
      | Ppat_or of pattern * pattern
      | Ppat_constraint of pattern * core_type
      | Ppat_type of Longident.t loc
      | Ppat_lazy of pattern
      | Ppat_unpack of string option loc
      | Ppat_exception of pattern
      | Ppat_effect of pattern * pattern
      | Ppat_extension of extension
      | Ppat_open of Longident.t loc * pattern
    and expression =
      { pexp_desc : expression_desc;
        pexp_loc : Location.t;
        pexp_loc_stack : location_stack;
        pexp_attributes : attributes }
    and expression_desc =
        Pexp_ident of Longident.t loc
      | Pexp_constant of constant
      | Pexp_let of rec_flag * value_binding list * expression
      | Pexp_function of
          function_param list * type_constraint option * function_body
      | Pexp_apply of expression * (arg_label * expression) list
      | Pexp_match of expression * case list
      | Pexp_try of expression * case list
      | Pexp_tuple of expression list
      | Pexp_construct of Longident.t loc * expression option
      | Pexp_variant of label * expression option
      | Pexp_record of (Longident.t loc * expression) list * expression option
      | Pexp_field of expression * Longident.t loc
      | Pexp_setfield of expression * Longident.t loc * expression
      | Pexp_array of expression list
      | Pexp_ifthenelse of expression * expression * expression option
      | Pexp_sequence of expression * expression
      | Pexp_while of expression * expression
      | Pexp_for of
          pattern * expression * expression * direction_flag * expression
      | Pexp_constraint of expression * core_type
      | Pexp_coerce of expression * core_type option * core_type
      | Pexp_send of expression * label loc
      | Pexp_new of Longident.t loc
      | Pexp_setinstvar of label loc * expression
      | Pexp_override of (label loc * expression) list
      | Pexp_letmodule of string option loc * module_expr * expression
      | Pexp_letexception of extension_constructor * expression
      | Pexp_assert of expression
      | Pexp_lazy of expression
      | Pexp_poly of expression * core_type option
      | Pexp_object of class_structure
      | Pexp_newtype of string loc * expression
      | Pexp_pack of module_expr
      | Pexp_open of open_declaration * expression
      | Pexp_letop of letop
      | Pexp_extension of extension
      | Pexp_unreachable
    and case =
      { pc_lhs : pattern; pc_guard : expression option; pc_rhs : expression }
    and letop =
      { let_ : binding_op; ands : binding_op list; body : expression }
    and binding_op =
      { pbop_op : string loc;
        pbop_pat : pattern;
        pbop_exp : expression;
        pbop_loc : Location.t }
    and function_param_desc =
        Pparam_val of arg_label * expression option * pattern
      | Pparam_newtype of string loc
    and function_param =
      { pparam_loc : Location.t; pparam_desc : function_param_desc }
    and function_body =
        Pfunction_body of expression
      | Pfunction_cases of case list * Location.t * attributes
    and type_constraint =
        Pconstraint of core_type
      | Pcoerce of core_type option * core_type
    and value_description =
      { pval_name : string loc;
        pval_type : core_type;
        pval_prim : string list;
        pval_attributes : attributes;
        pval_loc : Location.t }
    and type_declaration =
      { ptype_name : string loc;
        ptype_params : (core_type * (variance * injectivity)) list;
        ptype_cstrs : (core_type * core_type * Location.t) list;
        ptype_kind : type_kind;
        ptype_private : private_flag;
        ptype_manifest : core_type option;
        ptype_attributes : attributes;
        ptype_loc : Location.t }
    and type_kind =
        Ptype_abstract
      | Ptype_variant of constructor_declaration list
      | Ptype_record of label_declaration list
      | Ptype_open
    and label_declaration =
      { pld_name : string loc;
        pld_mutable : mutable_flag;
        pld_type : core_type;
        pld_loc : Location.t;
        pld_attributes : attributes }
    and constructor_declaration =
      { pcd_name : string loc;
        pcd_vars : string loc list;
        pcd_args : constructor_arguments;
        pcd_res : core_type option;
        pcd_loc : Location.t;
        pcd_attributes : attributes }
    and constructor_arguments =
        Pcstr_tuple of core_type list
      | Pcstr_record of label_declaration list
    and type_extension =
      { ptyext_path : Longident.t loc;
        ptyext_params : (core_type * (variance * injectivity)) list;
        ptyext_constructors : extension_constructor list;
        ptyext_private : private_flag;
        ptyext_loc : Location.t;
        ptyext_attributes : attributes }
    and extension_constructor =
      { pext_name : string loc;
        pext_kind : extension_constructor_kind;
        pext_loc : Location.t;
        pext_attributes : attributes }
    and type_exception =
      { ptyexn_constructor : extension_constructor;
        ptyexn_loc : Location.t;
        ptyexn_attributes : attributes }
    and extension_constructor_kind =
        Pext_decl of
          string loc list * constructor_arguments * core_type option
      | Pext_rebind of Longident.t loc
    and class_type =
      { pcty_desc : class_type_desc;
        pcty_loc : Location.t;
        pcty_attributes : attributes }
    and class_type_desc =
        Pcty_constr of Longident.t loc * core_type list
      | Pcty_signature of class_signature
      | Pcty_arrow of arg_label * core_type * class_type
      | Pcty_extension of extension
      | Pcty_open of open_description * class_type
    and class_signature =
      { pcsig_self : core_type; pcsig_fields : class_type_field list }
    and class_type_field =
      { pctf_desc : class_type_field_desc;
        pctf_loc : Location.t;
        pctf_attributes : attributes }
    and class_type_field_desc =
        Pctf_inherit of class_type
      | Pctf_val of (label loc * mutable_flag * virtual_flag * core_type)
      | Pctf_method of (label loc * private_flag * virtual_flag * core_type)
      | Pctf_constraint of (core_type * core_type)
      | Pctf_attribute of attribute
      | Pctf_extension of extension
    and 'a class_infos =
      { pci_virt : virtual_flag;
        pci_params : (core_type * (variance * injectivity)) list;
        pci_name : string loc;
        pci_expr : 'a;
        pci_loc : Location.t;
        pci_attributes : attributes }
    and class_description = class_type class_infos
    and class_type_declaration = class_type class_infos
    and class_expr =
      { pcl_desc : class_expr_desc;
        pcl_loc : Location.t;
        pcl_attributes : attributes }
    and class_expr_desc =
        Pcl_constr of Longident.t loc * core_type list
      | Pcl_structure of class_structure
      | Pcl_fun of arg_label * expression option * pattern * class_expr
      | Pcl_apply of class_expr * (arg_label * expression) list
      | Pcl_let of rec_flag * value_binding list * class_expr
      | Pcl_constraint of class_expr * class_type
      | Pcl_extension of extension
      | Pcl_open of open_description * class_expr
    and class_structure =
      { pcstr_self : pattern; pcstr_fields : class_field list }
    and class_field =
      { pcf_desc : class_field_desc;
        pcf_loc : Location.t;
        pcf_attributes : attributes }
    and class_field_desc =
        Pcf_inherit of override_flag * class_expr * string loc option
      | Pcf_val of (label loc * mutable_flag * class_field_kind)
      | Pcf_method of (label loc * private_flag * class_field_kind)
      | Pcf_constraint of (core_type * core_type)
      | Pcf_initializer of expression
      | Pcf_attribute of attribute
      | Pcf_extension of extension
    and class_field_kind =
        Cfk_virtual of core_type
      | Cfk_concrete of override_flag * expression
    and class_declaration = class_expr class_infos
    and module_type =
      { pmty_desc : module_type_desc;
        pmty_loc : Location.t;
        pmty_attributes : attributes }
    and module_type_desc =
        Pmty_ident of Longident.t loc
      | Pmty_signature of signature
      | Pmty_functor of functor_parameter * module_type
      | Pmty_with of module_type * with_constraint list
      | Pmty_typeof of module_expr
      | Pmty_extension of extension
      | Pmty_alias of Longident.t loc
    and functor_parameter =
        Unit
      | Named of string option loc * module_type
    and signature = signature_item list
    and signature_item =
      { psig_desc : signature_item_desc; psig_loc : Location.t }
    and signature_item_desc =
        Psig_value of value_description
      | Psig_type of rec_flag * type_declaration list
      | Psig_typesubst of type_declaration list
      | Psig_typext of type_extension
      | Psig_exception of type_exception
      | Psig_module of module_declaration
      | Psig_modsubst of module_substitution
      | Psig_recmodule of module_declaration list
      | Psig_modtype of module_type_declaration
      | Psig_modtypesubst of module_type_declaration
      | Psig_open of open_description
      | Psig_include of include_description
      | Psig_class of class_description list
      | Psig_class_type of class_type_declaration list
      | Psig_attribute of attribute
      | Psig_extension of extension * attributes
    and module_declaration =
      { pmd_name : string option loc;
        pmd_type : module_type;
        pmd_attributes : attributes;
        pmd_loc : Location.t }
    and module_substitution =
      { pms_name : string loc;
        pms_manifest : Longident.t loc;
        pms_attributes : attributes;
        pms_loc : Location.t }
    and module_type_declaration =
      { pmtd_name : string loc;
        pmtd_type : module_type option;
        pmtd_attributes : attributes;
        pmtd_loc : Location.t }
    and 'a open_infos =
      { popen_expr : 'a;
        popen_override : override_flag;
        popen_loc : Location.t;
        popen_attributes : attributes }
    and open_description = Longident.t loc open_infos
    and open_declaration = module_expr open_infos
    and 'a include_infos =
      { pincl_mod : 'a;
        pincl_loc : Location.t;
        pincl_attributes : attributes }
    and include_description = module_type include_infos
    and include_declaration = module_expr include_infos
    and with_constraint =
        Pwith_type of Longident.t loc * type_declaration
      | Pwith_module of Longident.t loc * Longident.t loc
      | Pwith_modtype of Longident.t loc * module_type
      | Pwith_modtypesubst of Longident.t loc * module_type
      | Pwith_typesubst of Longident.t loc * type_declaration
      | Pwith_modsubst of Longident.t loc * Longident.t loc
    and module_expr =
      { pmod_desc : module_expr_desc;
        pmod_loc : Location.t;
        pmod_attributes : attributes }
    and module_expr_desc =
        Pmod_ident of Longident.t loc
      | Pmod_structure of structure
      | Pmod_functor of functor_parameter * module_expr
      | Pmod_apply of module_expr * module_expr
      | Pmod_apply_unit of module_expr
      | Pmod_constraint of module_expr * module_type
      | Pmod_unpack of expression
      | Pmod_extension of extension
    and structure = structure_item list
    and structure_item =
      { pstr_desc : structure_item_desc; pstr_loc : Location.t }
    and structure_item_desc =
        Pstr_eval of expression * attributes
      | Pstr_value of rec_flag * value_binding list
      | Pstr_primitive of value_description
      | Pstr_type of rec_flag * type_declaration list
      | Pstr_typext of type_extension
      | Pstr_exception of type_exception
      | Pstr_module of module_binding
      | Pstr_recmodule of module_binding list
      | Pstr_modtype of module_type_declaration
      | Pstr_open of open_declaration
      | Pstr_class of class_declaration list
      | Pstr_class_type of class_type_declaration list
      | Pstr_include of include_declaration
      | Pstr_attribute of attribute
      | Pstr_extension of extension * attributes
    and value_constraint =
        Pvc_constraint of
          { locally_abstract_univars : string loc list; typ : core_type }
      | Pvc_coercion of { ground : core_type option; coercion : core_type }
    and value_binding =
      { pvb_pat : pattern;
        pvb_expr : expression;
        pvb_constraint : value_constraint option;
        pvb_attributes : attributes;
        pvb_loc : Location.t }
    and module_binding =
      { pmb_name : string option loc;
        pmb_expr : module_expr;
        pmb_attributes : attributes;
        pmb_loc : Location.t }
  end
module Type_immediacy =
  struct
    type t = Unknown | Always | Always_on_64bits
  end
module Outcometree =
  struct
    type out_name = { mutable printed_name : string }
    type out_ident =
        Oide_apply of out_ident * out_ident
      | Oide_dot of out_ident * string
      | Oide_ident of out_name
    type out_string = Ostr_string | Ostr_bytes
    type out_attribute = { oattr_name : string }
    type out_value =
        Oval_array of out_value list
      | Oval_char of char
      | Oval_constr of out_ident * out_value list
      | Oval_ellipsis
      | Oval_float of float
      | Oval_int of int
      | Oval_int32 of int32
      | Oval_int64 of int64
      | Oval_nativeint of nativeint
      | Oval_list of out_value list
      | Oval_printer of (Format_doc.formatter -> unit)
      | Oval_record of (out_ident * out_value) list
      | Oval_string of string * int * out_string
      | Oval_stuff of string
      | Oval_tuple of out_value list
      | Oval_variant of string * out_value option
      | Oval_lazy of out_value
    type out_type_param =
      { ot_non_gen : bool;
        ot_name : string;
        ot_variance : Asttypes.variance * Asttypes.injectivity }
    type out_type =
        Otyp_abstract
      | Otyp_open
      | Otyp_alias of { non_gen : bool; aliased : out_type; alias : string }
      | Otyp_arrow of Asttypes.arg_label * out_type * out_type
      | Otyp_class of out_ident * out_type list
      | Otyp_constr of out_ident * out_type list
      | Otyp_manifest of out_type * out_type
      | Otyp_object of { fields : (string * out_type) list; open_row : bool }
      | Otyp_record of out_label list
      | Otyp_stuff of string
      | Otyp_sum of out_constructor list
      | Otyp_tuple of out_type list
      | Otyp_var of bool * string
      | Otyp_variant of out_variant * bool * string list option
      | Otyp_poly of string list * out_type
      | Otyp_module of out_ident * (string * out_type) list
      | Otyp_attribute of out_type * out_attribute
    and out_label =
      { olab_name : string;
        olab_mut : Asttypes.mutable_flag;
        olab_type : out_type }
    and out_constructor =
      { ocstr_name : string;
        ocstr_args : out_type list;
        ocstr_return_type : out_type option }
    and out_variant =
        Ovar_fields of (string * bool * out_type list) list
      | Ovar_typ of out_type
    type out_class_type =
        Octy_constr of out_ident * out_type list
      | Octy_arrow of Asttypes.arg_label * out_type * out_class_type
      | Octy_signature of out_type option * out_class_sig_item list
    and out_class_sig_item =
        Ocsg_constraint of out_type * out_type
      | Ocsg_method of string * bool * bool * out_type
      | Ocsg_value of string * bool * bool * out_type
    type out_module_type =
        Omty_abstract
      | Omty_functor of
          (string option * out_module_type) option * out_module_type
      | Omty_ident of out_ident
      | Omty_signature of out_sig_item list
      | Omty_alias of out_ident
    and out_sig_item =
        Osig_class of
          bool * string * out_type_param list * out_class_type *
            out_rec_status
      | Osig_class_type of
          bool * string * out_type_param list * out_class_type *
            out_rec_status
      | Osig_typext of out_extension_constructor * out_ext_status
      | Osig_modtype of string * out_module_type
      | Osig_module of string * out_module_type * out_rec_status
      | Osig_type of out_type_decl * out_rec_status
      | Osig_value of out_val_decl
      | Osig_ellipsis
    and out_type_decl =
      { otype_name : string;
        otype_params : out_type_param list;
        otype_type : out_type;
        otype_private : Asttypes.private_flag;
        otype_immediate : Type_immediacy.t;
        otype_unboxed : bool;
        otype_cstrs : (out_type * out_type) list }
    and out_extension_constructor =
      { oext_name : string;
        oext_type_name : string;
        oext_type_params : string list;
        oext_args : out_type list;
        oext_ret_type : out_type option;
        oext_private : Asttypes.private_flag }
    and out_type_extension =
      { otyext_name : string;
        otyext_params : string list;
        otyext_constructors : out_constructor list;
        otyext_private : Asttypes.private_flag }
    and out_val_decl =
      { oval_name : string;
        oval_type : out_type;
        oval_prims : string list;
        oval_attributes : out_attribute list }
    and out_rec_status = Orec_not | Orec_first | Orec_next
    and out_ext_status = Oext_first | Oext_next | Oext_exception
    type out_phrase =
        Ophr_eval of out_value * out_type
      | Ophr_signature of (out_sig_item * out_value option) list
      | Ophr_exception of (exn * out_value)
  end
