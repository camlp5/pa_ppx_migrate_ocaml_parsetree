# 1 "ast.ORIG.ml"
(**pp -syntax camlp5o *)
(* camlp5r *)
(* pp_parsetree.ml,v *)

module Lexing = struct
[%%import: 
# 6 "ast.ORIG.ml"
             
# 6 "ast.ORIG.ml"
                  Lexing.position] 
end

# 10 "ast.ORIG.ml"
module Warnings = struct
[%%import: 
# 11 "ast.ORIG.ml"
             
# 11 "ast.ORIG.ml"
                  Warnings.loc] 
end

# 15 "ast.ORIG.ml"
module Location = struct
# 17 "ast.ORIG.ml"
type t = [%import: 
# 17 "ast.ORIG.ml"
                     
# 17 "ast.ORIG.ml"
                          Location.t
  [@synonym: Warnings.loc]
]
# 23 "ast.ORIG.ml"
[%%import: 'a 
# 23 "ast.ORIG.ml"
                
# 23 "ast.ORIG.ml"
                     Location.loc]
end
module Longident = struct
[%%import: 
# 26 "ast.ORIG.ml"
             
# 26 "ast.ORIG.ml"
                  Longident.t] 
end
module Asttypes = struct
# 30 "ast.ORIG.ml"
type 'a loc = [%import: 'a 
# 30 "ast.ORIG.ml"
                             
# 30 "ast.ORIG.ml"
                                  Asttypes.loc [@synonym: 'a Location.loc]] 
# 35 "ast.ORIG.ml"
[%%import: 
# 35 "ast.ORIG.ml"
             
# 35 "ast.ORIG.ml"
                  Asttypes.arg_label] 
# 37 "ast.ORIG.ml"
[%%import: 
# 37 "ast.ORIG.ml"
             
# 37 "ast.ORIG.ml"
                  Asttypes.label] 
[%%import: 
# 38 "ast.ORIG.ml"
             
# 38 "ast.ORIG.ml"
                  Asttypes.closed_flag] 
[%%import: 
# 39 "ast.ORIG.ml"
             
# 39 "ast.ORIG.ml"
                  Asttypes.rec_flag] 
[%%import: 
# 40 "ast.ORIG.ml"
             
# 40 "ast.ORIG.ml"
                  Asttypes.direction_flag] 
[%%import: 
# 41 "ast.ORIG.ml"
             
# 41 "ast.ORIG.ml"
                  Asttypes.private_flag] 
[%%import: 
# 42 "ast.ORIG.ml"
             
# 42 "ast.ORIG.ml"
                  Asttypes.mutable_flag] 
[%%import: 
# 43 "ast.ORIG.ml"
             
# 43 "ast.ORIG.ml"
                  Asttypes.virtual_flag] 
[%%import: 
# 44 "ast.ORIG.ml"
             
# 44 "ast.ORIG.ml"
                  Asttypes.override_flag] 
[%%import: 
# 45 "ast.ORIG.ml"
             
# 45 "ast.ORIG.ml"
                  Asttypes.variance] 
# 47 "ast.ORIG.ml"
[%%import: 
# 47 "ast.ORIG.ml"
             
# 47 "ast.ORIG.ml"
                  Asttypes.injectivity]
# 52 "ast.ORIG.ml"
end
module Parsetree = struct
open Asttypes
# 56 "ast.ORIG.ml"
[%%import: 
# 56 "ast.ORIG.ml"
             
# 56 "ast.ORIG.ml"
                  Parsetree.constant] 
# 58 "ast.ORIG.ml"
type location_stack = Location.t list 
[%%import: 
# 59 "ast.ORIG.ml"
             
# 59 "ast.ORIG.ml"
                  Parsetree.attribute [@with Asttypes.loc := loc ; Asttypes.label := label ; Asttypes.arg_label := arg_label ; Asttypes.closed_flag := closed_flag ; Asttypes.rec_flag := rec_flag ; Asttypes.direction_flag := direction_flag ; Asttypes.private_flag := private_flag ; Asttypes.mutable_flag := mutable_flag ; Asttypes.virtual_flag := virtual_flag ; Asttypes.variance := variance ; Asttypes.override_flag := override_flag ; Asttypes.variance := variance ; Asttypes.injectivity := injectivity]]
end

# 63 "ast.ORIG.ml"
module Type_immediacy = struct
[%%import: 
# 64 "ast.ORIG.ml"
             
# 64 "ast.ORIG.ml"
                  Type_immediacy.t] 
end

# 69 "ast.ORIG.ml"
module Format_doc = struct
  type abstract_formatter
    type formatter = abstract_formatter
end

# 75 "ast.ORIG.ml"
module Outcometree = struct
# 77 "ast.ORIG.ml"
[%%import: 
# 77 "ast.ORIG.ml"
             
# 77 "ast.ORIG.ml"
                  Outcometree.out_name] 
# 79 "ast.ORIG.ml"
[%%import: 
# 79 "ast.ORIG.ml"
             
# 79 "ast.ORIG.ml"
                  Outcometree.out_ident] 
# 81 "ast.ORIG.ml"
[%%import: 
# 81 "ast.ORIG.ml"
             
# 81 "ast.ORIG.ml"
                  Outcometree.out_string] 
# 84 "ast.ORIG.ml"
[%%import: 
# 84 "ast.ORIG.ml"
             
# 84 "ast.ORIG.ml"
                  Outcometree.out_attribute] 
# 86 "ast.ORIG.ml"
[%%import: 
# 86 "ast.ORIG.ml"
             
# 86 "ast.ORIG.ml"
                  Outcometree.out_value] 
# 88 "ast.ORIG.ml"
[%%import: 
# 88 "ast.ORIG.ml"
             
# 88 "ast.ORIG.ml"
                  Outcometree.out_type_param]
# 90 "ast.ORIG.ml"
[%%import: 
# 90 "ast.ORIG.ml"
             
# 90 "ast.ORIG.ml"
                  Outcometree.out_type] 
[%%import: 
# 91 "ast.ORIG.ml"
             
# 91 "ast.ORIG.ml"
                  Outcometree.out_class_type] 
[%%import: 
# 92 "ast.ORIG.ml"
             
# 92 "ast.ORIG.ml"
                  Outcometree.out_module_type] 
[%%import: 
# 93 "ast.ORIG.ml"
             
# 93 "ast.ORIG.ml"
                  Outcometree.out_phrase] 
end
