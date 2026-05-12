(**pp -syntax camlp5o *)
(* camlp5r *)
(* pp_parsetree.ml,v *)

module Lexing = struct
[%%import: 
             
                  Lexing.position] 
end


module Location = struct
type t = [%import: 
                     
                          Location.t]
[%%import: 'a 
                
                     Location.loc]
end
module Longident = struct
[%%import: 
             
                  Longident.t] 
end
module Asttypes = struct
type 'a loc = [%import: 'a 
                             
                                  Asttypes.loc [@synonym: 'a Location.loc]] 
[%%import: 
             
                  Asttypes.arg_label] 
[%%import: 
             
                  Asttypes.label] 
[%%import: 
             
                  Asttypes.closed_flag] 
[%%import: 
             
                  Asttypes.rec_flag] 
[%%import: 
             
                  Asttypes.direction_flag] 
[%%import: 
             
                  Asttypes.private_flag] 
[%%import: 
             
                  Asttypes.mutable_flag] 
[%%import: 
             
                  Asttypes.virtual_flag] 
[%%import: 
             
                  Asttypes.override_flag] 
[%%import: 
             
                  Asttypes.variance] 
end
module Parsetree = struct
open Asttypes
[%%import: 
             
                  Parsetree.constant] 
type location_stack = Location.t list 
[%%import: 
             
                  Parsetree.attribute] 
end


module Outcometree = struct
[%%import: 
             
                  Outcometree.out_ident] 
[%%import: 
             
                  Outcometree.out_attribute] 
[%%import: 
             
                  Outcometree.out_value] 
[%%import: 
             
                  Outcometree.out_type] 
[%%import: 
             
                  Outcometree.out_class_type] 
[%%import: 
             
                  Outcometree.out_module_type] 
[%%import: 
             
                  Outcometree.out_phrase] 
end
