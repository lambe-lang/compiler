module Make (CharParser : module type of Transept.Extension.Parser.For_char_list) = sig

  module Parser :

  module Kind : Kind.Make_via_parser (Parser)
  module Type : Type.Make_via_parser (Parser)
  module Term : Term.Make_via_parser (Parser)

  val stream s : string list -> 'a

  include module type of Parser
end
