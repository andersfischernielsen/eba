
(* TODO: Use CIL's Pretty instead *)

module SP = SmartPrint

type doc = SmartPrint.t

let empty = SP.empty

let (!^) = SP.(!^)

let space = SP.space

let newline = SP.newline

let dot = SP.(!^ ".")

let comma = SP.(!^ ",")

let colon = SP.(!^ ":")

let semi = SP.(!^ ";")

let (+) = SP.(^-^)

let (++) = SP.(^^)

let bool = SP.OCaml.bool

let int = SP.OCaml.int

let prefix str doc = !^ str + doc

let append = SP.append

let parens = SP.parens

let braces = SP.braces

let brackets = SP.brakets

let double_quotes = SP.double_quotes

let angle_brackets = SP.angle_brakets

let concat = SP.concat

let separate = SP.separate

let newline_sep = separate newline

let space_sep = separate space

let comma_sep = separate comma

let semi_sep = separate semi

let indent = SP.indent

let words = SP.words

let nest = SP.nest

let to_string = SP.to_string 70 4

let to_stdout = SP.to_stdout 70 4
