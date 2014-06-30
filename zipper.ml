(* Zipper implementation. *)

type 'a zipper = Zip of 'a list * 'a list


let empty = Zip ([], [])
let from_list l = Zip ([], l)
let to_list (Zip (ls, rs)) = List.rev ls @ rs

let to_begining (Zip (ls, rs)) = Zip ([], List.rev ls @ rs)
let to_end (Zip (ls, rs)) = Zip (List.rev rs @ ls, [])

let flip (Zip (ls, rs)) = Zip (rs, ls)

let before (Zip (ls, _)) = List.rev
let after (Zip (_, rs)) = List.hd rs
let around (Zip (ls, rs)) = List.rev ls @ List.tl rs

let null = function
  | Zip ([], []) -> true
  | _ -> false

let right = function
  | Zip (ls, hd::rs) -> Zip (hd::ls, rs)
  | z -> z

let left = function
  | Zip (hd::ls, rs) -> Zip (ls, hd::rs)
  | z -> z

let delete = function
  | Zip (ls, _::rs) -> Zip (ls, rs)
  | z -> z

let insert a (Zip (ls, rs)) = Zip (ls, a::rs)

let replace a = function
  | Zip (ls, _::rs) -> Zip (ls, a::rs)
  | z -> z

let cursor (Zip (_, rs)) =
  match rs with
  | hd::_ -> Some hd
  | [] -> None
