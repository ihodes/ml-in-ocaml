(* TODO: Make into functor with parameterizing module defining
         a group with e.g. +, -, *, identity, zero. Then this
         can work with int, rational, float, complex etc.  *)

(* Matrix is a list of columns, which are lists of elements. *)
type matrix = float list list
type vector = float list


let cons a lst = a::lst
let mapcat f lst = List.concat (List.map f lst)
let remove el lst = List.filter (fun e -> e != el) lst
let sumf = List.fold_left ( +. ) 0.
let pif n f = List.fold_left ( *. ) 1. (List.map f (0--n))

let take n lst =
  let rec take_h n lst acc =
    match n, lst with
    | 0, _ -> acc
    | n, [] -> raise (Failure "take")
    | n, hd::tl -> take_h (n-1) tl (hd::acc)
  in List.rev (take_h n lst [])

let rec drop n lst =
  match n, lst with
  | 0, _ -> lst
  | n, [] -> raise (Failure "drop")
  | n, hd::tl -> drop (n-1) tl

let rec butlast xs =
  match xs with
  | x::[] -> []
  | x::xs -> x::butlast xs
  | _ -> raise (Failure "butlast")

let rec last xs =
  match xs with
  | x::[] -> x
  | x::xs -> last xs
  | _ -> raise (Failure "last")

let (--) s e =
  let rec rng c acc  =
    if c < s  then acc else rng (c-1) (c::acc)
  in rng (e-1) []

let rec zeroes n =
  if n == 0 then [] else (0.::zeroes (n-1))

let rec inversions xs =
  let inverted y ys = List.filter (fun e -> y > e) ys in
  let rec inv xs acc =
    match xs with
    | [] -> acc
    | x::xs -> inv xs (acc + (List.length (inverted x xs)))
  in inv xs 0

let sgnf xs = if (inversions xs) mod 2 == 0 then 1. else -1.

(* NB: Works for lists with unique elements. *)
(* TODO: Make tail-recursive. *)
let rec permutations lst =
  if List.length lst == 1 then [lst] else
    let permute_but el = permutations (remove el lst) in
    mapcat (fun el -> List.map (cons el) (permute_but el)) lst

let permutation_indices n = permutations (0--n)


(* Vectors  *)

let rec dot xs ys =
  match xs, ys with
  | [], [] -> 0.
  | x::xs, y::ys -> x *. y +. dot xs ys
  | _, _ -> raise (Failure "dot")

let rec vadd xs ys =
  match xs, ys with
  | [], [] -> []
  | x::xs, y::ys -> x +. y :: vadd xs ys
  | _, _ -> raise (Failure "vadd")

let rec vminus xs ys =
  match xs, ys with
  | [], [] -> []
  | x::xs, y::ys -> x -. y :: vadd xs ys
  | _, _ -> raise (Failure "vminus")


(* Matrices *)

let mget matrix row col = List.nth (List.nth matrix col) row

let rec transpose m  =
  match m with
  | []::_ -> []
  | _     -> List.map List.hd m :: transpose (List.map List.tl m)

let mplus = List.map2 vadd
let mminus = List.map2 vminus

let mcat = List.append

let rec mtimes a b =
  match a, b with
  | _, [] -> []
  | [], _ -> raise (Failure "mtimes")
  | _, h::t -> List.map (dot h) (transpose a) :: mtimes a t

let determinant matrix =
  let n = List.length matrix in
  let product_midx perm = pif n (fun i -> mget matrix i (List.nth perm i)) in
  let sgn_product_midx idx perm = (sgnf perm) *. (product_midx perm) in
  sumf (List.mapi sgn_product_midx (permutations (0--n)))

let identity n =
  let zipper = Zipper.from_list (zeroes (n-1)) in
  let one_in zip m = (Zipper.to_list (Zipper.insert 1. zip))::m in
  let rec next_col zip m =
    match zip with
    | Zipper.Zip (_, []) -> one_in zip m
    | Zipper.Zip (ls, r::rs) -> next_col (Zipper.right zip) (one_in zip m)
  in List.rev (next_col zipper [])

(* implement row_reduce matrix, + parts *)
(* let rswap matrix r1 r2 = swap rows r1 and r2 *)
(* let rdiv matrix row s =  divide row by s *)
(* let raddm matrix s r1 r2 = add s*r1 to r2 *)

(* let row_reduce m = row reduce matrix m *)

(* let invert mata = *)
(*   if determinant mat <> 0 then raise (Failure "invert") else *)
(*     let n = List.length mat in *)
(*     let mat = mcat mat (identity n) in *)
(*     let reduced = row_reduce mat in *)
(*     drop n mat *)
