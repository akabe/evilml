open Format
open EmlUtils

type 'a t =
  (* comparison *)
  | Eq of 'a * 'a
  | Ne of 'a * 'a
  | Gt of 'a * 'a
  | Lt of 'a * 'a
  | Ge of 'a * 'a
  | Le of 'a * 'a
  (* boolean operator *)
  | Not of 'a
  | And of 'a * 'a
  | Or of 'a * 'a
  (* bit-wise operator *)
  | Lnot of 'a
  | Land of 'a * 'a
  | Lor of 'a * 'a
  | Lxor of 'a * 'a
  | Lsl of 'a * 'a
  | Lsr of 'a * 'a
  | Asr of 'a * 'a
  (* numerical operator *)
  | Pos of 'a (* unary operator + *)
  | Neg of 'a (* unary operator - *)
  | Add of 'a * 'a
  | Sub of 'a * 'a
  | Mul of 'a * 'a
  | Div of 'a * 'a
  | Mod of 'a * 'a
  | FPos of 'a (* unary operator +. *)
  | FNeg of 'a (* unary operator -. *)
  | FAdd of 'a * 'a
  | FSub of 'a * 'a
  | FMul of 'a * 'a
  | FDiv of 'a * 'a
             [@@deriving show]

let map f = function
  | Eq (x, y) -> Eq (f x, f y)
  | Ne (x, y) -> Ne (f x, f y)
  | Gt (x, y) -> Gt (f x, f y)
  | Lt (x, y) -> Lt (f x, f y)
  | Ge (x, y) -> Ge (f x, f y)
  | Le (x, y) -> Le (f x, f y)
  | Not x -> Not (f x)
  | And (x, y) -> And (f x, f y)
  | Or (x, y) -> Or (f x, f y)
  | Lnot x -> Lnot (f x)
  | Land (x, y) -> Land (f x, f y)
  | Lor (x, y) -> Lor (f x, f y)
  | Lxor (x, y) -> Lxor (f x, f y)
  | Lsl (x, y) -> Lsl (f x, f y)
  | Lsr (x, y) -> Lsr (f x, f y)
  | Asr (x, y) -> Asr (f x, f y)
  | Pos x -> Pos (f x)
  | Neg x -> Neg (f x)
  | Add (x, y) -> Add (f x, f y)
  | Sub (x, y) -> Sub (f x, f y)
  | Mul (x, y) -> Mul (f x, f y)
  | Div (x, y) -> Div (f x, f y)
  | Mod (x, y) -> Mod (f x, f y)
  | FPos x -> FPos (f x)
  | FNeg x -> FNeg (f x)
  | FAdd (x, y) -> FAdd (f x, f y)
  | FSub (x, y) -> FSub (f x, f y)
  | FMul (x, y) -> FMul (f x, f y)
  | FDiv (x, y) -> FDiv (f x, f y)

let fold f acc = function
  | Eq (x, y) | Ne (x, y) | Gt (x, y) | Lt (x, y) | Ge (x, y) | Le (x, y)
  | And (x, y) | Or (x, y) | Add (x, y) | Sub (x, y) | Mul (x, y)
  | Div (x, y) | FAdd (x, y) | FSub (x, y) | FMul (x, y) | FDiv (x, y)
  | Mod (x, y) | Land (x, y) | Lor (x, y) | Lxor (x, y) | Lsl (x, y)
  | Lsl (x, y) | Lsr (x, y) | Asr (x, y) -> f (f acc x) y
  | Not x | Lnot x | Pos x | Neg x | FPos x | FNeg x -> f acc x

let fold_map f init op =
  let acc = ref init in
  let aux x =
    let (acc', x') = f !acc x in
    acc := acc';
    x'
  in
  (!acc, map aux op)

let exists f op = fold (fun acc x -> acc || f x) false op
