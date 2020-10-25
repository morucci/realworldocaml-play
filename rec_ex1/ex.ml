open Base
open Stdio

let rec find_first_repeat list =
  match list with
  | [] -> None
  | [ _ ] -> None
  | x :: y :: tl -> if x = y then Some x else find_first_repeat (y :: tl)

let path = "/usr/bin:/usr/local/bin:/bin:/sbin:/usr/bin"

let print_dedup =
  String.split ~on:':' path
  |> List.dedup_and_sort ~compare:String.compare
  |> List.iter ~f:print_endline

let concat ?sep x y =
  let sep = match sep with None -> "" | Some x -> x in
  x ^ sep ^ y

let concat2 ?(sep = " - ") x y = x ^ sep ^ y

let display_concat = print_endline (concat2 "Fabien" "Boucher")

let rec sum l = match l with [] -> 0 | h :: t -> h + sum t

let rec to_drop l value =
  match l with
  | [] -> []
  | hd :: tl ->
      let ntl = to_drop tl value in
      if hd = value then to_drop ntl value else hd :: ntl

let () = display_concat
