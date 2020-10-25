open Base
open Stdio

let lenght l = List.map ~f:String.length l

(* let render_table headers elements = "" *)

let get_max_of_lists acc row = List.map2_exn ~f:Int.max acc (lenght row)

let max_widths headers rows =
  List.fold rows ~init:(lenght headers) ~f:get_max_of_lists

let headers = [ "language"; "architect"; "first release" ]

let render_separator widths =
  let pieces = List.map widths ~f:(fun l -> String.make (l + 2) '-') in
  "|" ^ String.concat ~sep:"+" pieces ^ "|"

let pad s l = " " ^ s ^ String.make (l + 1 - String.length s) ' '

let rows =
  [
    [ "Lisp"; "John McCarthy"; "1958" ];
    [ "C"; "Dennis Ritchie"; "1969" ];
    [ "ML"; "Robin Milner"; "1973" ];
    [ "OCaml"; "Xavier Leroy"; "1996" ];
  ]

let render_row row widths =
  let padded = List.map2_exn row widths ~f:pad in
  "|" ^ String.concat ~sep:"|" padded ^ "|"

let render_table headers rows =
  let widths = max_widths headers rows in
  String.concat ~sep:"\n"
    ( render_row headers widths :: render_separator widths
    :: List.map rows ~f:(fun row -> render_row row widths) )
