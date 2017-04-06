(* pc.ml
 * A parsing-combinators package for ocaml
 *
 * Prorammer: Mayer Goldberg, 2015
 *)

(* general list-processing procedures *)

let rec ormap f s =
  match s with
  | [] -> false
  | car :: cdr -> (f car) || (ormap f cdr);;

let rec andmap f s =
  match s with
  | [] -> true
  | car :: cdr -> (f car) && (andmap f cdr);;	  

let string_to_list str =
  let rec loop i limit =
    if i = limit then []
    else (Bytes.get str i) :: (loop (i + 1) limit)
  in
  loop 0 (Bytes.length str);;

let list_to_string s =
  let rec loop s n =
    match s with
    | [] -> Bytes.make n '?'
    | car :: cdr ->
       let result = loop cdr (n + 1) in
       Bytes.set result n car;
       result
  in
  loop s 0;;

module PC = struct

(* the parsing combinators defined here *)
  
exception X_not_yet_implemented;;

exception X_no_match;;

let const pred =
  function 
  | [] -> raise X_no_match
  | e :: s ->
     if (pred e) then (e, s)
     else raise X_no_match;;

let caten nt1 nt2 =
  fun s ->
  let (e1, s) = (nt1 s) in
  let (e2, s) = (nt2 s) in
  ((e1, e2), s);;

let disj nt1 nt2 =
  fun s ->
  try (nt1 s)
  with X_no_match -> (nt2 s);;

let pack nt f =
  fun s ->
  let (e, s) = (nt s) in
  ((f e), s);;

let delayed thunk =
  fun s -> thunk() s;;

let nt_epsilon s = ([], s);;

let nt_none _ = raise X_no_match;;

let disj_list nts = List.fold_right disj nts nt_none;;

let nt_end_of_input = function
  | []  -> (true, [])
  | _ -> raise X_no_match;;

let rec star nt =
  fun s ->
  try let (e, s) = (nt s) in
      let (es, s) = (star nt s) in
      (e :: es, s)
  with X_no_match -> ([], s);;

let plus nt =
  pack (caten nt (star nt))
       (fun (e, es) -> (e :: es));;

let guard nt pred =
  fun s ->
  let ((e, s) as result) = (nt s) in
  if (pred e) then result
  else raise X_no_match;;

let diff nt1 nt2 =
  fun s ->
  let result1 = try Some(nt1 s) with X_no_match -> None in
  match result1 with
  | None -> raise X_no_match
  | Some(result) ->
     let result2 = try Some(nt2 s) with X_no_match -> None in
     match (result2) with
     | None -> result
     | _ -> raise X_no_match;;

let maybe nt =
  fun s ->
  try let (e, s) = (nt s) in
      (Some(e), s)
  with X_no_match -> (None, s);;

(* useful general parsers for working with text *)

let make_char equal ch1 = const (fun ch2 -> equal ch1 ch2);;

let char = make_char (fun ch1 ch2 -> ch1 = ch2);;

let char_ci =
  make_char (fun ch1 ch2 -> (Char.lowercase ch1) = (Char.lowercase ch2));;

let make_word char str = 
  List.fold_right
    (fun nt1 nt2 -> pack (caten nt1 nt2) (fun (a, b) -> a :: b))
    (List.map char (string_to_list str))
    nt_epsilon;;

let word = make_word char;;

let word_ci = make_word char_ci;;

let make_one_of char str =
  List.fold_right
    disj
    (List.map char (string_to_list str))
    nt_none;;

let one_of = make_one_of char;;

let one_of_ci = make_one_of char_ci;;

let nt_whitespace = const (fun ch -> ch <= ' ');;

let make_range leq ch1 ch2 =
  const (fun ch -> (leq ch1 ch) && (leq ch ch2));;

let range = make_range (fun ch1 ch2 -> ch1 <= ch2);;

let range_ci =
  make_range (fun ch1 ch2 -> (Char.lowercase ch1) <= (Char.lowercase ch2));;

let nt_any = const (fun ch -> true);;

(* testing the parsers *)

let test_string nt str =
  let (e, s) = (nt (string_to_list str)) in
  (e, (Printf.sprintf "->[%s]" (list_to_string s)));;


end;; (* end of struct PC *)

(* end-of-input *)
