(* util functions *)

let is_lower_case c =
  'a' <= c && c <= 'z'

let is_upper_case c =
  'A' <= c && c <= 'Z'

let is_alpha c =
  is_lower_case c || is_upper_case c

let is_digit c =
  '0' <= c && c <= '9'

let is_alphanum c =
  is_lower_case c ||
  is_upper_case c ||
  is_digit c

let is_blank c =
  String.contains " \012\n\r\t" c

let explode s =
  List.of_seq (String.to_seq s)

let implode ls =
  String.of_seq (List.to_seq ls)

let readlines (file : string) : string =
  let fp = open_in file in
  let rec loop () =
    match input_line fp with
    | s -> s ^ "\n" ^ (loop ())
    | exception End_of_file -> ""
  in
  let res = loop () in
  let () = close_in fp in
  res

(* end of util functions *)

(* parser combinators *)

type 'a parser = char list -> ('a * char list) option

let parse (p : 'a parser) (s : string) : ('a * char list) option =
  p (explode s)

let pure (x : 'a) : 'a parser =
  fun ls -> Some (x, ls)

let fail : 'a parser = fun ls -> None

let bind (p : 'a parser) (q : 'a -> 'b parser) : 'b parser =
  fun ls ->
  match p ls with
  | Some (a, ls) -> q a ls
  | None -> None

let (>>=) = bind
let (let*) = bind

let read : char parser =
  fun ls ->
  match ls with
  | x :: ls -> Some (x, ls)
  | _ -> None

let satisfy (f : char -> bool) : char parser =
  fun ls ->
  match ls with
  | x :: ls ->
    if f x then Some (x, ls)
    else None
  | _ -> None

let char (c : char) : char parser =
  satisfy (fun x -> x = c)

let seq (p1 : 'a parser) (p2 : 'b parser) : 'b parser =
  fun ls ->
  match p1 ls with
  | Some (_, ls) -> p2 ls
  | None -> None

let (>>) = seq

let seq' (p1 : 'a parser) (p2 : 'b parser) : 'a parser =
  fun ls ->
  match p1 ls with
  | Some (x, ls) ->
    (match p2 ls with
     | Some (_, ls) -> Some (x, ls)
     | None -> None)
  | None -> None

let (<<) = seq'

let alt (p1 : 'a parser) (p2 : 'a parser) : 'a parser =
  fun ls ->
  match p1 ls with
  | Some (x, ls)  -> Some (x, ls)
  | None -> p2 ls

let (<|>) = alt

let map (p : 'a parser) (f : 'a -> 'b) : 'b parser =
  fun ls ->
  match p ls with
  | Some (a, ls) -> Some (f a, ls)
  | None -> None

let (>|=) = map

let (>|) = fun p c -> map p (fun _ -> c)

let rec many (p : 'a parser) : ('a list) parser =
  fun ls ->
  match p ls with
  | Some (x, ls) ->
    (match many p ls with
     | Some (xs, ls) -> Some (x :: xs, ls)
     | None -> Some (x :: [], ls))
  | None -> Some ([], ls)

let rec many1 (p : 'a parser) : ('a list) parser =
  fun ls ->
  match p ls with
  | Some (x, ls) ->
    (match many p ls with
     | Some (xs, ls) -> Some (x :: xs, ls)
     | None -> Some (x :: [], ls))
  | None -> None

let rec many' (p : unit -> 'a parser) : ('a list) parser =
  fun ls ->
  match p () ls with
  | Some (x, ls) ->
    (match many' p ls with
     | Some (xs, ls) -> Some (x :: xs, ls)
     | None -> Some (x :: [], ls))
  | None -> Some ([], ls)

let rec many1' (p : unit -> 'a parser) : ('a list) parser =
  fun ls ->
  match p () ls with
  | Some (x, ls) ->
    (match many' p ls with
     | Some (xs, ls) -> Some (x :: xs, ls)
     | None -> Some (x :: [], ls))
  | None -> None

let whitespace : unit parser =
  fun ls ->
  match ls with
  | c :: ls ->
    if String.contains " \012\n\r\t" c
    then Some ((), ls)
    else None
  | _ -> None

let ws : unit parser =
  (many whitespace) >| ()

let ws1 : unit parser =
  (many1 whitespace) >| ()

let digit : char parser =
  satisfy is_digit

let natural : int parser =
  fun ls ->
  match many1 digit ls with
  | Some (xs, ls) ->
    Some (int_of_string (implode xs), ls)
  | _ -> None

let literal (s : string) : unit parser =
  fun ls ->
  let cs = explode s in
  let rec loop cs ls =
    match cs, ls with
    | [], _ -> Some ((), ls)
    | c :: cs, x :: xs ->
      if x = c
      then loop cs xs
      else None
    | _ -> None
  in loop cs ls

let keyword (s : string) : unit parser =
  (literal s) >> ws >| ()
(* end of parser combinators *)
let digit = char '0' <|> char '1' <|> char '2' <|> char '3' <|> char '4' <|> char '5' <|> char '6' <|> char '7' <|> char '8' <|> char '9' (*Zero | One| Two| Three| Four| Five|Six| Seven| Eight| Nine *)
type nat = int

(*): char parser = satisfy( fun x -> '0' <= x && x <= '9')*) 
type const = Nat of int | Name of string | Unit of unit

type com =
    Push of const 
  | Trace 
  | Add
  | Sub 
  | Mul 
  | Div


type prog= com list
type value= N of int | Name2 of string | U of unit

(*let const_parser : const parser=*)
let nat_p : const parser = 
  natural >>= fun x -> pure (Nat (x))

let name_p : const parser = 
  (satisfy is_alpha <|> char '_') >>= 
  fun r-> many (satisfy is_alphanum <|> char ' ' <|> char '_' <|> char '\'' ) >>=
  fun rs-> pure (Name (implode (r:: rs)))

let unit_p: const parser =(*) let* _= keyword "()" in let* u= const in pure (Unit ()) *)
  (keyword "()") >>= fun u -> pure (Unit u) 

let const_p: const parser =
  let* w= ws in name_p <|> let* w=ws in nat_p <|> let* w=ws in unit_p

let push_p: com parser =
  keyword "Push" >>= fun _-> 
  const_p >>= fun x-> pure (Push x)

let trace_p =
  keyword "Trace" >>= fun _-> pure Trace

let add_p: com parser = 
  keyword "Add" >>= fun _-> pure Add

let sub_p: com parser = 
  keyword "Sub" >>= fun _-> pure Sub

let mul_p : com parser = 
  keyword "Mul" >>= fun _-> pure Mul

let div_p: com parser =
  keyword "Div" >>= fun _-> pure Div

(*let rec if_else (): com parser =
  keyword "If" >> (coms_p ()) >>= fun t_branch -> 
  keyword "Else" >> coms_p () >>= fun f_branch -> keyword "End" >> ws >> pure(If_else (t_branch, f_branch))

  and com_p (): com parser= 
  (push_p <|> trace_p <|> add_p <|> sub_p <|> mul_p <|> div_p <|> if_else ()) (* )>>= fun x-> 
                                                                                 ws >> pure x *)(*let* _= ws in let* _= keyword ";" in let* _= ws in pure c*)

  and coms_p (): com parser = many' com_p *)

let com_p : com parser= 
  push_p <|> trace_p <|> add_p <|> sub_p <|> mul_p <|> div_p

type stack = const list

let prog_parser : prog parser=
  let* wh=ws in let* com = com_p in
  let* prog = many (let*_ = ws in com_p) in let* wh2= ws
  in pure (com::prog) <|> let* com= com_p in pure (com::[])

let rec eval (p: com list) (stk: stack) : (string * string list ) = 
  let rec aux p stk out=
    match p, stk with 
    |Push x::p, _-> aux p (x::stk) out
    |Trace ::p, []-> "Error", []
    |Trace ::p, s::stk -> (match s with 
        |Nat s-> aux p (Unit ()::stk) (string_of_int s::out)
        |Name s-> aux p (Unit ()::stk) (s::out)
        |Unit s-> aux p (Unit ()::stk) ("()"::out))
    |Add ::p, v2::v1::stk-> (match v2,v1 with 
        |(Nat v2, Nat v1) -> aux p (Nat(v2+v1)::stk) out
        |_-> "Error", [])
    |Add ::p, _-> "Error", []
    |Sub ::p, v2::v1::stk-> (match v2,v1 with 
        |Nat v2, Nat v1 -> if ((v1-v2) <0) then ("Error", []) else aux p (Nat(v1-v2)::stk) out(*aux p (Nat(if v1-v2 <= 0 then v1-v2 else 0)::stk) out *)
        |_-> "Error", [])
    |Sub ::p, _-> "Error", []
    |Mul ::p, v2::v1::stk-> (match v2,v1 with 
        |Nat v2, Nat v1-> aux p (Nat(v2*v1)::stk) out
        |_-> "Error", [])
    |Mul ::p, _-> "Error", []
    |Div ::p, v2::v1::stk -> (match v2, v1 with 
        |Nat v2, Nat v1-> if v2 = 0 then ("Error", []) else aux p (Nat(v1/v2)::stk) out
        |_-> "Error", [])
    |Div ::p, _-> "Error", []
    |[],_-> match stk with 
      |Nat(h) ::stk-> (string_of_int h, out)
      |Name(h2)::stk-> (h2, out)
      |Unit(h3)::stk-> ("()", out)
      |_-> "Error", []
  in aux p stk []
  
let interpreter (src : string) : (string * string list) = 
  match parse prog_parser src with
  |Some (prog, [])-> eval prog []
  |_-> "Error", []


