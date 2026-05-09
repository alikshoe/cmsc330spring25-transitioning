open List
open Nfa
open Utils

(*********)
(* Types *)
(*********)

(* 
  from utils.ml, for your reference:

  type regexp_t =
  | Empty_String
  | Char of char
  | Union of regexp_t * regexp_t
  | Concat of regexp_t * regexp_t
  | Star of regexp_t


  (** NFA type (record) *)
  type ('q, 's) nfa_t = {
    sigma: 's list;
    qs: 'q list;
    q0: 'q;
    fs: 'q list;
    delta: ('q, 's) transition list;
  } *)

(*******************************)
(* Part 3: Regular Expressions *)
(*******************************)

(*i feel like there's a better way to go about this*)
let renum lst = 
  let rec aux lst acc count = 
    match lst with
    [] -> acc
    |_::t -> aux t (acc @ [count]) (count + 1)
  in aux lst [] 0
;;

(*This function takes a regexp and returns an NFA that accepts the same language as the
regular expression. Notice that as long as your NFA accepts the correct language, the
structure of the NFA does not matter since the NFA produced will only be tested to see
which strings it accepts.*)
let rec regexp_to_nfa (regexp: regexp_t) : (int, char) nfa_t = 
  match regexp with
  |Empty_String -> let start = fresh() in 
        {sigma = []; qs = [start]; q0 = start; fs = [start]; delta = []}

  |Char(c) -> let start = fresh() in
        let final = fresh() in
        {sigma = [c]; qs = [start; final]; q0 = start; fs = [final]; delta = [{input = Some c; states = (start, final)}]}
  
  |Union(reg1, reg2) -> let nfa1 = regexp_to_nfa reg1 in
                        let nfa2 = regexp_to_nfa reg2 in
                        let start = fresh() in
                        let final = fresh() in
                        let offset = List.length nfa1.qs + 2 in
                        let nfa1_qs = map (fun x -> x + 2) nfa1.qs in
                        let nfa2_qs = map (fun x -> x + offset) nfa2.qs in
                        let nfa1_start = nfa1.q0 + 2 in
                        let nfa2_start = nfa2.q0 + offset in
                        let end_states1 = map (fun x -> x + 2) nfa1.fs in
                        let end_states2 = map (fun x -> x + offset) nfa2.fs in
                        let delta_offset1 = map (fun x -> {input = x.input; states = (fst x.states + 2, snd x.states + 2)}) nfa1.delta in
                        let delta_offset2 = map (fun x -> {input = x.input; states = (fst x.states + offset, snd x.states + offset)}) nfa2.delta in

                        {sigma = union nfa1.sigma nfa2.sigma; qs = union (union nfa1.qs nfa2.qs) [start; final];
                        q0 = start; fs = [final]; delta = union (union nfa1.delta nfa2.delta) [{input = None; states = (start, nfa1.q0)};
                        {input = None; states = (start, nfa2.q0)}] @ (map (fun x -> {input = None; states = (x, final)}) nfa1.fs) @ (map (fun x -> {input = None; states = (x, final)}) nfa2.fs)}

  |Concat(reg1, reg2) -> let nfa1 = regexp_to_nfa reg1 in
                        let nfa2 = regexp_to_nfa reg2 in
                        let offset = List.length nfa1.qs in
                        let nfa2_qs = map (fun x -> x + offset) nfa2.qs in
                        let nfa2_start = nfa2.q0 + offset in
                        let end_states = map (fun x -> x + offset) nfa2.fs in
                        let delta_offset = map (fun x -> {input = x.input; states = (fst x.states + offset, snd x.states + offset)}) nfa2.delta in
                        {sigma = union nfa1.sigma nfa2.sigma; qs = union nfa1.qs nfa2.qs; q0 = nfa1.q0; fs = nfa2.fs;
                        delta = union (union nfa1.delta nfa2.delta) (map (fun x -> {input = None; states = (x, nfa2.q0)}) nfa1.fs)}
  |Star(reg) -> let nfa = regexp_to_nfa reg in
        let start = fresh() in
        let final = fresh() in
                {sigma = nfa.sigma; qs = union nfa.qs [start; final]; q0 = start; fs = [final]; delta = union nfa.delta [{input = None; states = (start, nfa.q0)}; {input = None; states = (start, final)}; 
                {input = None; states = (final, start)}] @ (map (fun x -> {input = None; states = (x, start)}) nfa.fs)} 
;;

(* The following functions are useful for testing, we have implemented them for you *)
let string_to_regexp str = parse_regexp @@ tokenize str

let string_to_nfa str = regexp_to_nfa @@ string_to_regexp str