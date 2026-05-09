open List
open Utils

(*********)
(* Types *)
(*********)

(* 
  from utils.ml, for your reference

  type ('q, 's) transition = {
    input: 's option; 
    states: 'q * 'q;
  }

  (** NFA type (record) *)
  type ('q, 's) nfa_t = {
    sigma: 's list;
    qs: 'q list;
    q0: 'q;
    fs: 'q list;
    delta: ('q, 's) transition list;
  } 
*)

(****************)
(* Part 1: NFAs *)
(****************)

(*Helper func that removes duplicate elts from a list.*)
let uniq lst = 
  let rec aux l acc = 
    match l with
    [] -> List.rev acc
    |h::t -> if elem h acc then aux t acc else aux t (h::acc)
  in aux lst []
;;

(*Helper func that converts an 'a list to an 'a option list.*)
let rec convert_lst lst = 
  let rec aux lst new_lst =
    match lst with
    [] -> new_lst
    |h::t -> aux t (new_lst @ [(Some(h))])
  in aux lst []
;;

(*Helper func that checks if two lists have any common elements.*)
let rec common_elts lst1 lst2 = 
  match lst2 with
  |[] -> false
  |h::t -> if elem h lst1 then true 
  else common_elts lst1 t
;;

                       (*qs: set of initial states, s: symbol option*)
let move (nfa: ('q,'s) nfa_t) (qs: 'q list) (s: 's option) : 'q list =
  let rec aux acc lst =
    match lst with
    [] -> uniq acc
    |h::t -> 
      let input = h.input in 
      let (from, too) = h.states in 
      if input = s && (elem from qs) then aux (acc@[too]) t 
      else aux acc t
    in aux [] nfa.delta
;;

let e_closure (nfa: ('q,'s) nfa_t) (qs: 'q list) : 'q list =
  let rec aux visited queue = 
    match queue with
    [] -> uniq visited
    |h::t ->
      if elem h visited then aux visited t (*if already visited, continue moving through queue*)
      else 
        let mv = move nfa [h] None in
        aux (h::visited) (t @ mv) (*if not visited, add h to visited and move through rest of the queue AND any new states moved to by h*)
    in aux [] qs
;;

(*******************************)
(* Part 2: Subset Construction *)
(*******************************)

let new_states (nfa: ('q,'s) nfa_t) (qs: 'q list) : 'q list list =
  fold_left (fun a x -> [uniq (move nfa qs x @ e_closure nfa (move nfa qs x))] @ a) [] (convert_lst nfa.sigma)
;;

let new_trans (nfa: ('q,'s) nfa_t) (qs: 'q list) : ('q list, 's) transition list =
  fold_left (fun a x -> {states = qs, uniq (move nfa qs x @ e_closure nfa (move nfa qs x)); input = x} :: a) [] (convert_lst nfa.sigma)
;;

let new_finals (nfa: ('q,'s) nfa_t) (qs: 'q list) : 'q list list =
  if common_elts qs nfa.fs then [qs] else []
;;

let rec nfa_to_dfa_step (nfa: ('q,'s) nfa_t) (dfa: ('q list, 's) nfa_t)
    (work: 'q list list) : ('q list, 's) nfa_t =
    match work with
    [] -> {sigma = dfa.sigma; qs = dfa.qs; q0 = dfa.q0; fs = dfa.fs; delta = dfa.delta} (*no changes*)
    |h::t -> 
      let e = new_states nfa h in
      let d' = new_trans nfa h in
      let fd = new_finals nfa h in
      {sigma = dfa.sigma; qs = dfa.qs @ e; q0 = dfa.q0; fs = dfa.fs @ fd; delta = union (dfa.delta) d'}
;;
  (* Input: NFA (Σ, Q, q0, Fn, δ)
     Output:DFA (Σ, R, r0, Fd, δ') 
  *)

  let rec nfa_to_dfa (nfa: ('q,'s) nfa_t) : ('q list, 's) nfa_t =
  let r0 = e_closure nfa [nfa.q0] in
  let rs = r0::[] in
  let dfa = {sigma = nfa.sigma; qs = rs; q0 = r0; fs = []; delta = []} in
  let rec aux acc visited queue = 
    match queue with
    [] -> {acc with qs = uniq (filter (fun states -> not (is_empty states)) acc.qs);
                    delta = uniq (filter (fun {input; states = (lst1, lst2)} -> not (is_empty lst1) && not (is_empty lst2)) acc.delta)}
    |h::t -> if elem h visited then aux acc visited t
    else
      let acc = nfa_to_dfa_step nfa acc [h] in
      let new_queue = diff acc.qs visited in
    aux acc (visited @ [h]) new_queue 
  in aux dfa [] dfa.qs
;;

(*TEMP: for debugging purposes*)
let fs (nfa: ('q,char) nfa_t) (s: string) : 'q list = 
  let fs =
    fold_left (fun acc ch -> 
      let next = e_closure nfa (move nfa acc ch) in 
      next) (e_closure nfa [nfa.q0]) (convert_lst (explode s))
  in fs
;;

let accept (nfa: ('q,char) nfa_t) (s: string) : bool =
  let end_states = 
    fold_left (fun acc ch -> 
      let next = e_closure nfa (move nfa acc ch) in 
      next) (e_closure nfa [nfa.q0]) (convert_lst (explode s))
in common_elts end_states nfa.fs
;;
