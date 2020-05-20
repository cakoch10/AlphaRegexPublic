open Options
open Lang
open Vocab
open Hopeless
(* open Worklist *)


module CostMain = struct let cost = Lang.cost end
module CostTimes = struct let cost = Lang.cost_times end
module CostStar = struct let cost = Lang.cost_star end
module CostPlus = struct let cost = Lang.cost_plus end

module WorklistTimes = Worklist.Make(CostPlus)
module WorklistStar = Worklist.Make(CostStar)
module WorklistPlus = Worklist.Make(CostTimes)
module Worklist = Worklist.Make(CostMain)

(*************************************)
(* examples and consistency checking *)
(*************************************)

let iter = ref 0
let t0 = ref (Sys.time ())
let t_total = Sys.time ()

let print_examples examples = 
  List.iter (fun str ->
    print_endline (str2str str)
  ) examples

let rec run : exp -> example -> bool
=fun exp example ->
  let _ = Profiler.start_event "Run" in
  let b = 
    let regexp = Str.regexp (exp2str_e exp ^ "\\.") in
    let str = str2str example ^ "." in
      Str.string_match regexp str 0 in
  let _ = Profiler.finish_event "Run" in
    b

let consistent : exp -> example list -> example list -> bool
=fun exp pos_examples neg_examples -> 
  let p = List.for_all (fun str -> run exp str) pos_examples in
  let n = List.for_all (fun str -> not (run exp str)) neg_examples in
    if !verbose >= 1 then begin
      print_endline ("- Consistency checking for " ^ exp2str exp); 
      print_endline ("  positive : " ^ string_of_bool p ^ ", negative : " ^ string_of_bool n) 
      end
    else
      ();
    (if not n && !verbose >= 1 then 
        print_endline ("  " ^ str2str (List.find (fun str -> run exp str) neg_examples))
    else ());
    p && n

let needless : exp -> example list -> example list -> bool
=fun exp pos_examples neg_examples ->
  let is_outset_zeroone exp = match exp with OZ _ -> true | _ -> false in
(*  let pos_examples_hd_arbitrary = 
    List.exists (fun example -> List.hd example = A) pos_examples &&
    List.exists (fun example -> List.hd example = B) pos_examples in
  let hd_is_closure_of_single_alpha exp = 
      match exp with
      | CONCAT (CLOSURE (ALPHA _), _) -> true
      | CLOSURE (CONCAT (CLOSURE (ALPHA _), _)) -> true
      | _ -> false in
*)    if (* (pos_examples_hd_arbitrary && hd_is_closure_of_single_alpha exp) || *)
       is_outset_zeroone exp 
    then true
    else false

(*************************************)
(* available, holes, subst           *)
(*************************************)

let available : unit -> exp list
=fun () ->
  let availst
  = [ALPHA A; ALPHA B; OR (new_hole(), new_hole()); 
    CONCAT (new_hole(), new_hole()); CLOSURE (new_hole()); OZ (new_hole())] in
  if !mode = IDIOM 
    then availst@[OR (ALPHA A, ALPHA B)]
  else availst

let rec holes : exp -> exp list
=fun e ->
  match e with
  | HOLE h -> [e]
  | OR (e1,e2)
  | CONCAT (e1,e2) -> (holes e1)@(holes e2)
  | CLOSURE e -> holes e
  | OZ e -> holes e
  | _ -> []

(* replace hole inside e by e' *)
let rec subst : exp -> exp -> exp -> exp
=fun e e' hole ->
  match hole with
  | HOLE h ->
    begin
      match e with
      | HOLE h' when h = h' -> e' (* replace the hole with e' *)
      | OR (e1,e2) -> OR (subst e1 e' (HOLE h),subst e2 e' (HOLE h))
      | CONCAT (e1,e2) -> CONCAT (subst e1 e' (HOLE h),subst e2 e' (HOLE h)) 
      | CLOSURE e -> CLOSURE (subst e e' (HOLE h))
      | OZ e -> OZ (subst e e' (HOLE h))
      | _ -> e
    end
  | _ -> raise (Failure "subst: hole is not specified")

(* A?(AB)*B? *)
let solution = CONCAT (CONCAT (OZ (ALPHA B), CLOSURE (CONCAT (ALPHA A, ALPHA B))), OZ (ALPHA A))
let init () = 
  let hole = new_hole() in
    (hole, Some hole)


(* [print_exp e] prints the string representation of [e] if depth of [e] is > [curr_depth] *)
let print_exp : exp -> unit
=fun e ->
  let cd = get_length e in
  if cd > !curr_depth 
  then begin
    let _ = curr_depth := cd in
    print_endline ("Testing expression: "^ exp2str_mod_hole e)
  end

  
let rec work : example list -> example list -> Worklist.t -> exp option
=fun pos_examples neg_examples worklist ->
  iter := !iter + 1;
  if !verbose >= 0 && !iter mod 1000 = 0 && not(!Options.simple)
  then begin
    let t = Sys.time () -. !t0 in
    let _ = t0 := Sys.time() in
    print_endline (string_of_int !iter ^ "." ^
       " Worklist size : " ^ Worklist.string_of_size worklist ^
       " took " ^ string_of_float t ^ "sec" ^ " total: " ^ string_of_float (Sys.time() -. t_total))
  end;
  let next_exp = if !random = 1 then Worklist.choose_ran worklist else Worklist.choose worklist in
  match next_exp with (* choose minimal cost node *)
  | None -> None (* failed to discover a solution *)
  (* when e is a closed expression *)
  | Some ((e, None),worklist) ->
    let e = normalize e in 
    (* print_exp e; *)
    if !verbose >= 1 then print_endline ("Pick a closed expression: " ^ exp2str e);
    if consistent e pos_examples neg_examples 
    then
        Some e (* solution found *)
    else
         (work pos_examples neg_examples worklist)     
         
  (* when e is an expression with hole f *)
  | Some ((e, Some f),worklist) -> (* (work, t type) *)
    (* print_exp e; *)
    if !verbose >= 2 then print_endline (" search: " ^ 
            exp2str_w_outset e ^ " level: " ^string_of_int (level e));
    let b_hopeless = hopeless run e pos_examples neg_examples in
    if b_hopeless || needless e pos_examples neg_examples
    then work pos_examples neg_examples worklist
    else
      (* making new worklist *)
      work pos_examples neg_examples (
        List.fold_left (fun worklist e' -> 
        (* replace the hole inside e by e' *)
        let e_subst = subst e e' f in
        (* e_subst can have holes 
         because subst change just only one hole *)
        let e'' = normalize e_subst in
        let holes = holes e'' in
            match holes with
            | [] -> Worklist.add cost (e'', None) worklist
            | _ ->
              let f' = List.hd holes in
              Worklist.add cost (e'', Some f') worklist
      ) worklist (available()))



let update_lists : Worklist.t ->
                   WorklistTimes.t ->
                   WorklistStar.t ->
                   int ->
                   exp ->
                   Worklist.t * WorklistTimes.t * WorklistStar.t
=fun w_plus w_times w_star idx e ->
  if idx = 0 then (w_plus, WorklistTimes.delete e w_times, WorklistStar.delete e w_star)
  else if idx = 1 then (Worklist.delete e w_plus, w_times, WorklistStar.delete e w_star)
  else (Worklist.delete e w_plus, WorklistTimes.delete e w_times, w_star)

(* [work'] is the same as [work] except it maintains three different worklists 
 * ordered according to three different cost functions and alternates between them *)
let rec work' : example list -> 
                example list -> 
                Worklist.t ->
                WorklistTimes.t ->
                WorklistStar.t ->
                int ->
                exp option
=fun pos_examples neg_examples w_plus w_times w_star idx ->
  let idx = (idx + 1) mod 3 in
  iter := !iter + 1;
  if !verbose >= 0 && !iter mod 1000 = 0 && not(!Options.simple)
  then begin
    let t = Sys.time () -. !t0 in
    let _ = t0 := Sys.time() in
    print_endline (string_of_int !iter ^ "." ^
        " Worklist size : " ^ Worklist.string_of_size w_plus ^
        " took " ^ string_of_float t ^ "sec" ^ " total: " ^ string_of_float (Sys.time() -. t_total))
  end;
  if idx = 0 then
    match Worklist.choose w_plus with
    | None -> None
    | Some ((e, None), w_plus) ->
      let e = normalize e in
      if !verbose >= 1 then print_endline ("Pick a closed expression: " ^ exp2str e);
      if consistent e pos_examples neg_examples then Some e
      else
        let (w_plus,w_times,w_star) = update_lists w_plus w_times w_star idx e in
        (work' pos_examples neg_examples w_plus w_times w_star idx)
    | Some ((e, Some f), w_plus) -> 
      let (w_plus,w_times,w_star) = update_lists w_plus w_times w_star idx e in 
      if !verbose >= 2 then 
        print_endline (" search: " ^ exp2str_w_outset e ^ " level: " ^string_of_int (level e));
      let b_hopeless = hopeless run e pos_examples neg_examples in
      if b_hopeless || needless e pos_examples neg_examples then 
        work' pos_examples neg_examples w_plus w_times w_star idx
      else
        let (w_plus,w_times,w_star) = List.fold_left (fun (w1, w2, w3) e' ->
          let e_subst = subst e e' f in
          let e'' = normalize e_subst in
          let holes = holes e'' in
          match holes with
          | [] -> (Worklist.add cost (e'', None) w1,
                   WorklistTimes.add cost (e'', None) w2,
                   WorklistStar.add cost (e'', None) w3)
          | _  ->
            let f' = List.hd holes in
            (Worklist.add cost (e'', Some f') w1,
             WorklistTimes.add cost (e'', Some f') w2,
             WorklistStar.add cost (e'', Some f') w3)
        ) (w_plus,w_times,w_star) (available())
        in
        work' pos_examples neg_examples w_plus w_times w_star idx
  else if idx = 1 then
    match WorklistTimes.choose w_times with
    | None -> None
    | Some ((e, None), w_times) ->
      let e = normalize e in
      if !verbose >= 1 then print_endline ("Pick a closed expression: " ^ exp2str e);
      if consistent e pos_examples neg_examples then Some e
      else
        let (w_plus,w_times,w_star) = update_lists w_plus w_times w_star idx e in
        (work' pos_examples neg_examples w_plus w_times w_star idx)
    | Some ((e, Some f), w_times) -> 
      let (w_plus,w_times,w_star) = update_lists w_plus w_times w_star idx e in 
      if !verbose >= 2 then 
        print_endline (" search: " ^ exp2str_w_outset e ^ " level: " ^string_of_int (level e));
      let b_hopeless = hopeless run e pos_examples neg_examples in
      if b_hopeless || needless e pos_examples neg_examples then 
        work' pos_examples neg_examples w_plus w_times w_star idx
      else
        let (w_plus,w_times,w_star) = List.fold_left (fun (w1, w2, w3) e' ->
          let e_subst = subst e e' f in
          let e'' = normalize e_subst in
          let holes = holes e'' in
          match holes with
          | [] -> (Worklist.add cost (e'', None) w1,
                    WorklistTimes.add cost (e'', None) w2,
                    WorklistStar.add cost (e'', None) w3)
          | _  ->
            let f' = List.hd holes in
            (Worklist.add cost (e'', Some f') w1,
              WorklistTimes.add cost (e'', Some f') w2,
              WorklistStar.add cost (e'', Some f') w3)
        ) (w_plus,w_times,w_star) (available())
        in
        work' pos_examples neg_examples w_plus w_times w_star idx
  else
    match WorklistTimes.choose w_times with
    | None -> None
    | Some ((e, None), w_times) ->
      let e = normalize e in
      if !verbose >= 1 then print_endline ("Pick a closed expression: " ^ exp2str e);
      if consistent e pos_examples neg_examples then Some e
      else
        let (w_plus,w_times,w_star) = update_lists w_plus w_times w_star idx e in
        (work' pos_examples neg_examples w_plus w_times w_star idx)
    | Some ((e, Some f), w_times) -> 
      let (w_plus,w_times,w_star) = update_lists w_plus w_times w_star idx e in 
      if !verbose >= 2 then 
        print_endline (" search: " ^ exp2str_w_outset e ^ " level: " ^string_of_int (level e));
      let b_hopeless = hopeless run e pos_examples neg_examples in
      if b_hopeless || needless e pos_examples neg_examples then 
        work' pos_examples neg_examples w_plus w_times w_star idx
      else
        let (w_plus,w_times,w_star) = List.fold_left (fun (w1, w2, w3) e' ->
          let e_subst = subst e e' f in
          let e'' = normalize e_subst in
          let holes = holes e'' in
          match holes with
          | [] -> (Worklist.add cost (e'', None) w1,
                    WorklistTimes.add cost (e'', None) w2,
                    WorklistStar.add cost (e'', None) w3)
          | _  ->
            let f' = List.hd holes in
            (Worklist.add cost (e'', Some f') w1,
              WorklistTimes.add cost (e'', Some f') w2,
              WorklistStar.add cost (e'', Some f') w3)
        ) (w_plus,w_times,w_star) (available())
        in
        work' pos_examples neg_examples w_plus w_times w_star idx


let perform : example list -> example list -> pgm option
=fun pos_examples neg_examples -> 
  if !extra_heaps = 0 then 
    let init_worklist = Worklist.add cost (init()) Worklist.empty in
    work pos_examples neg_examples init_worklist
  else
    let w_plus = Worklist.add cost (init()) Worklist.empty in
    let w_times = WorklistTimes.add cost (init()) WorklistTimes.empty in
    let w_star = WorklistStar.add cost (init()) WorklistStar.empty in
    work' pos_examples neg_examples w_plus w_times w_star  0
