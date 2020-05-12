open Lang
open Vocab

(*************************************)
(*             Worklist              *)
(*************************************)
type work = exp * (exp option)

module OrderedType = struct
  type t = work
  let compare (e1,_)  (e2,_) = 
    let c1,c2 = cost e1,cost e2 in
    if c1 = c2 then 0
    else if c1 < c2 then -1
    else 1
end

module Heap = BatHeap.Make (OrderedType)

(* Heap.t = work type = (hypothesis, free variable) *)
type t = Heap.t * exp BatSet.t * string BatSet.t
let empty = (Heap.empty, BatSet.empty, BatSet.empty)

let print_set : t -> unit
=fun (_,set,_) ->
  begin
    print_string " Processed forms : ";
    BatSet.iter (fun e -> print_endline (exp2str e ^ " ")) set;
    print_endline ""
  end

let print_sset : t -> unit
=fun (_,_,sset) ->
  begin
    print_string " Processed forms : ";
    BatSet.iter (fun e -> print_string (e ^ " ")) sset;
    print_endline ""
  end

let explored : exp -> t -> bool
=fun exp ((_,set,sset)) -> 
  let _ = Profiler.start_event "Worklist.explored" in
  let b1 = BatSet.mem (exp2str_mod_hole exp) sset in
(*    let b2 = BatSet.exists (eq_mod_hole exp) set in *)
  let _ = Profiler.finish_event "Worklist.explored" in
(*    (try 
  assert (b1 = b2)
  with _ -> print_endline (exp2str exp ^ " " ^ string_of_bool b1 ^ " " ^ string_of_bool b2); 
    print_set t;
    print_sset t;
  exit 0); *)
  b1

let add : (exp -> int) -> work -> t -> t
=fun cost (h,f) (lst,set,sset) ->
  let _ = Profiler.start_event "Worklist.add.exists" in
  let b_exist = explored h (lst,set,sset) in  
  let _ = Profiler.finish_event "Worklist.add.exists" in
  let res = 
    if b_exist then (lst,set,sset)
    else  
      let _ = Profiler.start_event "Worklist.add.insert" in
      let lst = Heap.add (h,f) lst in
      let _ = Profiler.finish_event "Worklist.add.insert" in
        (lst, BatSet.add h set, BatSet.add (exp2str_mod_hole h) sset) in
    res

(* Worklist.choose *)    
let choose : t -> (work * t) option
=fun (lst,set,sset) ->
  try
  let h = Heap.find_min lst in
    Some (h, (Heap.del_min lst, set, sset))
  with _ -> None

(* [random_elm lst] is (e,l) where [e] is a random element from list [lst] and 
 * [l] is the list resulting from removing [e] from [lst] *)
let random_elm lst =
  let n = Random.int (List.length lst) in
  let (new_lst,_) = List.fold_left (fun (l, i) h -> 
    if i = n then (l,i+1) else (h::l,i+1)
  )
  ([],0) lst in
  (List.nth lst n), new_lst


(* Worklist.choose_random *)    
let choose_ran : t -> (work * t) option
=fun (lst,set,sset) ->
  try
  let (elm, heap_lst) = random_elm (Heap.to_list lst) in 
    Some (elm, (Heap.of_list (heap_lst), set, sset))
  with _ -> None


let print : t -> unit
=fun (lst,set,_) -> ()
(*    print_endline "\n\n================ Worklist =================";
  List.iter (fun x -> 
    match x with
    | (e, Some f) -> print_endline (exp2str e ^ ", " ^ exp2str f) 
    | (e, None) -> print_endline (exp2str e ^ ", None")) lst;
  print_endline "===============================================\n\n"; flush stdout
*)
let string_of_size : t -> string
=fun (lst,set,sset) ->
  " " ^ string_of_int (Heap.size lst) ^ 
  " " ^ string_of_int (BatSet.cardinal set) ^ 
  " " ^ string_of_int (BatSet.cardinal sset)

let size_of_list : t -> int
=fun (lst,_,_) -> Heap.size lst

let size_of_set : t -> int
=fun (_,set,_) -> BatSet.cardinal set