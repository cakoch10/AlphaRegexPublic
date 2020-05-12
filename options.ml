let inputfile = ref ""
let verbose = ref 0
let profile = ref false
let simple = ref false
let random = ref 0
let cost_fun = ref 0

let options = 
  [
    ("-v", (Arg.Int (fun n -> verbose := n)), "verbose mode: 0, 1, 2");
    ("-input", (Arg.String (fun s -> inputfile := s)), "input file containing positive and negative examples");
    ("-profile", (Arg.Set profile), "profiling");
    ("-simple", (Arg.Set simple), "simple");
    ("-random", (Arg.Int (fun n -> random := n)), "how to select off worklist: 1 for random 0 else");
    ("-cost", (Arg.Int (fun n -> cost_fun := n)), "which cost function to use: 0, 1, 2")
  ]
