module ConfigSet = Set.Make(String)

(* TYPEDEFS *)
type direction = Left | Right
type symbol = char
type transition = {
  read: symbol;
  to_state: string;
  write: symbol;
  action: direction;
}
type state_transitions = (symbol * transition) list
type key = string * symbol
type transitions = (key, transition) Hashtbl.t
type turing_machine = {
  name: string;
  alphabet: symbol list;
  blank: symbol;
  states: string list;
  initial: string;
  finals: string list;
  transitions: transitions;
}
type tape = {
  left: symbol list;
  current: symbol;
  right: symbol list;
}
type config = {
  machine: turing_machine;
  tape: tape;
  state: string;
}
(* TYPEDEFS end *)

let direction_of_string = function
  | "LEFT" -> Left
  | "RIGHT" -> Right
  | _ -> failwith "Invalid direction"


let create_machine name alphabet blank states initial finals =
  {
    name;
    alphabet;
    blank;
    states;
    initial;
    finals;
    transitions = Hashtbl.create 10;
  }

  (* UTILS *)
let print_separator () =
  Printf.printf "********************************************************\n"

let print_tape tape state =
  Printf.printf "[";
  List.iter (Printf.printf "%c") tape.left;
  Printf.printf "<%c>" tape.current;
  List.iter (Printf.printf "%c") tape.right;
  Printf.printf "] " 

let print_alphabet alphabet =
  Printf.printf "Alphabet: [ ";
  List.iter (Printf.printf "%c, ") alphabet;
  Printf.printf "]\n"

let print_machine machine =
  let total_width = 54 in
  let name_length = String.length machine.name in
  let padding = (total_width - 2 - name_length) / 2 in
  let left_padding = String.make padding ' ' in
  let right_padding = String.make (total_width - 2 - name_length - padding) ' ' in  
  print_separator ();
  Printf.printf "* %-52s *\n" "";
  Printf.printf "* %s%s%s *\n" left_padding machine.name right_padding;
  Printf.printf "* %-52s *\n" "";
  print_separator ();
  print_alphabet machine.alphabet;
  (* Printf.printf "Blank Symbol: %c\n" machine.blank; *)
  Printf.printf "States: [";
  List.iter (Printf.printf "%s, ") machine.states;
  Printf.printf "]\nInitial: %s\n" machine.initial;
  Printf.printf "Finals: [";
  List.iter (Printf.printf " %s ") machine.finals;
  Printf.printf "]\n";
  Hashtbl.iter (fun (state, symbol) trans ->
    Printf.printf "(%s, %c) -> (%s, %c, %s)\n"
      state symbol trans.to_state trans.write (match trans.action with
        | Left -> "LEFT"
        | Right -> "RIGHT")
  ) machine.transitions ;
  print_separator ();
  Printf.printf "  "

let add_transition tbl state read to_state write action =
  let trans = { read; to_state; write; action = direction_of_string action } in
  Hashtbl.add tbl (state, read) trans


let create_machine_example () =
  let alphabet = ['1'; '.'; '-'; '='] in
  let states = ["scanright"; "eraseone"; "subone"; "skip"; "HALT"] in
  let initial = "scanright" in
  let finals = ["HALT"] in
  let machine = create_machine "unary_sub" alphabet '.' states initial finals in

  add_transition machine.transitions "scanright" '.' "scanright" '.' "RIGHT";
  (* add_transition machine.transitions "scanright" '.' "scanright" '.' "LEFT"; *)
  (* add_transition machine.transitions "scanright" '1' "scanright" '1' "RIGHT"; *)
  add_transition machine.transitions "scanright" '1' "scanright" '1' "LEFT";
  add_transition machine.transitions "scanright" '-' "scanright" '-' "RIGHT";
  add_transition machine.transitions "scanright" '=' "eraseone" '.' "LEFT";

  add_transition machine.transitions "eraseone" '1' "subone" '=' "LEFT";
  add_transition machine.transitions "eraseone" '-' "HALT" '.' "LEFT";

  add_transition machine.transitions "subone" '1' "subone" '1' "LEFT";
  add_transition machine.transitions "subone" '-' "skip" '-' "LEFT";

  add_transition machine.transitions "skip" '.' "skip" '.' "LEFT";
  add_transition machine.transitions "skip" '1' "scanright" '.' "RIGHT";

  machine

(* Turing's machine utils *)
let update_current tape sym =
  { tape with current = sym }

let move_left tape blank =
  match tape.left with
  | [] ->
      failwith "Cannot move left from the beginning of the tape"
  | hd :: tl ->
      { left    = tl
      ; current = hd
      ; right   = tape.current :: tape.right
      }

let move_right tape blank =
  match tape.right with
  | [] ->
      failwith "Cannot move right from the end of the tape"
  | hd :: tl ->
      { left    = tape.current :: tape.left
      ; current = hd
      ; right   = tl
      }

let step config =
  let { state; tape = { current; _ }; machine } = config in
  match Hashtbl.find_opt machine.transitions (state, current) with
  | None ->
      failwith (Printf.sprintf "No transition for (%s, %c)" state current)
  | Some { to_state; write; action; _ } ->
      let tape' = update_current config.tape write in
      let tape'' =
        match action with
        | Left  -> move_left tape' machine.blank
        | Right -> move_right tape' machine.blank
      in
      { machine; state = to_state; tape = tape'' }
(* Turing's machine utils END *)
      
(* Simple turing, it allows infinite loops *)
let run machine input =
  let tape = { left = []; current = input.[0]; right = List.init (String.length input - 1) (fun i -> input.[i + 1]) } in
  let config = { machine; tape; state = machine.initial } in
  let rec loop cfg =
    if List.mem cfg.state machine.finals then (
      Printf.printf "\n";
      exit 0
    );
  
    print_tape cfg.tape cfg.state;
    Printf.printf "%s  "
      (let key = (cfg.state, cfg.tape.current) in
       match Hashtbl.find_opt cfg.machine.transitions key with
       | None -> "ERROR. No transition found\n"
       | Some tr ->
           Printf.sprintf "(%s, %c) -> (%s, %c, %s)\n"
             cfg.state cfg.tape.current
             tr.to_state tr.write
             (match tr.action with Left -> "LEFT" | Right -> "RIGHT")
      );
    
    loop (step cfg)
  in
  loop config


let serialize_config cfg =
  let tape_str =
    String.concat ""
      [ String.of_seq (List.to_seq (List.rev cfg.tape.left));
        "<"; String.make 1 cfg.tape.current; ">";
        String.of_seq (List.to_seq cfg.tape.right) ]
  in
  cfg.state ^ "|" ^ tape_str

  (* Serializes configuration for avoiding potential infinite loops *)
let run_detect_cycles machine input =
  let tape = { left = []; current = input.[0]; right = List.init (String.length input - 1) (fun i -> input.[i + 1]) } in
  let rec loop cfg seen =
    let key = serialize_config cfg in
    if List.mem cfg.state machine.finals then begin
      Printf.printf "\n";
      ()
    end else if ConfigSet.mem key seen then begin
      failwith (Printf.sprintf "Potential infinite loop detected in configuration:\n%s\n" key);
    end else begin
      print_tape cfg.tape cfg.state;
      Printf.printf "%s  "
      (let key = (cfg.state, cfg.tape.current) in
       match Hashtbl.find_opt cfg.machine.transitions key with
       | None -> "ERROR. No transition found\n"
       | Some tr ->
           Printf.sprintf "(%s, %c) -> (%s, %c, %s)\n"
             cfg.state cfg.tape.current
             tr.to_state tr.write
             (match tr.action with Left -> "LEFT" | Right -> "RIGHT")
      );
      let seen' = ConfigSet.add key seen in
      loop (step cfg) seen'
    end
  in
  loop { machine; tape; state = machine.initial } ConfigSet.empty

let () =
let machine = create_machine_example () in
let input = ".111-11=........." in
print_machine machine;

run_detect_cycles machine input;
