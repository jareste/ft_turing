(* 
let () =
  if Array.length Sys.argv <> 3 then (
    Printf.printf "Usage: %s <arg1> <arg2>\n" Sys.argv.(0);
    exit 1
  ) else
    let arg1 = Sys.argv.(1) in
    let arg2 = Sys.argv.(2) in
    Printf.printf "Argument 1: %s\nArgument 2: %s\n" arg1 arg2

let add x y = x + y
let result = add 3 5
let () = Printf.printf "The result is: %d\n" result
let () = assert (result = 8)
let () = Printf.printf "All tests passed!\n" *)

(* starting with real turing *)
type direction = Left | Right
let direction_of_string = function
  | "LEFT" -> Left
  | "RIGHT" -> Right
  | _ -> failwith "Invalid direction"

let turn dir =
  match dir with
  | Left -> Printf.printf "Turning left\n"
  | Right -> Printf.printf "Turning right\n"
  (* let () = turn Left
  let () = turn Right *)

type symbol = char
let print_symbol s =
  Printf.printf "Symbol: %c\n" s
(* let () = print_symbol 'A'
let () = print_symbol 'B' *)


type transition = {
  read: symbol;
  to_state: string;
  write: symbol;
  action: direction;
}

type state_transitions = (symbol * transition) list

type turing_machine = {
  name: string;
  alphabet: symbol list;
  blank: symbol;
  states: string list;
  initial: string;
  finals: string list;
  transitions: (string, state_transitions) Hashtbl.t;
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
  Printf.printf "******************************************************\n"

let print_tape tape state =
  Printf.printf "Tape: ";
  List.iter (Printf.printf "%c ") tape.left;
  Printf.printf "[%c] " tape.current;
  List.iter (Printf.printf "%c ") tape.right;
  Printf.printf "\nState: %s\n" state

let print_alphabet alphabet =
  Printf.printf "Alphabet: [ ";
  List.iter (Printf.printf "%c, ") alphabet;
  Printf.printf "]\n"

let print_machine machine =
  Printf.printf "Machine Name: %s\n" machine.name;
  print_alphabet machine.alphabet;
  Printf.printf "Blank Symbol: %c\n" machine.blank;
  Printf.printf "States: [";
  List.iter (Printf.printf "%s, ") machine.states;
  Printf.printf "]\nInitial: %s\n" machine.initial;
  Printf.printf "Finals: [";
  List.iter (Printf.printf " %s ") machine.finals;
  Printf.printf "]\n";
  Hashtbl.iter (fun state transitions ->
    List.iter (fun (symbol, trans) ->
      Printf.printf "(%s, %c) -> (%s, %c, %s)\n"
        state symbol trans.to_state trans.write (match trans.action with
          | Left -> "LEFT"
          | Right -> "RIGHT")
    ) transitions
  ) machine.transitions
  (* UTILS_END *)

let add_transition tbl state trans =
let lst =
  try Hashtbl.find tbl state
  with Not_found -> []
in
Hashtbl.replace tbl state ((trans.read, trans) :: lst)


let create_machine_example () =
  let alphabet = ['1'; '.'; '-'; '='] in
  let states = ["scanright"; "eraseone"; "subone"; "skip"; "HALT"] in
  let initial = "scanright" in
  let finals = ["HALT"] in
  let machine = create_machine "unary_sub" alphabet '.' states initial finals in

  let open Printf in

  let add state read to_state write action =
    let trans = { read; to_state; write; action = direction_of_string action } in
    add_transition machine.transitions state trans
  in

  add "scanright" '.' "scanright" '.' "RIGHT";
  add "scanright" '1' "scanright" '1' "RIGHT";
  add "scanright" '-' "scanright" '-' "RIGHT";
  add "scanright" '=' "eraseone" '.' "LEFT";

  add "eraseone" '1' "subone" '=' "LEFT";
  add "eraseone" '-' "HALT" '.' "LEFT";

  add "subone" '1' "subone" '1' "LEFT";
  add "subone" '-' "skip" '-' "LEFT";

  add "skip" '.' "skip" '.' "LEFT";
  add "skip" '1' "scanright" '.' "RIGHT";

  machine



let () =
let machine = create_machine_example () in
print_machine machine;
print_separator ();
Printf.printf "Machine created successfully!\n";
print_separator ()
