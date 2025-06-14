
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
let () = Printf.printf "All tests passed!\n"
