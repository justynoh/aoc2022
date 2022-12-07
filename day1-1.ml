open Core 

let input_file = "day1.input"

let process_line (curr,best) line = 
  match String.length line with
    0 -> (0, best) (* Reset curr *)
  | _ -> let curr = curr + Int.of_string line in (curr, Int.max curr best)

let main = 
  let res = Stdio.In_channel.(fold_lines (create input_file) ~init:(0,0) ~f:process_line) |> snd in
  Stdio.Out_channel.print_endline (Int.to_string res)