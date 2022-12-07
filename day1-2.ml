open Core 

let input_file = "day1.input"

let process_line (curr, (best1, best2, best3)) line = 
  match String.length line with
    0 ->     
      (* if curr is in the top 3, we should kick downwards as necessary. *)
      if curr > best1 then (0, (curr, best1, best2)) else
      if curr > best2 then (0, (best1, curr, best2)) else
      if curr > best3 then (0, (best1, best2, curr)) else
      (0, (best1, best2, best3))
  | _ -> 
      let curr = curr + Int.of_string line in 
      (curr, (best1, best2, best3))

let main = 
  let (best1, best2, best3) = Stdio.In_channel.(fold_lines (create input_file) ~init:(0,(0,0,0)) ~f:process_line) |> snd in
  Stdio.Out_channel.print_endline (Int.to_string (best1 + best2 + best3))