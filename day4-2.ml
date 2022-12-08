open Core 

let input_file = "day4.input"

let read_ranges s = 
  match String.split_on_chars s ~on:[','; '-'] |> List.map ~f:Int.of_string with 
    [a; b; c; d] -> ((a, b), (c, d))
  | _ -> raise (Failure "Precondition error.")

let is_overlap ((a1, a2), (b1, b2)) = 
  not ((a2 < b1 || b2 < a1))  

let main = 
  input_file
  |> Stdio.In_channel.read_lines
  |> List.map ~f:read_ranges 
  |> List.count ~f:is_overlap
  |> Int.to_string 
  |> Stdio.Out_channel.print_endline