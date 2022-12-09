open Core 

let input_file = "day6.input"

let rec find_marker_position i (a, b, c, d) stream = 
  if not Char.(equal a b || equal a c || equal a d || equal b c || equal b d || equal c d) then i else
  match stream with 
    [] -> i
  | e::s -> find_marker_position (i + 1) (b, c, d, e) s

let find_marker stream = 
  match stream with
    a::b::c::d::s -> find_marker_position 4 (a, b, c, d) s
  | _ -> raise (Failure "Precondition error.")

let main = 
  input_file
  |> Stdio.In_channel.read_all
  |> String.to_list
  |> find_marker
  |> Int.to_string
  |> Stdio.Out_channel.print_endline