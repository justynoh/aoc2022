open Core 

let input_file = "day6.input"

module S = Set.Make(Char)

let rec find_marker i stream = 
  if List.length stream < 14 then raise (Failure "Precondition error.") else
  let header = List.take stream 14 in
  if (S.of_list header) |> S.length = 14 then i else find_marker (i + 1) (List.tl_exn stream)

let main = 
  input_file
  |> Stdio.In_channel.read_all
  |> String.to_list
  |> find_marker 14
  |> Int.to_string
  |> Stdio.Out_channel.print_endline