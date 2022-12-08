open Core 

let input_file = "day3.input"

module S = Set.Make(Char)

let map c = Char.(to_int c - if is_lowercase c then 96 else 38)

let process_line line = 
  let break = String.length line / 2 in
  let (_, copt) = String.foldi 
    line 
    ~init:(S.empty, None)
    ~f:(fun i (s, res) c -> 
      if i < break then (S.add s c, res) else
      match res with 
        Some _ -> (s, res)
      | None ->
      if S.exists s ~f:(Char.equal c) 
      then (s, Some c)
      else (s, res)
      )
    in
  match copt with 
    None -> raise (Failure "Precondition error.")
  | Some c -> map c

let main = 
  input_file
  |> Stdio.In_channel.read_lines
  |> List.map ~f:process_line 
  |> List.fold ~init:0 ~f:(+)
  |> Int.to_string 
  |> Stdio.Out_channel.print_endline