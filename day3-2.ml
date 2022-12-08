open Core 

let input_file = "day3.input"

module S = Set.Make(Char)

let map c = Char.(to_int c - if is_lowercase c then 96 else 38)

let process_line (priority, (s1opt, s2opt)) s3 = 
  match s1opt with 
    None -> (priority, (Some s3, None))
  | Some s1 -> (
    match s2opt with 
      None -> (priority, (Some s1, Some s3))
    | Some s2 -> ( (* This is the third line. We can process now. *)
      let inter = S.(s1 |> inter s2 |> inter s3) in
      if S.length inter = 1 
      then (priority + (S.choose_exn inter |> map), (None, None)) 
      else raise (Failure "Precondition error.")
    )
  )
let main = 
  input_file
  |> Stdio.In_channel.read_lines
  |> List.map ~f:(fun s -> String.to_list s |> S.of_list) 
  |> List.fold ~init:(0, (None, None)) ~f:process_line
  |> fst
  |> Int.to_string
  |> Stdio.Out_channel.print_endline