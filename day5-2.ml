open Core 

let input_file = "day5.input"

module Config =
struct 
  type t = char list array 

  let construct (lines: string list): t = 
    match lines with 
      [] -> raise (Failure "Precondition error.")
    | nums::boxes -> (
      (* Get the number of columns from the first row *)
      let cols = nums |> String.split ~on:' ' |> List.count ~f:(fun s -> not (String.is_empty s)) in
      let config = Array.init cols ~f:(const []) in
      let addRow line =
        for col = 0 to cols - 1 do
          let idx = 4 * col + 1 in
          let c = if String.length line > idx then String.get line idx else ' ' in
          if not (Char.equal c ' ') then config.(col) <- c::config.(col)
        done
      in
      List.iter boxes ~f:addRow;
      config
    )

  let move (config: t) ((num, src, dst): int * int * int) : unit = 
    let rec move' num = if not (num = 0) then (
      match config.(src) with 
      [] -> raise (Failure "Precondition error.")
    | c::cs -> (
      config.(src) <- cs;
      move' (num - 1);
      config.(dst) <- c::config.(dst)
    )) in 
    move' num 

  let getTop (config: t): string = 
    Array.filter_map config ~f:List.hd |> Array.to_list |> String.of_char_list
end

let split_input: string list -> string list * string list = 
  let rec split_input' block1 block2 =
    match block2 with 
      [] -> (block1, block2)
    | l::ls -> if String.equal l "" then (block1, ls) else split_input' (l::block1) ls
  in
  split_input' []

let construct_and_simulate (config, actions) = 
  let config = Config.construct config in
  let actions = List.filter_map actions ~f:(fun action ->
    if String.is_empty action then None else
    let words = String.split ~on:' ' action in
    let num = List.nth_exn words 1 |> Int.of_string in 
    let src = List.nth_exn words 3 |> Int.of_string in 
    let dst = List.nth_exn words 5 |> Int.of_string in 
    Some (num, src - 1, dst - 1)
  ) in
  let move = Config.move config in
  List.iter actions ~f:move;
  Config.getTop config

let main = 
  input_file
  |> Stdio.In_channel.read_lines
  |> split_input 
  |> construct_and_simulate
  |> Stdio.Out_channel.print_endline