open Core 

let input_file = "day2.input"

module RPS = struct 
  module Play = struct
    type t = Rock | Paper | Scissors
    let to_int play = 
      match play with 
        Rock -> 1
      | Paper -> 2 
      | Scissors -> 3
    let of_char c = 
      match c with 
        'A' -> Rock
      | 'B' -> Paper 
      | 'C' -> Scissors
      | _ -> raise (Failure "Unknown character.")
  end
  
  module Result = struct 
    type t = Lose | Draw | Win
    let to_int res =
      match res with 
        Lose -> 0
      | Draw -> 3 
      | Win -> 6
    let of_char c = 
      match c with 
        'X' -> Lose
      | 'Y' -> Draw 
      | 'Z' -> Win
      | _ -> raise (Failure "Unknown character.")  
  end

  let get_play opp res = 
    match res with 
      Result.Lose -> (
        match opp with 
          Play.Rock -> Play.Scissors
        | Play.Scissors -> Play.Paper 
        | Play.Paper -> Play.Rock
      )
    | Result.Draw -> opp
    | Result.Win -> (
        match opp with 
          Play.Rock -> Play.Paper
        | Play.Scissors -> Play.Rock
        | Play.Paper -> Play.Scissors
    )

end

let process_line score line = 
  let opp = String.get line 0 |> RPS.Play.of_char in 
  let res = String.get line 2 |> RPS.Result.of_char in (* format of input is "_ _" *)
  score + RPS.((get_play opp res |> Play.to_int) + Result.to_int res)

let main = 
  let score = Stdio.In_channel.(fold_lines (create input_file) ~init:0 ~f:process_line) in
  Stdio.Out_channel.print_endline (Int.to_string score)