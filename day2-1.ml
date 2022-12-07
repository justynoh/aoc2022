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
        'A' | 'X' -> Rock
      | 'B' | 'Y' -> Paper 
      | 'C' | 'Z' -> Scissors
      | _ -> raise (Failure "Unknown character.")
  end
  
  module Result = struct 
    type t = Lose | Draw | Win
    let to_int res =
      match res with 
        Lose -> 0
      | Draw -> 3 
      | Win -> 6
  end

  let get_result opp self = 
    match (opp, self) with 
      (Play.Rock, Play.Scissors) | (Play.Scissors, Play.Paper) | (Play.Paper, Play.Rock) -> Result.Lose
    | (Play.Rock, Play.Rock) | (Play.Scissors, Play.Scissors) | (Play.Paper, Play.Paper) -> Result.Draw
    | (Play.Rock, Play.Paper) | (Play.Scissors, Play.Rock) | (Play.Paper, Play.Scissors) -> Result.Win

end

let process_line score line = 
  let get_play c = String.get line c |> RPS.Play.of_char in
  let (opp, self) = (get_play 0, get_play 2) in (* format of input is "_ _" *)
  score + RPS.(Play.to_int self + (get_result opp self |> Result.to_int))

let main = 
  let score = Stdio.In_channel.(fold_lines (create input_file) ~init:0 ~f:process_line) in
  Stdio.Out_channel.print_endline (Int.to_string score)