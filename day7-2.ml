open Core 

let input_file = "day7.input"

let total_space_available = 70000000
let required_space = 30000000

module FileSystem = 
struct
  type t = Dir of string * int * t list | File of string * int

  let empty (root: string): t = Dir (root, 0, [])

  let file (name: string) (size: int): t = File (name, size)

  let dir (name: string) = Dir (name, 0, [])

  let rec populate_dir (fs: t) (target_path: string list) (target_contents: t list): t = 
    match target_path with 
      [] -> raise (Failure "Precondition error.")
    | target::path -> (
      match fs with
        Dir (name, size, contents) -> (
          if String.equal name target then
            (* Found the right directory. *)
            if List.is_empty path then 
              (* We are at the target. Assume that the file system is static, so every `ls` in the same directory will result in the same list of contents. *)
              Dir (name, size, target_contents)
            else
              (* Not yet at the target, so recurse further down to find the target. *)
              Dir (name, size, List.map contents ~f:(fun sub -> populate_dir sub path target_contents))
          else
            (* Wrong path, ignore this. *)
            fs
        )
      | File _ -> fs
    )
  
  let rec compute_size (fs: t): int * t =
    match fs with 
      File (_, sz) -> (sz, fs)
    | Dir (name, _, contents) -> 
      let (sz, contents') = List.fold_map contents ~init:0 ~f:(fun sz sub -> let (subsz, sub') = compute_size sub in (sz + subsz, sub')) in
      (sz, Dir (name, sz, contents'))
end

module Prompt = 
struct 
  type t = Down of string | Up | Ls of FileSystem.t list

  let rec make_prompts (prompts: string list): t list = 
    match prompts with 
      [] -> []
    | cmd::cmds -> (
      let tokens = String.split ~on:' ' cmd in
      match tokens with 
        "$"::"cd"::".."::[] -> Up :: make_prompts cmds
      | "$"::"cd"::dir::[] -> Down dir :: make_prompts cmds
      | "$"::"ls"::[] -> 
        let (ls, cmds') = List.split_while cmds ~f:(fun line -> not (Char.equal '$' (String.get line 0))) in
        Ls (List.map ls ~f:(fun item ->
          let tokens = String.split ~on:' ' item in
          match tokens with 
            "dir"::dir::[] -> FileSystem.dir dir
          | size::name::[] -> FileSystem.file name (Int.of_string size)
          | _ -> raise (Failure "Precondition error.")
        )) :: make_prompts cmds'
      | _ -> raise (Failure "Precondition error.")
    )
end

let rec prompts_to_filesystem (prompts: Prompt.t list): FileSystem.t =
  match prompts with 
    (Prompt.Down root)::ps -> (
      List.fold ps ~init:(FileSystem.dir root, [root]) ~f:(fun (fs, path) p -> 
        match p with 
          Prompt.Up -> (fs, List.drop_last path |> Option.value ~default:[])
        | Prompt.Down dir -> (fs, List.append path [dir])
        | Prompt.Ls contents -> (FileSystem.populate_dir fs path contents, path)
      )
      |> fst
    )
  | _ -> raise (Failure "Precondition error.")

let fs_get_best_dir (fs_size, fs) =
  let size_to_delete = required_space - (total_space_available - fs_size) in 
  let rec fs_get_best_dir' fs =
    match fs with
      FileSystem.File _ -> fs_size (* high number to ignore *)
    | FileSystem.Dir (_, sz, contents) -> 
      List.fold contents ~init:(if sz < size_to_delete then fs_size else sz) ~f:(fun sz sub -> Int.min sz (fs_get_best_dir' sub))
  in
  fs_get_best_dir' fs

let find_best_deletion input =
  input
  |> Prompt.make_prompts
  |> prompts_to_filesystem 
  |> FileSystem.compute_size
  |> fs_get_best_dir

let main = 
  input_file
  |> Stdio.In_channel.read_lines
  |> find_best_deletion
  |> Int.to_string
  |> Stdio.Out_channel.print_endline