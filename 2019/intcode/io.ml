open Utils

let read_program (fname : string) : Computer.program_t =
  Io.read_file fname |> List.of_string ~sep:','
