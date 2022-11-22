open Ast

(* aux - an auxiliary function 
    curr: current indentation level
    tks: lexer token inputs
    out: scanner token outputs stacked
    stack: stack of history indentation levels *)
(* let rec indent curr tks out stack = 
  match tks with
  (* no further input, return output stack in reverse *)
  | [] -> (curr, stack, List.rev out)
  (* indentation not done, keep scanning for TAB *)
  | Chzapparse.TAB :: Chzapparse.TAB :: t -> aux (curr + 1) (Chzapparse.TAB :: t) out stack
  (* indentation done, start scanning other tokens *)
  | Chzapparse.TAB :: t -> aux curr t out stack
  (* acutal tokens *)
  | h :: t -> 
    (* same level, do nothing *)
    if Stack.top stack = curr then aux curr t (a :: out) stack
    (* higher level, add INDENT *)
    else if Stack.top stack < curr then 
      let _ = ignore(Stack.push curr stack) in aux curr (a :: t) (Chzapparse.INDENT :: out) stack
    (* lower level, add DEDENT *)
    else if Stack.top stack < curr then 
      (* dedent all levels until curr *)
      let rec dedent out curr stack = 
        if curr < Stack.top stack then 
          let _ = Stack.pop stack in dedent (Chzapparse.DEDENT :: out) curr stack
        else (out, stack) in
      let (t, s) = dedent [] curr stack in
      aux curr (a :: t) (t @ out) stack
    else raise (Failure "Indentation Error") *)

let _ =
  let lexbuf = Lexing.from_channel stdin in
  let program = Chzapparse.program Scanner.token lexbuf in
  print_endline (string_of_program program)
