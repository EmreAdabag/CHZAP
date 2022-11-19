(* Semantic checking for the MicroC compiler *)
open Ast
open Sast

module StringMap = Map.Make(String)

(* Semantic checking of the AST. Returns an SAST if successful,
   throws an exception if something is wrong.
   Check each global variable, then check each function *)

let check program =

  let built_in_decls = Hashtbl.create 1000 in
  let function_decls = Hashtbl.create 1000 in
  let globals = Hashtbl.create 1000 in

  (* Verify a list of bindings has no duplicate names *)
  let check_binds (kind : string) (binds : (typ * string) list )=
    let rec dups = function
        [] -> ()
      |	((_,n1) :: (_,n2) :: _) when n1 = n2 ->
        raise (Failure ("duplicate " ^ kind ^ " " ^ n1))
      | _ :: t -> dups t
    in dups (List.sort (fun (_,a) (_,b) -> compare a b) binds)
  in

  (* Collect function declarations for built-in functions: no bodies *)
  
  Hashtbl.add built_in_decls "print" {
    rtyp = Int;
    fname = "print";
    formals = [(Int, "x")]; 
    body = Expr(Noexpr) };
  
  Hashtbl.add function_decls "print" {
    rtyp = Int;
    fname = "print";
    formals = [(Int, "x")]; 
    body = Expr(Noexpr) };

  (* Add function name to symbol table *)
  let add_func fd =
    let built_in_err = "function " ^ fd.fname ^ " may not be defined"
    and dup_err = "duplicate function " ^ fd.fname
    and make_err er = raise (Failure er)
    and n = fd.fname (* Name of the function *)
    in match fd with (* No duplicate functions or redefinitions of built-ins *)
      _ when Hashtbl.mem built_in_decls n -> make_err built_in_err
    | _ when Hashtbl.mem function_decls n -> make_err dup_err
    | _ ->  Hashtbl.add function_decls n fd; true 
  in

  (* Collect all function names into one symbol table *)

  (* Return a variable from our local symbol table *)
  let type_of_identifier s symbols =
    if Hashtbl.mem globals s then
      Hashtbl.find globals s else
    try Hashtbl.find symbols s
    with Not_found -> raise (Failure ("undeclared identifier " ^ s))
  in

  (* Return a function from our symbol table *)
  let find_func s =
    if Hashtbl.mem function_decls s then
      Hashtbl.find function_decls s
    else try Hashtbl.find built_in_decls s
    with Not_found -> raise (Failure ("unrecognized function " ^ s))
  in

(*  let _ = find_func "main" in (* Ensure "main" is defined *)*)


  (* Raise an exception if the given rvalue type cannot be assigned to
  the given lvalue type *)
  let check_assign lvaluet rvaluet err =
    if lvaluet = rvaluet then lvaluet 
    else if rvaluet = Arr Void then lvaluet
    else raise (Failure err)
  in

  


   (* Return a semantically-checked expression, i.e., with a type *)
  let rec check_expr ex svars =
    match ex with
    IntLit l -> (Int, SIntLit l)
    | BoolLit l -> (Bool, SBoolLit l)
    | CharLit l -> (Char, SCharLit l)
    | FloatLit l -> (Float, SFloatLit l)
    | ArrayLit l -> 
      let res = match l with
      | [] -> (Arr Void, SArrayLit [])          (* empty list literal *)
      | hd::tl ->
          let typecheck typ expr = 
            let t, e' = check_expr expr svars in
            if t != typ then 
              let err = "inconsistent array " ^ string_of_expr (ArrayLit l) in
              raise(Failure err)
            else (t, e')
          in
          let hd_type, _ = check_expr hd svars in
          let listcheck = typecheck hd_type in
          (Arr hd_type, SArrayLit (List.map listcheck l))
        in res
    | Id var -> (type_of_identifier var svars, SId var)
    | Assign(var, e) as ex ->
      let lt = type_of_identifier var svars
      and (rt, e') = check_expr e svars in
      let err = "illegal assignment " ^ string_of_typ lt ^ " = " ^
                string_of_typ rt ^ " in " ^ string_of_expr ex
      in
      (check_assign lt rt err, SAssign(var, (rt, e')))

    | Unop(op, e) -> 
      let t, e' = check_expr e svars in 
      let err = "illegal unary operator " ^ 
                string_of_uop op ^ string_of_typ t ^ " in " ^ 
                string_of_expr e
      in
      let ty = match op with
        | Not when t = Bool -> t
        | _ -> raise (Failure err)
      in
      (ty, SUnop(op, (t, e')))
    | Binop(e1, op, e2) as e ->
      let (t1, e1') = check_expr e1 svars
      and (t2, e2') = check_expr e2 svars in
      let err = "illegal binary operator " ^
                string_of_typ t1 ^ " " ^ string_of_op op ^ " " ^
                string_of_typ t2 ^ " in " ^ string_of_expr e
      in
      (* All binary operators require operands of the same type*)
      if t1 = t2 then
        (* Determine expression type based on operator and operand types *)
        let t = match op with
            Add | Sub | Mul | Div | Mod | BWAnd | BWOr | Exp when t1 = Int -> Int
          | Add | Sub | Mul | Div when t1 = Float -> Float
          | Eq | Neq | Less | Greater | Geq | Leq when t1 = Int || t1 = Float || t1 = Bool -> Bool
          | And | Or when t1 = Bool -> Bool
          | _ -> raise (Failure err)
        in
        (t, SBinop((t1, e1'), op, (t2, e2')))
      else
        let t = match op with
          Add | Sub | Mul | Div | Exp when t1 = Float && t2 = Int -> Float
          | Add | Sub | Mul | Div when t1 = Int && t2 = Float -> Float
          | Eq | Neq | Less | Greater | Geq | Leq when 
            (t1 = Int || t1 = Float || t1 = Bool) && (t2 = Int || t2 = Float || t2 = Bool) -> Bool
          | _ -> raise (Failure err)
        in
        (t, SBinop((t1, e1'), op, (t2, e2')))
    | Call(fname, args) as call ->
      let fd = find_func fname in
      let param_length = List.length fd.formals in
      if List.length args != param_length then
        raise (Failure ("expecting " ^ string_of_int param_length ^
                        " arguments in " ^ string_of_expr call))
      else let check_call (ft, _) e =
              let (et, e') = check_expr e svars in
              let err = "illegal argument found " ^ string_of_typ et ^
                        " expected " ^ string_of_typ ft ^ " in " ^ string_of_expr e
              in (check_assign ft et err, e')
        in
        let args' = List.map2 check_call fd.formals args
        in (fd.rtyp, SCall(fname, args'))
    | Noexpr -> (Int, SNoexpr)
    | Subscription(a, e) -> 
      let (t', e') = check_expr e svars in
      if t' != Int then 
        let err = "array indicies must be integers, not " ^ string_of_typ t' in
        raise(Failure err)
      else
      (Int, SSubscription(a, (Int, e'))) (* TODO... I think this is done? *)
  
  and check_bool_expr e svars=
    let (t, e') = check_expr e svars in
    match t with
    | Bool -> (t, e')
    |  _ -> raise (Failure ("expected Boolean expression in " ^ string_of_expr e))
  in

  (* return a semantically checked statement list *)
  let rec check_stmt_list s svars =
    match s with
      [] -> []
    | Block sl :: sl'  -> check_stmt_list (sl @ sl') svars (* Flatten blocks *)
    | s :: sl -> check_stmt s svars :: check_stmt_list sl svars
  
  
  (* Returns (semantically-checked statement, map of t i.e. containing sexprs *)
  and check_stmt (s:stmt) svars =
    match s with
    (* A block is correct if each statement is correct and nothing
      follows any Return statement.  Nested blocks are flattened. *)
    | Expr e -> SExpr (check_expr e svars)
    | Bstmt(t, id) -> 
      let _ = match id with
      | _ when Hashtbl.mem svars id -> raise(Failure "duplicate variable declaration")
      | _ -> Hashtbl.add svars id t
      in SBstmt(t, id)
    | If(e, st1, st2) ->
      SIf(check_bool_expr e svars, check_stmt st1 svars, check_stmt st2 svars)
    | While(e, st) ->
      SWhile(check_bool_expr e svars, check_stmt st svars)
    | For(e1, e2, e3, st) ->
      SFor(check_expr e1 svars, check_expr e2 svars, check_expr e3 svars, check_stmt st svars)
    | Continue -> SContinue
    | Break -> SBreak
    | Return e -> 
      let (t, e') = check_expr e svars in
      SReturn(t, e')
      (* TODO: need to check that return val matches declared return val *)
      (* if t = func.rtyp then SReturn (t, e')
      else raise (
          Failure ("return gives " ^ string_of_typ t ^ " expected " ^
                  string_of_typ func.rtyp ^ " in " ^ string_of_expr e)) *)
    | Func(f) -> check_func f svars

  (* return semantically checked function *)
  and check_func func svars =
      
    (* add function name to set of function names *)
    let _ = add_func func in

    (* Make sure no formals are void or duplicates *)
    let _ = check_binds "formal" func.formals in

    let symbols = Hashtbl.copy svars in

    (* Build local symbol table of variables for this function *)
    let _ = List.map (fun (ty, name) -> Hashtbl.add symbols name ty) (func.formals ) 

  in
    (* body of check_func *)
    SFunc({ srtyp = func.rtyp;
      sfname = func.fname;
      sformals = func.formals;
      sbody = check_stmt func.body symbols;
    })
  in check_stmt_list program globals