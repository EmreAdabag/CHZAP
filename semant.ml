(* Semantic checking for the MicroC compiler *)
open Ast
open Sast

(* module StringMap = Map.Make(String) *)
type tbl_typ = (string, typ) Hashtbl.t

(* Semantic checking of the AST. Returns an SAST if successful,
   throws an exception if something is wrong.
   Check each global variable, then check each function *)

let check (program : stmt list) =

  (* let built_in_decls = Hashtbl.create 1000 in *)
  (* let function_decls = Hashtbl.create 1000 in *)
  let (globals : tbl_typ) = Hashtbl.create 1000 in
  let (locals : tbl_typ) = Hashtbl.create 1000 in

  (* Collect function declarations for built-in functions: no bodies *)

  (* Return a variable from an input hash table *)
  let type_of_identifier (s : string) (globalvars : tbl_typ) (localvars : tbl_typ) : typ = 
    if Hashtbl.mem localvars s then Hashtbl.find localvars s else
    try Hashtbl.find globalvars s
    with Not_found -> raise (Failure ("undeclared identifier " ^ s))
  in

  (* Return a list of types from a list of bindings *)
  let rec types_of_binds (binds : bind list) : typ list =
    match binds with
    | [] -> []
    | Bind(x, _) :: t -> x :: types_of_binds t
  in


  (* Raise an exception if the given rvalue type cannot be assigned to
  the given lvalue type *)
  let check_assign (lvaluet : typ) (rvaluet : typ) (err : string) (is_bind : bool) : typ =
    match lvaluet, rvaluet with
    (* lvalue is const while rvalue is non-const *)
    | Const(t1), t2 -> 
      if is_bind = true then match t1 with
        Int | Bool | Char | Float | Void -> if t2 = t1 then Const(t1) else raise (Failure err)
        | _ -> raise (Failure err)
      else raise (Failure err)
    (* regular *)
    | _ when lvaluet = rvaluet -> lvaluet 
    (* arrays *)
    | _, Arr(Void, _) -> lvaluet
    (* | _, Arr(Void, _) -> lvaluet *)
    (* error *)
    | _ -> raise (Failure err)
  in

  (* return a semantically checked statement list *)
  let rec check_stmt_list (s : stmt list) (globalvars : tbl_typ) (localvars : tbl_typ) (rettyp : typ) : sstmt list =
    match s with
    | [] -> []
    (* Flatten blocks *)
    (* | Block sl :: sl'  -> check_stmt_list (sl @ sl') globalvars localvars rettyp *)
    | s :: sl -> 
      let a = check_stmt s globalvars localvars rettyp in 
      a :: check_stmt_list sl globalvars localvars rettyp 

  (* Returns (semantically-checked statement, map of t i.e. containing sexprs *)
  and check_stmt (s : stmt) (globalvars : tbl_typ) (localvars : tbl_typ) (rettyp : typ) : sstmt =
    match s with
    (* A block is correct if each statement is correct and nothing
      follows any Return statement.  Nested blocks are flattened. *)
    | Block(b) -> SBlock(check_stmt_list b globalvars localvars rettyp)
    | Bstmt(Bind(t, id)) -> 
      if Hashtbl.mem localvars id then raise(Failure ("duplicate variable declaration: " ^ id))
      else let _ = Hashtbl.add localvars id t in
      SBstmt(Bind(t, id))
    | BAIstmt(t, id, e) -> 
      let (t', _) = check_expr e globalvars localvars in
      let b = (match t with
        Const_auto -> Bind(Const(t'), id)
        | _ -> Bind(t', id)
      ) in
      (check_stmt (BAstmt (b, e)) globalvars localvars Void)
    | BAstmt(b, e) -> 
      let Bind(t, id) = b in
      let _ = check_stmt (Bstmt(b)) globalvars localvars Void in
      let _, sx = match t with
        Const(t) -> check_expr (CAssign(id, e)) globalvars localvars 
        | _ -> check_expr (Assign(id, e)) globalvars localvars 
      in
      SBAstmt(b, sx)
    | Expr(e) -> SExpr(check_expr e globalvars localvars)
    | If(e, st1, st2) ->
      SIf(check_bool_expr e globalvars localvars, check_stmt st1 globalvars localvars rettyp, check_stmt st2 globalvars localvars rettyp)
    | While(e, st) ->
      SWhile(check_bool_expr e globalvars localvars, check_stmt st globalvars localvars rettyp)
    | For(s1, e2, e3, st) ->
      let ss = check_stmt s1 globalvars localvars Void in
      SFor(ss, check_expr e2 globalvars localvars, check_expr e3 globalvars localvars, check_stmt st globalvars localvars rettyp)
    | Continue -> SContinue
    | Break -> SBreak
    | Assert(e) -> SAssert(check_bool_expr e globalvars localvars)
    | Func(b, bl, s) as f -> check_func f globalvars localvars
    | Return e -> 
      let (t, e') = check_expr e globalvars localvars in
      if t = rettyp then SReturn (t, e')
      else raise (Failure ("return gives " ^ string_of_typ t ^ " expected " ^
                  string_of_typ rettyp ^ " in " ^ string_of_expr e))

  (* Return a semantically-checked expression, i.e., with a type *)
  and check_expr (ex : expr) (globalvars : tbl_typ) (localvars : tbl_typ) : sexpr =
    match ex with
    | IntLit l -> (Int, SIntLit l)
    | BoolLit l -> (Bool, SBoolLit l)
    | CharLit l -> (Char, SCharLit l)
    | StringLit l -> (String, SStringLit l)
    | FloatLit l -> (Float, SFloatLit l)
    | ArrayLit l -> 
      let res = match l with
      | [] -> (Arr(Void, 0), SArrayLit [])          (* empty list literal *)
      | hd::tl ->
          let typecheck typ expr = 
            let t, e' = check_expr expr globalvars localvars in
            if t != typ then 
              let err = "inconsistent array " ^ string_of_expr (ArrayLit l) in
              raise(Failure err)
            else (t, e')
          in
          let hd_type, _ = check_expr hd globalvars localvars in
          let listcheck = typecheck hd_type in
          let size = List.length tl + 1 in
          (Arr(hd_type, size), SArrayLit (List.map listcheck l))
        in res
    | Id var -> (type_of_identifier var globalvars localvars, SId var)
    | Assign(var, e) as ex ->
      let ty = type_of_identifier var globalvars localvars
      and (rt, e') = check_expr e globalvars localvars in
      let err = "illegal assignment " ^ string_of_typ ty ^ " = " ^
                string_of_typ rt ^ " in " ^ string_of_expr ex
      in
      (check_assign ty rt err false, SAssign(var, (rt, e')))
    | CAssign(var, e) as ex ->
      let ty = type_of_identifier var globalvars localvars
      and (rt, e') = check_expr e globalvars localvars in
      let err = "illegal assignment " ^ string_of_typ ty ^ " = " ^
                string_of_typ rt ^ " in " ^ string_of_expr ex
      in
      (check_assign ty rt err true, SAssign(var, (rt, e')))
    | Unop(op, e) -> 
      let t, e' = check_expr e globalvars localvars in 
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
      let (t1, e1') = check_expr e1 globalvars localvars
      and (t2, e2') = check_expr e2 globalvars localvars in
      let err = "illegal binary operator " ^
                string_of_typ t1 ^ " " ^ string_of_op op ^ " " ^
                string_of_typ t2 ^ " in " ^ string_of_expr e
      in
      (* All binary operators require operands of the same type*)
      if t1 = t2 then
        (* Determine expression type based on operator and operand types *)
        let t = match op with
            Add | Sub | Mul | Div | Mod | BWAnd | BWOr when t1 = Int -> Int
          | Add | Sub | Mul | Div when t1 = Float -> Float
          | Eq | Neq | Less | Greater | Geq | Leq when t1 = Int || t1 = Float || t1 = Bool -> Bool
          | And | Or when t1 = Bool -> Bool
          | _ -> raise (Failure err)
        in
        (t, SBinop((t1, e1'), op, (t2, e2')))
      else
        let t = match op with
          Add | Sub | Mul | Div when t1 = Float && t2 = Int -> Float
          | Add | Sub | Mul | Div when t1 = Int && t2 = Float -> Float
          | Eq | Neq | Less | Greater | Geq | Leq when 
            (t1 = Int || t1 = Float || t1 = Bool) && (t2 = Int || t2 = Float || t2 = Bool) -> Bool
          | _ -> raise (Failure err)
        in
        (t, SBinop((t1, e1'), op, (t2, e2')))
    | Call(fname, args) as call -> check_call fname args call globalvars localvars
    | Noexpr -> (Int, SNoexpr)
    | Subscription(a, e) -> 
      let (t', e') = check_expr e globalvars localvars in
      if t' != Int then 
        let err = "array indicies must be integers, not " ^ string_of_typ t' in
        raise(Failure err)
      else
      (Int, SSubscription(a, (Int, e'))) (* TODO... I think this is done? *)
    | Afunc(rt, bl, s) as f -> check_afunc f globalvars localvars

  and check_bool_expr e globalvars localvars =
    let (t, e') = check_expr e globalvars localvars in
    match t with
    | Bool -> (t, e')
    |  _ -> raise (Failure ("expected Boolean expression in " ^ string_of_expr e))

  and check_call fname args call globalvars localvars = 
      match fname with
      | "print" -> 
          let sargs = List.map (fun e -> check_expr e globalvars localvars) args in
          (Int, SCall("print", sargs))
      | _ ->
        let ty = 
          if Hashtbl.mem localvars fname then Hashtbl.find localvars fname
          else if Hashtbl.mem globalvars fname then Hashtbl.find globalvars fname
          else raise (Failure ("unrecognized function " ^ fname)) 
        in 
          match ty with
          | Ftyp(rt, tl) -> 
            if List.length args != List.length tl then 
              raise (Failure ("expecting " ^ string_of_int (List.length args) ^
              " arguments in " ^ string_of_expr call))
            else let check_c ft e =
              let (et, e') = check_expr e globalvars localvars in
              let err = "illegal argument found " ^ string_of_typ et ^
                        " expected " ^ string_of_typ ft ^ " in " ^ string_of_expr e
              in (check_assign ft et err false, e')
            in
            let args' = List.map2 check_c tl args
            in (rt, SCall(fname, args'))
          | _ -> raise(Failure "invalid call")

  (* return semantically checked function *)
  and check_func func (globalvars : tbl_typ) (localvars : tbl_typ) : sstmt =
    match func with
    | Func(Bind(rt, fname), formals, body) -> 
      let _ = Hashtbl.add globalvars fname (Ftyp(rt, types_of_binds formals)) in
      SFunc(Bind(rt, fname), formals, check_func_decl formals body rt globalvars localvars)
    | _ -> raise (Failure ("error parsing function"))

  and check_afunc func (globalvars : tbl_typ) (localvars : tbl_typ) : sexpr =
    match func with
    | Afunc(rt, formals, body) -> 
      (Ftyp(rt, types_of_binds formals), SAfunc(rt, formals, check_func_decl formals body rt globalvars localvars))
    | _ -> raise (Failure ("error parsing anonymous function"))

  and check_func_decl (formals : bind list) (body : stmt) (rt : typ) (globalvars : tbl_typ) (localvars : tbl_typ) : sstmt = 
    (* create a new "global" scope with variables outside of current scope *)
    let globals = Hashtbl.copy globalvars in
    let add_fn k v = Hashtbl.add globals k v in
    let _ = Hashtbl.iter add_fn localvars in
    (* create local scope and fill with formals, locally scoped vars can't be redefined but globals can *)
    let locals = Hashtbl.create 1000 in
    let _ = List.map (fun (Bind((ty : typ), name)) -> Hashtbl.add locals name ty) formals in
    check_stmt body globals locals rt

  in check_stmt_list program globals locals Void 
