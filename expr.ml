(* 
                         CS 51 Final Project
                        MiniML -- Expressions
*)

(*......................................................................
  Abstract syntax of MiniML expressions 
 *)

type unop =
  | Negate_i 
  | Negate_f (* new *)
  | Not (* new *)
;;
    
type binop =
  | Plus_i 
  | Plus_f (* new *)
  | Minus_i
  | Minus_f (* new *)
  | Times_i
  | Times_f (* new *)
  | Divide_i (* new *)
  | Divide_f (* new *)
  | Power_i (* new *)
  | Power_f (* new *)
  | Equals
  | LessThan
  | GreaterThan (* new *)
  | LessOrEqual (* new *)
  | GreaterOrEqual (* new *)
  | Unequal (* new *)
;;

type varid = string ;;
  
type expr =
  | Var of varid                         (* variables *)
  | Num of int                           (* integers *)
  | Float of float (* new *)             (* floats *)
  | Bool of bool                         (* booleans *)
  | Unop of unop * expr                  (* unary operators *)
  | Binop of binop * expr * expr         (* binary operators *)
  | Conditional of expr * expr * expr    (* if then else *)
  | Fun of varid * expr                  (* function definitions *)
  | Let of varid * expr * expr           (* local naming *)
  | Letrec of varid * expr * expr        (* recursive local naming *)
  | Raise                                (* exceptions *)
  | Unassigned                           (* (temporarily) unassigned *)
  | App of expr * expr                   (* function applications *)
;;
  

(*......................................................................
  Manipulation of variable names (varids) and sets of them
 *)

(* varidset -- Sets of varids *)
module SS = Set.Make (struct
                       type t = varid
                       let compare = String.compare
                     end ) ;;

type varidset = SS.t ;;


(* same_vars varids1 varids2 -- Tests to see if two `varid` sets have
   the same elements (for testing purposes) *)
let same_vars : varidset -> varidset -> bool =
  SS.equal;;


(* vars_of_list varids -- Generates a set of variable names from a
   list of `varid`s (for testing purposes) *)
let vars_of_list : string list -> varidset =
  SS.of_list ;;
  

(* free_vars exp -- Returns the set of `varid`s corresponding to free
   variables in `exp` *)
let rec free_vars (exp : expr) : varidset =
  match exp with 
  | Var varid -> SS.singleton varid
  | Num _int -> SS.empty
  | Float _float -> SS.empty
  | Bool _bool -> SS.empty
  | Unop (_unop, expr) -> free_vars expr
  | Binop (_binop, expr1, expr2) -> SS.union (free_vars expr1)
                                             (free_vars expr2)
  | Conditional (expr1, expr2, expr3) -> 
      SS.union (free_vars expr1) 
               (SS.union (free_vars expr2) 
                         (free_vars expr3))
  | Fun (name, expr) -> SS.diff (free_vars expr) 
                                (SS.singleton name)
  | Let (name, def, body) -> SS.union (free_vars def)
                                      (SS.diff (free_vars body) 
                                               (SS.singleton name))
  | Letrec (name, def, body) -> SS.diff (SS.union (free_vars def) 
                                                  (free_vars body))
                                        (SS.singleton name)
  | Raise -> SS.empty
  | Unassigned -> SS.singleton "Unassigned"
  | App (expr1, expr2) -> SS.union (free_vars expr1) 
                                   (free_vars expr2) ;;
  

(* new_varname () -- Returns a freshly minted `varid` constructed with
   a running counter a la `gensym`. Assumes no variable names use the
   prefix "var". (Otherwise, they might accidentally be the same as a
   generated variable name.) *)
let new_varname =
  let ctr = ref ~-1 in 
  fun () ->
    ctr := !ctr + 1 ;
    "var" ^ string_of_int (!ctr) ;;


(*......................................................................
  Substitution 

  Substitution of expressions for free occurrences of variables is the
  cornerstone of the substitution model for functional programming
  semantics.
 *)

(* subst var_name repl exp -- Return the expression `exp` with `repl`
   substituted for free occurrences of `var_name`, avoiding variable
   capture *)
let rec subst (var_name : varid) (repl : expr) (exp : expr) : expr =
 (* shorthand abstractions for operations used in multiptle match cases *)
  let sub (exp : expr) : expr = subst var_name repl exp in 
  let not_freevar (name : string) : bool = not(SS.mem name (free_vars exp)) in
  let newname = new_varname () in 
  let sub_new (name : string) (e : expr) = subst name (Var newname) e in 

    (* var_name ∉ free_vars of exp, so nothing to be done *)
    if not_freevar var_name then exp 
    else 
      match exp with 
      | Var varid -> if varid = var_name 
                      then repl (* match vars with var to be substituted *)
                     else exp (* otherwise ignore *)
      | Num _ |Float _ | Bool _ | Raise | Unassigned -> exp
      | Unop (unop, expr) -> Unop (unop, sub expr)
      | Binop (binop, expr1, expr2) -> Binop (binop, sub expr1, sub expr2)
      | Conditional (ex1, ex2, ex3) -> Conditional (sub ex1, sub ex2, sub ex3)
      | Fun (name, expr) -> if name = var_name 
                              then exp
                            else if not_freevar name
                              then Fun (name, sub expr)
                            else Fun (newname, sub_new name expr)
      | Let (name, def, body) -> if name = var_name 
                                   then Let (name, sub def, body)
                                 else if not_freevar name 
                                   then Let (name, sub def, sub body)
                                 else Let (name, sub def, sub_new name body)
      | Letrec (name, def, body) -> if name = var_name 
                                      then Letrec (name, def, body)
                                    else if not_freevar name 
                                      then Letrec (name, sub def, sub body)
                                    else Letrec (newname,
                                                 sub (sub_new name def), 
                                                 sub (sub_new name body))
      | App (expr1, expr2) -> App (sub expr1, sub expr2) ;;
     

(*......................................................................
  String representations of expressions
 *)

(* exp_to_concrete_string exp -- Returns a string representation of
   the concrete syntax of the expression `exp` *)
let rec exp_to_concrete_string (exp : expr) : string =
  (* we include this "redundant" match statement to allow for future growth *)
  let unop_matcher (unop: unop) : string = 
    match unop with 
    | Negate_i -> "~-" 
    | Negate_f -> "~-."
    | Not -> "not "
    (* future new unops go here *) in 

  let binop_matcher (binop : binop) : string = 
    match binop with 
    | Plus_i -> " + " 
    | Plus_f -> " +. "
    | Minus_i -> " - " 
    | Minus_f -> " -. "
    | Times_i -> " * "
    | Times_f -> " *. "
    | Divide_i -> " / "
    | Divide_f -> " /. "
    | Power_i -> " ** "
    | Power_f -> " ** "
    | Equals -> " = " 
    | LessThan -> " < " 
    | GreaterThan -> " > " 
    | LessOrEqual -> " <= "
    | GreaterOrEqual -> " >= "
    | Unequal -> " <> " in 
  
  let unop_concat (u: unop) (e: expr) = 
    unop_matcher u ^ exp_to_concrete_string e in 

  let binop_concat (b: binop) (e1: expr) (e2: expr) : string =
    exp_to_concrete_string e1 ^ 
    binop_matcher b ^ 
    exp_to_concrete_string e2 in
  
  (* shorthand for Let & Letrec *)
  let f_def_concat (name : varid) (def: expr) (body: expr) : string = 
    name ^ " = " ^ 
    exp_to_concrete_string def ^ " in " ^ 
    exp_to_concrete_string body in

      match exp with 
      | Var varid -> varid
      | Num int -> string_of_int int
      | Float float -> string_of_float float
      | Bool bool -> string_of_bool bool
      | Unop (unop, expr) -> unop_concat unop expr
      | Binop (binop, expr1, expr2) -> binop_concat binop expr1 expr2
      | Conditional (cond, expr1, expr2) -> 
          "if " ^ exp_to_concrete_string cond ^
          " then " ^ exp_to_concrete_string expr1 ^
          " else " ^ exp_to_concrete_string expr2
      | Fun (name, def) -> 
          "fun " ^ name ^ " -> " ^ exp_to_concrete_string def
      | Let (name, def, body) -> "let " ^ f_def_concat name def body
      | Letrec (name, def, body) -> "let rec " ^ f_def_concat name def body
      | Raise -> "raise (Some (Exception))"
      | Unassigned -> "Some (Unassigned exp)"
      | App (expr1, expr2) -> exp_to_concrete_string expr1 ^ " (" ^ 
                              exp_to_concrete_string expr2 ^ ")"  ;;
     

(* exp_to_abstract_string exp -- Return a string representation of the
   abstract syntax of the expression `exp` *)
let rec exp_to_abstract_string (exp : expr) : string =
  (* we include this "redundant" match statement to allow for future growth *)
  let unop_matcher (unop: unop) : string = 
    match unop with 
    | Negate_i -> "Negate_i" 
    | Negate_f -> "Negate_f"
    | Not -> "Not"
    (* future new unops go here *) in 

  let binop_matcher (binop : binop) : string = 
    match binop with 
    | Plus_i -> "Plus_i" 
    | Plus_f -> "Plus_f"
    | Minus_i -> "Minus_i" 
    | Minus_f -> "Minus_f"
    | Times_i -> "Times_i"
    | Times_f -> "Times_f"
    | Divide_i -> "Divide_i"
    | Divide_f -> "Divide_f"
    | Power_i -> "Power_i"
    | Power_f -> "Power_f"
    | Equals -> "Equals" 
    | LessThan -> "LessThan" 
    | GreaterThan -> "GreaterThan" 
    | LessOrEqual -> "LessOrEqual"
    | GreaterOrEqual -> "GreaterOrEqual"
    | Unequal -> "Unequal" in  

  let unop_concat (u: unop) (e: expr) = 
    "Unop(" ^ unop_matcher u ^ ", " 
            ^ exp_to_abstract_string e ^ ")" in 

  let binop_concat (b: binop) (e1: expr) (e2: expr) : string =
    "Binop(" ^ binop_matcher b ^ ", " 
             ^ exp_to_abstract_string e1 ^ ", " 
             ^ exp_to_abstract_string e2 ^ ")" in 
  
  (* shorthand for Let & Letrec *)
  let f_def_concat (name : varid) (def: expr) (body: expr) : string = 
    name ^ ", " ^ exp_to_abstract_string def ^ ", " 
                ^ exp_to_abstract_string body ^ ")" in

      match exp with 
      | Var varid -> "Var(" ^ varid ^ ")"
      | Num int -> "Num(" ^ string_of_int int ^ ")"
      | Float float -> "Float(" ^ string_of_float float ^ ")"
      | Bool bool -> "Bool(" ^ string_of_bool bool ^ ")"
      | Unop (unop, expr) -> unop_concat unop expr
      | Binop (binop, expr1, expr2) -> binop_concat binop expr1 expr2
      | Conditional (expr1, expr2, expr3) -> 
          "Conditional(" ^ exp_to_abstract_string expr1 ^ ", " 
                         ^ exp_to_abstract_string expr2 ^ ", " 
                         ^ exp_to_abstract_string expr3 ^ ")"
      | Fun (name, expr) -> 
          "Fun(" ^ name ^ ", " ^ exp_to_abstract_string expr ^ ")"
      | Let (name, def, body) -> "Let(" ^ f_def_concat name def body
      | Letrec (name, def, body) -> "Letrec(" ^ f_def_concat name def body
      | Raise -> "Raise"
      | Unassigned -> "Unassigned"
      | App (expr1, expr2) -> "App(" ^ exp_to_abstract_string expr1 ^ ", " 
                                     ^ exp_to_abstract_string expr2 ^ ")"  ;;
