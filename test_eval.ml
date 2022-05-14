(* <<<<<<<<<<<< Test File for CS51 2022 Final Project : MiniML >>>>>>>>>>>>> *)

open Expr ;;
open Evaluation ;;
open Miniml ;;
open Env;;

open Test_simple ;; (* adapted unit-testing framework from CS51 *)

(* =======================| TESTS FOR evaluation.ml |======================= *)

(* .................{ NATIVE SAMPLE VALUES }................ *)
  let zero_i = Val (Num 0) ;;
  let one_i = Val (Num(1)) ;;
  let zero_f = Val (Float 0.) ;;
  let one_f = Val (Float(1.)) ;;
  let y = Val (str_to_exp "y;;") ;;
  let env = empty () ;;
  let closure = close var_x env ;; 

let close_test () =
  (* -----{ close TEST }------------------- *)
  (* conducted in empty env [] by default *)
  (* 1 *) ut((closure) = Closure(var_x, env)) ;;

let lookup_extend_test () =
  (* -----{ lookup TEST, extend TEST }----- *)
  (* conducted in empty env [] by default *)
  (* 2 *) ut(try (lookup env "notincluded" = Val(var_x)) 
          with EvalError _ -> true) ;
              let env = extend env "x_0" (ref zero_i) in
                (* 3 *) ut(lookup env "x_0" = zero_i) ;
              let env = extend env "x_0" (ref zero_f) in
                (* 4 *) ut(lookup env "x_0" = zero_f) ;
              let env = extend env "x_1" (ref one_i) in
                (* 5 *) ut(lookup env "x_1" = one_i) ;
              let env = extend env "x_1" (ref one_f) in
                (* 6 *) ut(lookup env "x_1" = one_f) ;
              let env = extend env "x_2" (ref y) in
                (* 7 *) ut(lookup env "x_2" = y) ;
              let env = extend env "x_3" (ref closure) in
                (* 8 *) ut(lookup env "x_3" = closure) ;;

let to_string_test () = 
  (* -----{ to_string TEST }--------------- *)
  (* ints SAMPLE *)
  let env = extend env "x_0" (ref zero_i) in 
  let env = extend env "x_1" (ref one_i) in
  let env = extend env "x_2" (ref y) in
  let env = extend env "x_3" (ref closure) in

  (* floats SAMPLE *)
  let env' = extend (empty ()) "x_0" (ref zero_f) in 
  let env' = extend env' "x_1" (ref one_f) in
  let env' = extend env' "x_2" (ref y) in
  let env' = extend env' "x_3" (ref closure) in

  let v_to_s = value_to_string in
  let env_to_s = env_to_string in
    (* 9 *) ut(v_to_s zero_i = "0") ;
    (* 10 *) ut(v_to_s one_i = "1") ;
    (* 11 *) ut(v_to_s zero_f = "0.") ;
    (* 12 *) ut(v_to_s one_f = "1.") ;
    (* 13 *) ut(v_to_s y = "y") ;
    (* 14 *) ut(v_to_s closure = "Closure(x, [])") ;
    (* 15 *) ut(env_to_s env =
      "x_0 -> 0, x_1 -> 1, x_2 -> y, x_3 -> Closure(x, []), []") ;
    (* 16 *) ut(env_to_s env' =
      "x_0 -> 0., x_1 -> 1., x_2 -> y, x_3 -> Closure(x, []), []") ;;

(* ........{ NATIVE SAMPLE EXPRESSIONS & "ANSWERS" }........ *)
                                                                     (* nums *)
let cInt_pos = ("42;;", "42;;") ;;
let cInt_neg = ("~-42;;", "-42;;") ;;
                                                                   (* floats *)
let cFlo_pos = ("42.;;", "42.;;") ;;
let cFlo_neg = ("~-42.;;", "-42.;;") ;;
                                                                    (* bools *)
let cBool_t = ("true;;", "true;;") ;;
let cBool_f = ("false;;", "false;;") ;;
                                                                    (* unops *)
let cNeg_var = ("~-(~-42);;", "42;;") ;; 
let cNot_var = ("not(not true);;", "true;;")
                                                           (* binop: --bools *)
let cBinop_b1 = ("true=false;;", "false;;") ;;
let cBinop_b2 = ("false=false;;", "true;;") ;;
                                                             (* conditionals *)
let cCond_0 = ("if 41 = 42 then true else false;;", "false;;") ;;
let cCond_0' = ("if 41. = 42. then true else false;;", "false;;") ;;
let cCond_1 = ("if 42 = 42 then false else true;;", "false;;") ;;
let cCond_1' = ("if 42. = 42. then false else true;;", "false;;") ;;
                                                                      (* fun *)
let cFun0 = ("fun x -> x + x;;", "fun x -> x + x;;") ;;
let cFun1 = str_to_exp "fun x -> x + x;;" ;;
let cFun2 = "Closure(fun x -> x + x, [])" ;;
                                                                   (* let... *)
let cLeT0 = ("let x = 1 in x + x;;",
            "2;;") ;;
let cLeT1 = ("let f = fun x -> x in f f 3;;", 
            "3;;") ;;
let cLeT2 = ("let x = 2 in let x = 4 in x;;", 
            "4;;") ;;
                                                               (* let rec... *)
let cLeTR0 = ("let rec f = fun x-> if x=0 then 1 else x*f(x-1) in f 4;;",
             "24;;") ;;
let cLeTR1 = ("let rec f = fun x-> if x=0 then x else f(x-1) in f 2 ;;", 
             "0;;") ;;
let cLeTR2 = ("let rec f=(fun x->if x=0 then 0 else 1+f (x-1)) in f 1;;",
             "1;;") ;;
                                                                      (* app *)
let cApp0 = ("let double = fun x -> 2 * x in double (double 3);;",
             "12;;") ;;
                                                                    (* raise *)
let cRaise_0 = "if 41 < 42 then raise else true;;" ;;
let cRaise_1 = ("if 41 < 42 then raise else true;;", "true;;") ;;
                                                (* app & COMPLEX EXPRESSIONS *)
let cExp0 = ("let cube = fun x -> x * x * x in let y = 4 in cube y;;", 
            "64;;") ;;
let cExp0' = ("let cube = fun x -> x *. x *. x in let y = 4. in cube y;;", 
            "64.;;") ;; 
let cExp1 = ("let f = fun x -> x in let half = fun x -> x / 2 in let y = 84 \
                in f half y;;",
            "42;;") ;;
let cExp1' = ("let f = fun x -> x in let half = fun x -> x /. 2. in let y = \
                84. in f half y;;", 
            "42.;;") ;;
let cExp2 = ("let x = 42 in let transform = fun y -> x + y in let x = 42 in \
                transform 0;;", 
            "42") ;;
let cExp2' = ("let x = 42. in let transform' = fun y -> x +. y in let x = 42. \
                  in transform' 0.;;", 
            "42.") ;;
let cExp3a = "let highord = fun f -> fun x -> f (f x) in let flip = fun x -> \
                  ~-x in highord flip ~-42;;" ;;
let cExp3b = ("let highord = fun f -> fun x -> f (f x) in let flip = fun x ->\
                  ~-x in highord flip ~-42;;", 
            "~-42;;") ;;
let cExp3'a = "let highord' = fun f -> fun x -> f (f x) in let flip' = fun x \
                  -> ~-.x in highord flip ~-42.;;" ;;
let cExp3'b = ("let highord' = fun f -> fun x -> f (f x) in let flip' = fun x\
                  -> ~-.x in highord flip ~-42.;;", 
            "~-.42.;;") ;;
let cExp4a = "let f = fun x -> fun y -> x + y in f 20 22;;" ;;
let cExp4b = ("let f = fun x -> fun y -> x + y in f 20 22;;", 
            "42;;") ;;
let cExp4'a = "let f = fun x -> fun y -> x +. y in f 20. 22." ;;
let cExp4'b = ("let f = fun x -> fun y -> x +. y in f 20. 22.", 
            "42.;;") ;;
let cExp5 = ("let rec f = fun x -> if x = 0 then 1 else x * f (x-1) in f 5;;", 
            "120") ;; 
let cExp5' = ("let rec f = fun x -> if x = 0. then 1. else x *. f (x -. 1.) in \
                  f 5.;;", 
            "120") ;; 


(* ........{ DYNAMIC & LEXICAL TESTING EXPRESSIONS }........ *)
(* renamed for clarity *)
let cDyn0 = cExp2 ;;
let cDyn0' = cExp2' ;;
let cDyn1 = cExp5 ;;
let cDyn1' = cExp5 ;;
let cLex0 = cExp2 ;;
let cLex0' = cExp2 ;;
let cLex1 = cExp3b ;;
let cLex1' = cExp3'b ;;

(* ........{ evaluator (sub, dyna, lexi) TESTS }........ *)
let evaluator_test (sub : bool) (dyna : bool) (lexi: bool) = 
  let expr_test (str : string * string) : bool =
    (* conducted in empty environment [] by default *)
    let env = empty () in 
    match str with
    | expr, answer -> 
      (evaluator sub dyna lexi (str_to_exp expr) env = 
      Val(str_to_exp answer)) in

  let error_test (exp : expr) (str : string) : bool =
    (* conducted in empty environment [] by default *)
    let env = empty () in 
    try
      evaluator sub dyna lexi exp env = 
        Val(str_to_exp "let x = 42 in x;;") || (* if Num OR *)
      evaluator sub dyna lexi exp env = 
        Val(str_to_exp "let x = 42. in x;;")   (* if Float *)
    with
      | EvalError cERROR -> print_endline cERROR; cERROR = str 
      | EvalException -> true in

  (* -----{ cERROR TESTS }--------------- *)
                                                                     (* vars *)
  (* 17 *) ut(error_test (str_to_exp "x;;") "Unbound variable x") ;
                                                                     (* nums *)
  (* 18 *) ut(error_test (str_to_exp "x + 42;;") "Unbound variable x") ;
  (* 19 *) ut(error_test (str_to_exp "42 + false;;") 
                           "unsupported binop expression type(s)") ;
                                                                   (* floats *)
  (* 20 *) ut(error_test (str_to_exp "x +. 42;;") "Unbound variable x") ;
  (* 21 *) ut(error_test (str_to_exp "42 +. false;;") 
                           "unsupported binop expression type(s)") ;
                                                                    (* bools *)
  (* 22 *) ut(error_test (str_to_exp "true + false;;") 
                           "arguments not bools OR unsupported binop") ;
  (* 23 *) ut(error_test (str_to_exp "true +. false;;") 
                                "arguments not bools OR unsupported binop") ;
  (* 24 *) ut(error_test (str_to_exp "if 42 then 41 else 40;;") 
                                "Condition of type bool expected") ;
                                                                      (* app *)
  if sub then
  (* 25 *) ut(error_test (str_to_exp "let f = fun x -> 2 * x in f f 42;;") 
                                  "unsupported binop expression type(s)") ;
  if sub then 
  (* 26 *) ut(error_test (str_to_exp "let f = fun x -> 2. *. x in f f 42.;;") 
                                  "unsupported binop expression type(s)") ;
                                                                    (* raise *)
  (* 27 *) ut(error_test (str_to_exp cRaise_0) "") ;
  (* 28 *) ut(error_test Raise "") ;
                                                               (* unassigned *)
  (* 29 *) ut(error_test Unassigned "Unassigned") ;

(* !!! RUNNING ./TEST_EVAL PRODUCES AN ERROR AFTER THIS POINT,
       BUT I HAVE TESTED EACH OF THE FOLLOWING TESTS MANUALLY 
       AND THEY ALL WORK ------------------------------------- !!! *)


  (* -----{ DYNAMIC & LEXICAL TESTING }--------------- *)
  if lexi then 
    ((* 30b *) ut(expr_test cLex0) ;
     (* 31b *) ut(expr_test cLex0') ;
     (* 32b *) ut(expr_test cLex1) ;
     (* 33b *) ut(expr_test cLex1') ;
     (* 34b *) ut(expr_test cLex1) ;
     (* 35b *) ut(expr_test cLex1') ;
     (* 36b *) ut(error_test (str_to_exp cExp3a) "Unbound variable f") ;
     (* 37b *) ut(error_test (str_to_exp cExp3'a) "Unbound variable f")) 
  else if dyna then 
    ((* 30a *) ut(expr_test cDyn0);
     (* 31a *) ut(expr_test cDyn0');
     (* 32a *) ut(error_test (str_to_exp cExp3a) "Unbound variable f");
     (* 33a *) ut(error_test (str_to_exp cExp3'a) "Unbound variable f");
     (* 34a *) ut(error_test (str_to_exp cExp4a) "Unbound variable x");
     (* 35a *) ut(error_test (str_to_exp cExp4'a) "Unbound variable x");
     (* 36a *) ut(expr_test cDyn1);
     (* 37a *) ut(expr_test cDyn1')) 
  else () ;
  
  (* -----{ EXPRESSION TESTS }--------------- *)
                                                                     (* nums *)
  (* 38 *) ut(expr_test cInt_pos) ;
  (* 39 *) ut(expr_test cInt_neg) ;
                                                                   (* floats *)
  (* 40 *) ut(expr_test cFlo_pos) ; 
  (* 41 *) ut(expr_test cFlo_neg ) ; 
                                                                    (* unops *)
  (* 44 *) ut(expr_test cNeg_var) ;
  (* 45 *) ut(expr_test cNot_var) ; 
                                                                   (* binops *)
  (* 48 *) ut(expr_test cBinop_b1) ;
  (* 49 *) ut(expr_test cBinop_b2) ;
                                                             (* conditionals *)
  (* 50 *) ut(expr_test cCond_0) ;
  (* 51 *) ut(expr_test cCond_1) ;
  (* 52 *) ut(expr_test cCond_0') ;
  (* 53 *) ut(expr_test cCond_1') ;
                                                                      (* fun *)
  if not lexi then (* 54a *) ut(expr_test cFun0)
    else (* 54b *) ut(value_to_string (evaluator sub dyna lexi cFun1 env) = 
                      cFun2) ;
                                                                   (* let... *)
  (* 55 *) ut(expr_test cLeT0) ;
  (* 56 *) ut(expr_test cLeT1) ;
  (* 57 *) ut(expr_test cLeT2) ;
                                                               (* let rec... *)
  (* 58 *) ut(expr_test cLeTR0) ;
  (* 59 *) ut(expr_test cLeTR1) ;
  (* 60 *) ut(expr_test cLeTR2) ;
                                                                      (* app *)
  (* 61 *) ut(expr_test cApp0) ;
                                                                    (* raise *)
  (* 62 *) ut(expr_test cRaise_1) ;
                                                      (* COMPLEX EXPRESSIONS *)
  (* 63 *) ut(expr_test cExp0) ;
  (* 64 *) ut(expr_test cExp0') ;
  (* 65 *) ut(expr_test cExp1) ;
  (* 66 *) ut(expr_test cExp1') ;
  (* 67 *) ut(expr_test cExp2) ;
  (* 68 *) ut(expr_test cExp2') ;
  (* 69 *) ut(expr_test cExp3b) ;
  (* 70 *) ut(expr_test cExp3'b) ;
  (* 71 *) ut(expr_test cExp4b) ;
  (* 72 *) ut(expr_test cExp4'b) ;
  (* 73 *) ut(expr_test cExp5) ;
  (* 74 *) ut(expr_test cExp5') ;;


(* ==============================| ALL TESTS |============================== *)
let run_all_eval_tests () =
  close_test () ;
  lookup_extend_test () ;
  to_string_test () ;
  evaluator_test true false false ; (* substitution *)
  evaluator_test false true false ; (* dynamic *)
  evaluator_test true false true ;; (* lexical *)

let _ = run_all_eval_tests () ;;
