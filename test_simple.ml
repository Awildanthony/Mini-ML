open Miniml ;;

(* ...........{ UNIVERSAL SAMPLE EXPRESSIONS }.......... *)
let s_to_e = str_to_exp ;;
                                                                   (* var(s) *)
let var_x = s_to_e "x;;" ;; 
                                                                     (* nums *)
let int_pos = s_to_e "42;;" ;; 
let int_neg = s_to_e "~-42;;" ;;
                                                                   (* floats *)
let flo_pos = s_to_e "42.;;" ;; 
let flo_neg = s_to_e "~-42.;;" ;;
                                                                    (* bools *)
let bool_t = s_to_e "true;;" ;; 
let bool_f = s_to_e "false;;" ;;
                                                                    (* unops *)
let neg_var = s_to_e "~-x;;" ;; 
let not_var = s_to_e "not x;;" ;;
                                                           (* binops: --nums *)
let bin_add_i = s_to_e "x + 42;;" ;; 
let bin_sub_i = s_to_e "x - 42;;" ;;     
let bin_mul_i = s_to_e "x * 42;;" ;;
let bin_div_i = s_to_e "x / 41;;" ;; 
let bin_pow_i = s_to_e "x ** 3.;;" ;; 
let bin_equ_i = s_to_e "x = 42;;" ;;
let bin_les_i = s_to_e "x < 42;;" ;;
let bin_leq_i = s_to_e "x <= 42;;" ;;
let bin_gre_i = s_to_e "x > 42;;" ;;
let bin_geq_i = s_to_e "x >= 42;;" ;;
let bin_neq_i = s_to_e "x <> 42;;" ;;
                                                         (* binops: --floats *)
let bin_add_f = s_to_e "x +. 42.;;" ;;   
let bin_sub_f = s_to_e "x -. 42.;;" ;;
let bin_mul_f = s_to_e "x *. 42.;;" ;;
let bin_div_f = s_to_e "x /. 41.;;" ;; 
let bin_pow_f = s_to_e "x ** 3.;;" ;; 
let bin_equ_f = s_to_e "x = 42.;;" ;;
let bin_les_f = s_to_e "x < 42.;;" ;;
let bin_leq_f = s_to_e "x <= 42.;;" ;;
let bin_gre_f = s_to_e "x > 42.;;" ;;
let bin_geq_f = s_to_e "x >= 42.;;" ;;
let bin_neq_f = s_to_e "x <> 42.;;" ;;
                                                             (* conditionals *)
let cond_0 = s_to_e "if x = 42 then true else y;;" ;; 
let cond_1 = s_to_e "if x = 42. then true else y;;" ;;
                                                                      (* fun *)
let fun0 = s_to_e "fun x -> x + x ;;" ;; 
let fun0' = s_to_e "fun x -> x +. x ;;"
let fun1 = s_to_e "fun y -> f (x + y);;" ;;
let fun1' = s_to_e "fun y -> f (x +. y);;" ;;
let fun2 = s_to_e "fun y -> x + y;;" ;;
let fun2' = s_to_e "fun y -> x +. y;;" ;;
                                                                   (* let... *)
let leT0 = s_to_e "let x = 42 in x + x;;" ;; 
let leT0' = s_to_e "let x = 42. in x +. x;;" ;; 
let leT1 = s_to_e "let x = 42 in x + y;;" ;;
let leT1' = s_to_e "let x = 42. in x +. y;;" ;;
                                                               (* let rec... *)
let leTR = s_to_e "let rec x = fun y -> x in x;;" ;; 
                                                                      (* app *)
let ap0 = s_to_e "let sample = fun x -> 42 * x in sample 42;;" ;; 
let ap0' = s_to_e "let sample = fun x -> 42. *. x in sample 42.;;" ;;
let ap1 = s_to_e "f (x + y);;" ;;
let ap1' = s_to_e "f (x +. y);;" ;;


(* counter reset each time ./tests.byte is called *)
let ctr = ref 0 ;;

(* adapted from the original unit_test by CS51 staff, but this time
   disregarding the need for a msg and instead printing the test # *)
let unit_test (condition : bool) : unit =  
 let count : unit -> int = 
    fun () ->
        ctr := !ctr + 1; 
        !ctr  in 
  if condition then
    (Printf.printf "test %i " (count ()) ;
    Printf.printf "passed\n")
  else
    (Printf.printf "test %i " (count ()) ;
    Printf.printf "FAILED\n") ;;



let ut x = unit_test x ;; (* shorthand *)
