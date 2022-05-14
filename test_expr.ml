(* <<<<<<<<<<<< Test File for CS51 2022 Final Project : MiniML >>>>>>>>>>>>> *)

open Expr ;;
open Miniml ;;

open Test_simple ;; (* adapted unit-testing framework from CS51 *)

(* ==========================| TESTS FOR expr.ml |========================== *)

(* .................{ free_vars TESTS }................. *)
let free_vars_test () = 
  let vars_test (e : expr) (v : string list) : bool =
    same_vars (free_vars e) (vars_of_list v) in
                                                  (* var(s) *)
  (* 1 *) ut(vars_test var_x ["x"]) ;
                                                  (* nums *)
  (* 2 *) ut(vars_test int_pos []) ;
  (* 3 *) ut(vars_test int_neg []) ;
                                                  (* floats *)
  (* 4 *) ut(vars_test flo_pos []) ;
  (* 5 *) ut(vars_test flo_neg []) ;
                                                  (* bools *)
  (* 6 *) ut(vars_test bool_t []) ;
  (* 7 *) ut(vars_test bool_f []) ;
                                                  (* unops *)
  (* 8 *) ut(vars_test neg_var ["x"]) ;
  (* 9 *) ut(vars_test not_var ["x"]) ;
                                                  (* binops: --nums *)
  (* 10 *) ut(vars_test bin_add_i ["x"]) ;
  (* 11 *) ut(vars_test bin_sub_i ["x"]) ;
  (* 12 *) ut(vars_test bin_mul_i ["x"]) ;
  (* 13 *) ut(vars_test bin_div_i ["x"]) ;
  (* 14 *) ut(vars_test bin_pow_i ["x"]) ;
  (* 15 *) ut(vars_test bin_equ_i ["x"]) ;
  (* 16 *) ut(vars_test bin_les_i ["x"]) ;
  (* 17 *) ut(vars_test bin_leq_i ["x"]) ;
  (* 18 *) ut(vars_test bin_gre_i ["x"]) ;
  (* 19 *) ut(vars_test bin_geq_i ["x"]) ;
  (* 20 *) ut(vars_test bin_neq_i ["x"]) ;
                                                  (* binops: --floats *)
  (* 21 *) ut(vars_test bin_add_f ["x"]) ;
  (* 22 *) ut(vars_test bin_sub_f ["x"]) ;
  (* 23 *) ut(vars_test bin_mul_f ["x"]) ;
  (* 24 *) ut(vars_test bin_div_f ["x"]) ;
  (* 25 *) ut(vars_test bin_pow_f ["x"]) ;
  (* 26 *) ut(vars_test bin_equ_f ["x"]) ;
  (* 27 *) ut(vars_test bin_les_f ["x"]) ;
  (* 28 *) ut(vars_test bin_leq_f ["x"]) ;
  (* 29 *) ut(vars_test bin_gre_f ["x"]) ;
  (* 30 *) ut(vars_test bin_geq_f ["x"]) ;
  (* 31 *) ut(vars_test bin_neq_f ["x"]) ;
                                                  (* conditionals *)
  (* 32 *) ut(vars_test cond_0 ["x"; "y"]) ;
  (* 33 *) ut(vars_test cond_1 ["x"; "y"]) ;
                                                  (* fun *)
  (* 34 *) ut(vars_test fun0 []) ;
  (* 35 *) ut(vars_test fun0' []) ;
  (* 36 *) ut(vars_test fun1 ["f"; "x"]) ;
  (* 37 *) ut(vars_test fun1' ["f"; "x"]) ;
  (* 38 *) ut(vars_test fun2 ["x"]) ;
  (* 39 *) ut(vars_test fun2' ["x"]) ;
                                                  (* let... *)
  (* 40 *) ut(vars_test leT0 []) ;
  (* 41 *) ut(vars_test leT0' []) ;
  (* 42 *) ut(vars_test leT1 ["y"]) ;
  (* 43 *) ut(vars_test leT1' ["y"]) ;
                                                  (* let rec... *)
  (* 44 *) ut(vars_test leTR []) ;
                                                  (* app *)
  (* 45 *) ut(vars_test ap0 []) ;
  (* 46 *) ut(vars_test ap0' []) ;
  (* 47 *) ut(vars_test ap1 ["f"; "x"; "y"]) ;
  (* 48 *) ut(vars_test ap1' ["f"; "x"; "y"]) ;;


(* ................{ new_varname TESTS }................ *)
let new_varname_test () =
  (* 49 *) ut(new_varname () = "var0") ;
  (* 50 *) ut(new_varname () = "var1") ;
  (* 51 *) ut(new_varname () = "var2") ;
  (* 52 *) ut(not(new_varname () = "var0" || 
                  new_varname () = "var1" || 
                  new_varname () = "var2")) ;;
                  (* ^ ^ ^ shouldn't repeat names *)


(* ...................{ subst TESTS }................... *)
let subst_test () =
  let repl_i = Num 41 in
  let repl_f = Float(41.) in
  let repl_var = Var "z" in
  
                                                                   (* var(s) *)
  (* 53 *) ut(subst "x" repl_var var_x = str_to_exp "z;;") ;  
                                                                     (* nums *)
  (* 54 *) ut(subst "x" repl_i var_x = str_to_exp "41;;") ; 
  (* 55 *) ut(subst "x" repl_i neg_var = str_to_exp "~-41;;") ;
  (* 56 *) ut(subst "x" repl_i int_pos = int_pos) ;
  (* 57 *) ut(subst "x" repl_i int_neg = int_neg) ;
                                                                   (* floats *)
  (* 58 *) ut(subst "x" repl_f var_x = str_to_exp "41.;;") ; 
  (* 59 *) ut(subst "x" repl_f neg_var = str_to_exp "~-41.;;") ;
  (* 60 *) ut(subst "x" repl_f flo_pos = flo_pos) ;
  (* 61 *) ut(subst "x" repl_f flo_neg = flo_neg) ;
                                                                    (* bools *)
  (* 62 *) ut(subst "x" repl_i bool_t = bool_t) ; 
  (* 63 *) ut(subst "x" repl_i bool_f = bool_f) ;
                                                                    (* unops *)
  (* 64 *) ut(subst "x" repl_var neg_var = str_to_exp "~-z;;") ; 
  (* 65 *) ut(subst "x" repl_var not_var = str_to_exp "not z;;") ;
                                                      (* binops: --nums, int *)
  (* 66 *) ut(subst "x" repl_i bin_add_i = str_to_exp "41 + 42;;") ; 
  (* 67 *) ut(subst "x" repl_i bin_sub_i = str_to_exp "41 - 42;;") ; 
  (* 68 *) ut(subst "x" repl_i bin_mul_i = str_to_exp "41 * 42;;") ;
  (* 69 *) ut(subst "x" repl_i bin_div_i = str_to_exp "41 / 41;;") ;
  (* 70 *) ut(subst "x" repl_f bin_pow_i = str_to_exp "41. ** 3.;;") ;
  (* 71 *) ut(subst "x" repl_i bin_equ_i = str_to_exp "41 = 42;;") ;
  (* 72 *) ut(subst "x" repl_i bin_les_i = str_to_exp "41 < 42;;") ;
  (* 73 *) ut(subst "x" repl_i bin_leq_i = str_to_exp "41 <= 42;;") ;
  (* 74 *) ut(subst "x" repl_i bin_gre_i = str_to_exp "41 > 42;;") ;
  (* 75 *) ut(subst "x" repl_i bin_geq_i = str_to_exp "41 >= 42;;") ;
  (* 76 *) ut(subst "x" repl_i bin_neq_i = str_to_exp "41 <> 42;;") ;
                                                      (* binops: --nums, var *)
  (* 77 *) ut(subst "x" repl_var bin_add_i = str_to_exp "z + 42;;") ; 
  (* 78 *) ut(subst "x" repl_var bin_sub_i = str_to_exp "z - 42;;") ;
  (* 79 *) ut(subst "x" repl_var bin_mul_i = str_to_exp "z * 42;;") ;
  (* 80 *) ut(subst "x" repl_var bin_div_i = str_to_exp "z / 41;;") ;
  (* 81 *) ut(subst "x" repl_var bin_pow_i = str_to_exp "z ** 3.;;") ;
  (* 82 *) ut(subst "x" repl_var bin_equ_i = str_to_exp "z = 42;;") ;
  (* 83 *) ut(subst "x" repl_var bin_les_i = str_to_exp "z < 42;;") ;
  (* 84 *) ut(subst "x" repl_var bin_leq_i = str_to_exp "z <= 42;;") ;
  (* 85 *) ut(subst "x" repl_var bin_gre_i = str_to_exp "z > 42;;") ;
  (* 86 *) ut(subst "x" repl_var bin_geq_i = str_to_exp "z >= 42;;") ;
  (* 87 *) ut(subst "x" repl_var bin_neq_i = str_to_exp "z <> 42;;") ;
                                                  (* binops: --floats, float *)
  (* 88 *) ut(subst "x" repl_f bin_add_f = str_to_exp "41. +. 42.;;") ; 
  (* 89 *) ut(subst "x" repl_f bin_sub_f = str_to_exp "41. -. 42.;;") ;
  (* 90 *) ut(subst "x" repl_f bin_mul_f = str_to_exp "41. *. 42.;;") ;
  (* 91 *) ut(subst "x" repl_f bin_div_f = str_to_exp "41. /. 41.;;") ;
  (* 92 *) ut(subst "x" repl_f bin_pow_f = str_to_exp "41. ** 3.;;") ;
  (* 93 *) ut(subst "x" repl_f bin_equ_f = str_to_exp "41. = 42.;;") ;
  (* 94 *) ut(subst "x" repl_f bin_les_f = str_to_exp "41. < 42.;;") ;
  (* 95 *) ut(subst "x" repl_f bin_leq_f = str_to_exp "41. <= 42.;;") ;
  (* 96 *) ut(subst "x" repl_f bin_gre_f = str_to_exp "41. > 42.;;") ;
  (* 97 *) ut(subst "x" repl_f bin_geq_f = str_to_exp "41. >= 42.;;") ;
  (* 98 *) ut(subst "x" repl_f bin_neq_f = str_to_exp "41. <> 42.;;") ;
                                                    (* binops: --floats, var *)
  (* 99 *) ut(subst "x" repl_var bin_add_f = str_to_exp "z +. 42.;;") ; 
  (* 100 *) ut(subst "x" repl_var bin_sub_f = str_to_exp "z -. 42.;;") ;
  (* 101 *) ut(subst "x" repl_var bin_mul_f = str_to_exp "z *. 42.;;") ;
  (* 102 *) ut(subst "x" repl_var bin_div_f = str_to_exp "z /. 41.;;") ;
  (* 103 *) ut(subst "x" repl_var bin_pow_f = str_to_exp "z ** 3.;;") ;
  (* 104 *) ut(subst "x" repl_var bin_equ_f = str_to_exp "z = 42.;;") ;
  (* 105 *) ut(subst "x" repl_var bin_les_f = str_to_exp "z < 42.;;") ;
  (* 106 *) ut(subst "x" repl_var bin_leq_f = str_to_exp "z <= 42.;;") ;
  (* 107 *) ut(subst "x" repl_var bin_gre_f = str_to_exp "z > 42.;;") ;
  (* 108 *) ut(subst "x" repl_var bin_geq_f = str_to_exp "z >= 42.;;") ;
  (* 109 *) ut(subst "x" repl_var bin_neq_f = str_to_exp "z <> 42.;;") ;
                                             (* conditionals (nums & floats) *)
  (* 110 *) ut(subst "x" repl_i cond_0 = 
         str_to_exp "if 41 = 42 then true else y;;") ;
  (* 111 *) ut(subst "x" repl_f cond_1 = 
         str_to_exp "if 41. = 42. then true else y;;") ;
  (* 112 *) ut(subst "y" repl_var cond_0 = 
         str_to_exp "if x = 42 then true else z;;") ;
  (* 113 *) ut(subst "y" repl_var cond_1 = 
         str_to_exp "if x = 42. then true else z;;") ;
                                                                      (* fun *)
  (* 114 *) ut(subst "x" repl_i fun0 = str_to_exp "fun x -> x + x;;") ;
  (* 115 *) ut(subst "x" repl_f fun0' = str_to_exp "fun x -> x +. x;;") ;
  (* 116 *) ut(subst "x" repl_i fun1 = str_to_exp "fun y -> f (41 + y);;") ;
  (* 117 *) ut(subst "x" repl_f fun1' = str_to_exp "fun y -> f (41. +. y);;") ;
  (* 118 *) ut(subst "y" repl_i fun1 = str_to_exp "fun y -> f (x + y);;") ;
  (* 119 *) ut(subst "y" repl_f fun1' = str_to_exp "fun y -> f (x +. y);;") ;
  (* 120 *) ut(subst "x" (str_to_exp "y + 41;;") fun2 = 
         str_to_exp "fun y -> y + 41 + y;;") ;
  (* 121 *) ut(subst "x" (str_to_exp "y +. 41.;;") fun2' = 
         str_to_exp "fun y -> y +. 41. +. y;;") ;
                                                                   (* let... *)
  (* 122 *) ut(subst "y" repl_i leT1 = str_to_exp "let x = 42 in x + 41;;") ;
  (* 123 *) ut(subst "y" repl_f leT1' =str_to_exp "let x = 42. in x +. 41.;;");
  (* app *)
  (* 124 *) ut(subst "x" repl_i ap1 = str_to_exp "f (41 + y);;") ;
  (* 125 *) ut(subst "x" repl_f ap1' = str_to_exp "f (41. +. y);;") ;;


(* ...........{ exp_to_concrete_string TESTS }.......... *)
let exp_to_concrete_string_test () =
  let e_to_cs = exp_to_concrete_string in
                                                                   (* var(s) *)
  (* 126 *) ut(e_to_cs var_x = "x") ; 
                                                                     (* nums *)
  (* 127 *) ut(e_to_cs int_pos = "42") ; 
                                                                   (* floats *)
  (* 128 *) ut(e_to_cs flo_pos = "42.") ; 
                                                                    (* bools *)
  (* 129 *) ut(e_to_cs bool_t = "true") ; 
  (* 130 *) ut(e_to_cs bool_f = "false") ;
                                                                    (* unops *)
  (* 131 *) ut(e_to_cs int_neg = "~-42") ;
  (* 132 *) ut(e_to_cs flo_neg = "~-42.") ;
  (* 133 *) ut(e_to_cs neg_var = "~-x") ; 
  (* 134 *) ut(e_to_cs not_var = "not x") ;
                                                           (* binops: --nums *)
  (* 135 *) ut(e_to_cs bin_add_i = "x + 42") ; 
  (* 136 *) ut(e_to_cs bin_sub_i = "x - 42") ;     
  (* 137 *) ut(e_to_cs bin_mul_i = "x * 42") ;
  (* 138 *) ut(e_to_cs bin_div_i = "x / 41") ;
  (* 139 *) ut(e_to_cs bin_pow_i = "x ** 3.") ;
  (* 140 *) ut(e_to_cs bin_equ_i = "x = 42") ;
  (* 141 *) ut(e_to_cs bin_les_i = "x < 42") ;
  (* 142 *) ut(e_to_cs bin_leq_i = "x <= 42") ;
  (* 143 *) ut(e_to_cs bin_gre_i = "x > 42") ;
  (* 144 *) ut(e_to_cs bin_geq_i = "x >= 42") ;
  (* 145 *) ut(e_to_cs bin_neq_i = "x <> 42") ;
                                                         (* binops: --floats *)
  (* 146 *) ut(e_to_cs bin_add_f = "x +. 42.") ;     
  (* 147 *) ut(e_to_cs bin_sub_f = "x -. 42.") ;
  (* 148 *) ut(e_to_cs bin_mul_f = "x *. 42.") ;
  (* 149 *) ut(e_to_cs bin_div_f = "x /. 41.") ;
  (* 150 *) ut(e_to_cs bin_pow_f = "x ** 3.") ;
  (* 151 *) ut(e_to_cs bin_equ_f = "x = 42.") ;
  (* 152 *) ut(e_to_cs bin_les_f = "x < 42.") ;
  (* 153 *) ut(e_to_cs bin_leq_f = "x <= 42.") ;
  (* 154 *) ut(e_to_cs bin_gre_f = "x > 42.") ;
  (* 155 *) ut(e_to_cs bin_geq_f = "x >= 42.") ;
  (* 156 *) ut(e_to_cs bin_neq_f = "x <> 42.") ;
                                                             (* conditionals *)
  (* 157 *) ut(e_to_cs cond_0 = "if x = 42 then true else y") ; 
  (* 158 *) ut(e_to_cs cond_1 = "if x = 42. then true else y") ;
                                                                      (* fun *)
  (* 159 *) ut(e_to_cs fun0 = "fun x -> x + x") ; 
  (* 160 *) ut(e_to_cs fun0' = "fun x -> x +. x") ;
  (* 161 *) ut(e_to_cs fun1 = "fun y -> f (x + y)") ;
  (* 162 *) ut(e_to_cs fun1' = "fun y -> f (x +. y)") ;
  (* 163 *) ut(e_to_cs fun2 = "fun y -> x + y") ;
  (* 164 *) ut(e_to_cs fun2' = "fun y -> x +. y") ;
                                                                   (* let... *)
  (* 165 *) ut(e_to_cs leT0 = "let x = 42 in x + x") ; 
  (* 166 *) ut(e_to_cs leT0' = "let x = 42. in x +. x") ;
  (* 167 *) ut(e_to_cs leT1 = "let x = 42 in x + y") ;
  (* 168 *) ut(e_to_cs leT1' = "let x = 42. in x +. y") ;
                                                               (* let rec... *)
  (* 169 *) ut(e_to_cs leTR  = "let rec x = fun y -> x in x") ; 
                                                                      (* app *)
  (* 170 *) ut(e_to_cs ap0 = "let sample = fun x -> 42 * x in sample (42)") ; 
  (* 171 *) ut(e_to_cs ap0' ="let sample = fun x -> 42. *. x in sample (42.)");   
  (* 172 *) ut(e_to_cs ap1 = "f (x + y)") ;
  (* 173 *) ut(e_to_cs ap1' = "f (x +. y)") ;
                                                                    (* Raise *)
  (* 174 *) ut(e_to_cs Raise = "raise (Some (Exception))") ; 
                                                               (* Unassigned *)
  (* 175 *) ut(e_to_cs Unassigned = "Some (Unassigned exp)") ;; 


(* ...........{ exp_to_abstract_string TESTS }.......... *)
let exp_to_abstract_string_test () =
  let e_to_as = exp_to_abstract_string in
                                                                     (* vars *)
  (* 176 *) ut(e_to_as var_x = "Var(x)") ;
                                                                     (* nums *)
  (* 177 *) ut(e_to_as int_pos = "Num(42)") ;
                                                                   (* floats *)
  (* 178 *) ut(e_to_as flo_pos = "Float(42.)") ;
                                                                    (* bools *)
  (* 179 *) ut(e_to_as bool_t = "Bool(true)") ;
  (* 180 *) ut(e_to_as bool_f = "Bool(false)") ;
                                                                    (* unops *)
  (* 181 *) ut(e_to_as int_neg = "Unop(Negate_i, Num(42))") ;
  (* 182 *) ut(e_to_as flo_neg = "Unop(Negate_i, Float(42.))") ;
  (* 183 *) ut(e_to_as neg_var = "Unop(Negate_i, Var(x))") ;
  (* 184 *) ut(e_to_as not_var = "Unop(Not, Var(x))") ;
                                                           (* binops: --nums *)
  (* 185 *) ut(e_to_as bin_add_i = "Binop(Plus_i, Var(x), Num(42))") ;
  (* 186 *) ut(e_to_as bin_sub_i = "Binop(Minus_i, Var(x), Num(42))") ;
  (* 187 *) ut(e_to_as bin_mul_i = "Binop(Times_i, Var(x), Num(42))") ;
  (* 188 *) ut(e_to_as bin_div_i = "Binop(Divide_i, Var(x), Num(41))") ;
  (* 189 *) ut(e_to_as bin_pow_i = "Binop(Power_f, Var(x), Float(3.))") ;
  (* 190 *) ut(e_to_as bin_equ_i = "Binop(Equals, Var(x), Num(42))") ;
  (* 191 *) ut(e_to_as bin_les_i = "Binop(LessThan, Var(x), Num(42))") ;
  (* 192 *) ut(e_to_as bin_leq_i = "Binop(LessOrEqual, Var(x), Num(42))") ;
  (* 193 *) ut(e_to_as bin_gre_i = "Binop(GreaterThan, Var(x), Num(42))") ;
  (* 194 *) ut(e_to_as bin_geq_i = "Binop(GreaterOrEqual, Var(x), Num(42))") ;
  (* 195 *) ut(e_to_as bin_neq_i = "Binop(Unequal, Var(x), Num(42))") ;
                                                         (* binops: --floats *)
  (* 196 *) ut(e_to_as bin_add_f = "Binop(Plus_f, Var(x), Float(42.))") ;
  (* 197 *) ut(e_to_as bin_sub_f = "Binop(Minus_f, Var(x), Float(42.))") ;
  (* 198 *) ut(e_to_as bin_mul_f  = "Binop(Times_f, Var(x), Float(42.))") ;
  (* 199 *) ut(e_to_as bin_div_f = "Binop(Divide_f, Var(x), Float(41.))") ;
  (* 200 *) ut(e_to_as bin_pow_f = "Binop(Power_f, Var(x), Float(3.))") ;
  (* 201 *) ut(e_to_as bin_equ_f = "Binop(Equals, Var(x), Float(42.))") ;
  (* 202 *) ut(e_to_as bin_les_f = "Binop(LessThan, Var(x), Float(42.))") ;
  (* 203 *) ut(e_to_as bin_leq_f = "Binop(LessOrEqual, Var(x), Float(42.))") ;
  (* 204 *) ut(e_to_as bin_gre_f = "Binop(GreaterThan, Var(x), Float(42.))") ;
  (* 205 *) ut(e_to_as bin_geq_f ="Binop(GreaterOrEqual, Var(x), Float(42.))");
  (* 206 *) ut(e_to_as bin_neq_f = "Binop(Unequal, Var(x), Float(42.))") ;
                                                             (* conditionals *)
  (* 207 *) ut(e_to_as cond_0 =
    "Conditional(Binop(Equals, Var(x), Num(42)), Bool(true), Var(y))") ;
  (* 208 *) ut(e_to_as cond_1 =
    "Conditional(Binop(Equals, Var(x), Float(42.)), Bool(true), Var(y))") ;
                                                                      (* fun *)
  (* 209 *) ut(e_to_as fun0 = "Fun(x, Binop(Plus_i, Var(x), Var(x)))") ;
  (* 210 *) ut(e_to_as fun0' = "Fun(x, Binop(Plus_f, Var(x), Var(x)))") ; 
  (* 211 *) ut(e_to_as fun2 = "Fun(y, Binop(Plus_i, Var(x), Var(y)))") ;
  (* 212 *) ut(e_to_as fun2' = "Fun(y, Binop(Plus_f, Var(x), Var(y)))") ; 
                                                                   (* let... *)
  (* 213 *) ut(e_to_as leT0 ="Let(x, Num(42), Binop(Plus_i, Var(x), Var(x)))");
  (* 214 *) ut(e_to_as leT0' = 
              "Let(x, Float(42.), Binop(Plus_f, Var(x), Var(x)))") ;
  (* 215 *) ut(e_to_as leT1 = 
              "Let(x, Num(42), Binop(Plus_i, Var(x), Var(y)))") ;
  (* 216 *) ut(e_to_as leT1' = 
              "Let(x, Float(42.), Binop(Plus_f, Var(x), Var(y)))") ;
                                                               (* let rec... *)
  (* 217 *) ut(e_to_as leTR = "Letrec(x, Fun(y, Var(x)), Var(x))") ;
                                                                      (* app *)
  (* 218 *) ut(e_to_as ap0 =
    "Let(sample, \
          Fun(x, \
            Binop(Times_i, Num(42), Var(x))), \
            App(Var(sample), Num(42)))") ;
  (* 219 *) ut(e_to_as ap0' =
    "Let(sample, \
          Fun(x, \
            Binop(Times_f, Float(42.), Var(x))), \
            App(Var(sample), Float(42.)))") ;
                                                                    (* Raise *)
  (* 220 *) ut(e_to_as Raise = "Raise") ;
                                                               (* Unassigned *)
  (* 221 *) ut(e_to_as Unassigned = "Unassigned") ;;



(* ==============================| ALL TESTS |============================== *)
let run_all_expr_tests () =
  free_vars_test () ;
  new_varname_test () ;
  subst_test () ;
  exp_to_concrete_string_test () ;
  exp_to_abstract_string_test () ;;

let _ = run_all_expr_tests () ;;
