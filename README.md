
# MiniML -- Spring 2022

An interpreter written in OCaml for a subset of the OCaml language. \
Independently developed as a final project for Harvard's CS51 course.


# Instructions 
Use ```make all``` to compile the files using the makefile, then use ```./miniml.byte``` to launch the interpreter. 
\
You may edit the evaluation environment at the bottom of ```evaluation.ml``` by setting ```evaluate = eval_s``` for a substitution environment, ```evaluate = eval_d``` for a dynamically-scoped environment, or ```evaluate = eval_l``` for a lexically-scoped environment.


# Description of Files

## .gitignore
Included in the original basic source code template written and provided by the course heads of Harvard CS51.

## .merlin 
An error-detecting software working in collaboration with PKG CS51Utils and PKG graphics.

## Project Writeup PDF
A writeup report detailing my implementation of MiniML and its functionality, including what subsets of the OCaml language it supports as an interpreter in a substitution environment and dynamically- & lexically-scoped environments, with examples.

## Project Description PDF
A PDF containing the project description and required specifications in their entirety, taken directly from Harvard CS51's online textbook. 

## evaluation.ml
A module "implementing small untyped ML-like language under various operational semantics." Evaluates expressions taken in by the interpreter. 

## expr.ml 
Defining algebraic data types, expressions, and several helper functions utilized in evaluation.ml. Mainly an implementation of the interpreter's semantics. 

## expr.mli
Abstract syntax of MiniML expressions & type signatures for expr.ml, including my extensions apart from the basic operators and expression types given by the Harvard CS51 course heads. 

## miniml.ml
A Read-Eval-Print Loop for the interpreter, prompting for MiniML expressions and then evaluating them and printing the resulting values. Provided by the Harvard CS51 course heads. 

## miniml_lex.mll
A lexical analyzer for miniml, provided by the Harvard cs51 course heads and modified to include my extensions.

## miniml_parse.mly
A parser for miniml, provided by the Harvard CS51 course heads and modified to include my extensions. 

## test_eval.ml
Extensive unit tests for eval.ml

## test_expr.ml 
Extensive unit tests for expr.ml

## test_simple.ml
A simple unit-testing framework.
