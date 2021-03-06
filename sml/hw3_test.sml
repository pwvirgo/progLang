(* Homework3 Simple Test

These are basic test cases. Passing these tests does not guarantee
that your code will pass the actual homework grader To run the test,
add a new line to the top of this file: use "homeworkname.sml"; All
the tests should evaluate to true. For example, the REPL should say:
val test1 = true : bool *)

use "hw3.sml";

val test1a = only_capitals ["A","B","C"] = ["A","B","C"]
val test1b = only_capitals ["antelope","Bear","Cantalope"] = ["Bear","Cantalope"]
val test1c = only_capitals ["a fish","B","Come Here"] = ["B", "Come Here"]
val test1d = only_capitals [] = []

val test2a = longest_string1 ["A","bc","C"] = "bc"
val test2b = longest_string1 ["ab","bc","Cd"] = "ab"
val test2c = longest_string1 [] = ""

val test3a = longest_string2 ["A","bc","C"] = "bc"
val test3b = longest_string2 ["ab","bc","Cd"] = "Cd"
val test3c = longest_string2 [] = ""

val test4aa= longest_string3 ["A","bc","C"] = "bc"
val test4ab= longest_string3 ["ab","bc","Cd"] = "ab"
val test4ac= longest_string3 [] = ""

val test4ba = longest_string4 ["A","bc","C"] = "bc"
val test4bb = longest_string4 ["ab","bc","Cd"] = "Cd"
val test4bc = longest_string4 [] = ""

val test5a = longest_capitalized ["A","bc","C"] = "A"
val test5b = longest_capitalized ["Ab","bc","Cds","ef"] = "Cds"
val test5c = longest_capitalized [] = ""


val test6 = rev_string "abc" = "cba"


val test7a  = first_answer 
   (fn x => if x > 3 then SOME x else NONE) 
   [1,2,3,4,5] = 4

(* val test7b  = first_answer 
   (fn x => if x > 6 then SOME x else NONE) 
   [1,2,3,4,5] = 4 *)


val test8a = all_answers (fn x => if x = 1 then SOME [x] else NONE) [2,3,4,5,6,7] = NONE

val test8b = all_answers (fn x => if x = 6 then SOME [x] else NONE) [2,3,4,5,6,7] = NONE

val test8C = all_answers (fn x => if x>0 then SOME [x] else NONE) [2,3,4,5,6,7] = SOME [2,3,4,5,6,7]

val test8d = all_answers (fn x => if x>0 then SOME [x] else NONE) [] = SOME []


val test9aa = count_wildcards Wildcard = 1
val test9ab = count_wildcards (TupleP[Wildcard, Wildcard, Variable("12345")]) =2
val test9ac = count_wildcards (ConstP(23))  = 0

val test9ba = count_wild_and_variable_lengths (Variable("a")) = 1
val test9bb = count_wild_and_variable_lengths (TupleP[Wildcard,
     Wildcard, Variable("12345")]) =7


val test9ca = count_some_var ("x", Variable("x")) = 1
val test9cb = count_some_var ("x", Wildcard) = 0
val test9cc = count_some_var ("zo", TupleP([Variable("x"),
      Variable("zo"), Wildcard, Variable("zo")])) = 2
val test9cd = count_some_var ("zo", ConstructorP("zo",Wildcard)) = 0
val test9ce = count_some_var ("z", TupleP([ConstructorP("z",Wildcard),
 ConstructorP("z",Variable("z"))])) = 1


(* testing function  davai when check_pat was only davai
val test10a = check_pat (Variable("x"))
val test10b = check_pat (TupleP([Variable("y"),
         ConstructorP("z",Variable("q")),
                Variable("x"),Variable("zz"),Variable("x")])) *)


val test10a = check_pat (Variable("x")) = true
val test10b = check_pat (TupleP([Variable("x"),Variable("y")])) = true
val test10c = check_pat (TupleP([Variable("x"),Variable("y"),
                        ConstructorP("z",Variable("x"))])) = false

(*
val test11 = match (Const(1), UnitP) = NONE

val test12 = first_match Unit [UnitP] = SOME []
*)
