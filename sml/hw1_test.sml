(* Homework1 Simple Test *)
(* These are basic test cases. Passing these tests does not guarantee that your code will pass the actual homework grader *)
(* To run the test, add a new line to the top of this file: use "homeworkname.sml"; *)
(* All the tests should evaluate to true. For example, the REPL should say: val test1 = true : bool *)


val test1a = is_older ((1,2,3),(2,3,4)) = true
val test1b = is_older ((2,3,4),(1,2,3)) = false
val test1c = is_older ((10,10,10), (10,10,10)) = false
val test1d = is_older ((1,2,1), (1,1,1)) = false
val test1e = is_older ((5,5,5), (5,5,6)) = true

val test2a = number_in_month ([(2012,2,28),(2013,12,1)],2) = 1
val test2b = number_in_month ([],2) = 0
val test2c = number_in_month ([(2012,7,28),(2013,3,1), (1,7,7)],2) = 0
val test2d = number_in_month ([(2012,7,28),(2013,3,1), (1,7,7)],7) = 2

val test3a = number_in_months ([(2012,2,28),(2013,12,1),(2011,3,31),(2011,4,28)],[2,3,4]) = 3
val test3b = number_in_months ([],[]) = 0
val test3c = number_in_months ([(1,2,1),(1,2,1),(1,3,1),(1,2,28)],[]) = 0
val test3d = number_in_months ([],[2]) = 0
val test3e = number_in_months ([(1,2,1),(1,2,1),(1,3,1),(1,2,28)],[2,7]) = 3

val test4a = dates_in_month ([(2012,2,28),(2013,12,1)],2) = [(2012,2,28)]
val test4b = dates_in_month ([],2) = []
val test4c = dates_in_month ([(2012,2,28),(2013,2,1)],2) =
             [(2012,2,28),(2013,2,1)]
val test4d = dates_in_month ([(2012,2,28),(2013,12,1)],5) = []

val test5a = dates_in_months ([(2012,2,28),(2013,12,1),(2011,3,31),(2011,4,28)],[2,3,4]) = [(2012,2,28),(2011,3,31),(2011,4,28)]

val test5b = dates_in_months ([],[]) = []
val test5c = dates_in_months ([(1,2,3)],[]) = []
val test5d = dates_in_months ([(1,2,3),(1,6,3),(1,4,4),(2,6,2),(2,4,5)],[4,6])=
             [(1,4,4), (2,4,5), (1,6,3), (2,6,2)]

val test6a = get_nth (["hi", "there", "how", "are", "you"], 2) = "there"
val test6b = get_nth (["hi", "there", "how", "are", "you"], 1) = "hi"
val test6c = get_nth (["hi", "there", "how", "are", "you"], 9) = ""
val test6d = get_nth ([], 2) = ""

val test7a = date_to_string (2013, 6, 1) = "June 1, 2013"
val test7b = date_to_string (2017, 1, 18) = "January 18, 2017"
val test7c = date_to_string (1919, 12, 31) = "December 31, 1919"

val test8a = number_before_reaching_sum (10, [1,2,3,4,5]) = 3
val test8b = number_before_reaching_sum (1,  [1,2,3,4,5]) = 0
val test8c = number_before_reaching_sum (2,  [1,2,3,4,5]) = 1
val test8d = number_before_reaching_sum (11, [1,2,3,4,5]) = 4

val test9a = what_month 70 = 3
val test9b = what_month 1 = 1
val test9c = what_month 31 = 1
val test9d = what_month 32 = 2
val test9e = what_month 334 = 11
val test9f = what_month 335 = 12
val test9g = what_month 365 = 12

val test10a = month_range (31, 34) = [1,2,2,2]
val test10b = month_range (334,334) = [11]
val test10c = month_range (334, 366) = []
val test10d = month_range (1,4) = [1,1,1,1]
val test10e = month_range (359, 365) = [12,12,12,12,12,12,12]

val test11a = oldest([(2012,2,28),(2011,3,31),(2011,4,28)]) = SOME (2011,3,31)
val test11b = oldest([]) = NONE
val test11c = oldest([(2012,2,28)]) = SOME (2012,2,28)
val test11d = oldest([(12,2,28),(11,3,31),(11,4,28),(11,3,29)])
                     = SOME (11,3,29)

