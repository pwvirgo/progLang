(* sml stuff week2 of "programming Languages part a*)
(* functions lecture *)

(* create a sinlge list of int from 2 lists of int *)
fun append(xs : int list, ys: int list) =
  if null xs then ys
  else hd xs :: append(tl xs, ys);

 append([1,2,3], [4,5,6]);

(*  sum all the elements in a list of pairs of ints *)
fun sum_pair_list(xs: (int * int) list) =
  if null(xs) then 0
  else #1 (hd xs) + #2 (hd xs) + sum_pair_list(tl xs);

sum_pair_list([(1,2), (3,4), (5,6)]);
 

(* create a list of the first element of a list of int pairs *)
fun firsts(xs : (int * int) list) =
  if null xs then []
  else #1 (hd xs) :: firsts(tl xs);

 firsts([(1,2), (5,6), (7,8)]);

(* pattern matching for lists of lists *) 

fun gsok(lls: string list list, s: string) = 
  case lls of [] => NONE
           | (a::b) :: [c::[d]]  =>  SOME b;
 
 gsok([["a","b"],["WW","ZZ"]], "a");  
