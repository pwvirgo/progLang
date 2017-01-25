(* Dan Grossman, Coursera PL, HW2 Provided Code *)

(* compare 2 strings and return true if they are the same.  
   Avoiding several of the functions in problem 1 having
   polymorphic types that may be confusing *)
fun same_string(s1 : string, s2 : string) =  s1 = s2

(* put your solutions for problem 1 here *)


(* 1. This problem involves using first-name substitutions to come up
with alternate names. For example, Fredrick William Smith could also
be Fred William Smith or Freddie William Smith. Only part (d) is
specifically about this, but the other problems are helpful. *)

(* (a) Write a function all_except_option, which takes a string and a
string list. Return NONE if the string is not in the list, else return
SOME lst where lst is identical to the argument list except the string
is not in it. You may assume the string is in the list at most
once. Use same_string, provided to you, to compare strings. Sample
solution is around 8 lines. *)

fun all_except_option(s: string, sl: string list) =
  let 
    fun getString(found: bool, sl2: string list) =
       case sl2 of [] => (found, [])
          | h::sl'  =>  
             if same_string(s, h) then getString(true, sl')
             else case getString(found, sl') of 
                (f, lst) => (f, h :: lst) 
  in                                
      case getString(false, sl)  of (found, lst) =>
           if found then SOME lst  else  NONE 
  end;

(* (1b) Write a function get_substitutions1, which takes a string
list list (a list of list of strings, the substitutions) and a
string s and returns a string list. The result has all the strings
that are in some list in substitutions that also has s, but s
itself should not be in the result. Example:
*)

(* you may assume that Num is always used with values 2, 3, ..., 10
   though it will not really come up *)
fun aeo(s: string, sl: string list) =
       case sl of [] => []
          | h::sl'  =>  
             if same_string(s, h) then aeo(s, sl')
             else h:: aeo(s, sl');
(*
val testaeo1  = aeo ("string", ["string"]) =[]
val testaeo2  = aeo("delete me", ["abc", "delete me", "efg"])
              = ["abc", "efg"]
val testaeo3  = aeo ("a", ["q", "r", "s"]) = ["q", "r", "s"];
*)
fun get_substitutions1(x: string list list, s: string) = 
   case x of []::[] => []
          | head::[tail]  =>  aeo(s, head) @ aeo(s,tail);


(*
datatype suit = Clubs | Diamonds | Hearts | Spades
datatype rank = Jack | Queen | King | Ace | Num of int 
type card = suit * rank

datatype color = Red | Black
datatype move = Discard of card | Draw 

exception IllegalMove
*)
(* put your solutions for problem 2 here *)