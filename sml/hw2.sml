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

fun get_substitutions1(x: string list list, s: string) =
     case x of [] => [] 
          | h::t  => 
             (case all_except_option(s, h) of NONE  => []
                  | SOME lst => lst)
                @ get_substitutions1(t, s);

(* (1c) Write a function get_substitutions2, which is like
get_substitutions1 except it uses a tail-recursive local helper
function. *)
fun get_substitutions2(x: string list list, s: string) =
  let fun gets(x2: string list list, ret: string list) =
      case x2 of [] => ret
          | h::t  => 
             gets(t, ret @ (case all_except_option(s, h) of NONE  => []
                  | SOME lst => lst))
  in
      gets(x, [])
  end;
 
(* (1d) similar_names takes a string list list of substitutions (as in
parts (b) and (c)) and a full name of type
{first:string,middle:string,last:string} and returns a list of full
names (type {first:string,middle:string,last:string} list). The result
is all the full names you can produce by substituting for the first
name (and only the first name) using substitutions and parts (b) or
(c). The answer should begin with the original name (then have 0 or
more other names).  *)

fun similar_names(x: string list list, 
                  name: {first:string,middle:string,last:string})=
  case x of [] => [name]
       | h::t  => 
         let val all= get_substitutions2(x, 
               case name of {first=f, middle=_, last=_}=>f)
             val mid=case name of {first=_, middle=m, last=_}=>m
             val las=case name of {first=_, middle=_, last=l}=>l
             fun getem(namelist: string list)= 
               case namelist of []=>[]
                  | h::t => {first=h, middle=mid, last=las} ::
                        getem(t)
         in
             name :: getem(all)
         end;

                                 

datatype suit = Clubs | Diamonds | Hearts | Spades
datatype rank = Jack | Queen | King | Ace | Num of int 
type card = suit * rank

datatype color = Red | Black
datatype move = Discard of card | Draw 

exception IllegalMove


(* put your solutions for problem 2 here *)
(* (2a) Write a function card_color, which takes a card and returns
its color (spades and clubs are black, diamonds and hearts are
red). Note: One case-expression is enough. *)

fun card_color(c: card) =
  case c of (Diamonds, _) =>Red  | (Hearts,_) =>Red 
                      | _  => Black;

(* (2b) Write a function card_value, which takes a card and returns its
value (numbered cards have their number as the value, aces are 11,
everything else is 10). Note: One case-expression is enough. *)
fun card_value (c: card) =
  case c of (_, Ace) => 11 | (_, Num a) => a | _  => 10;

(* (2c) Write a function remove_card, which takes a list of cards cs, a
card c, and an exception e. It returns a list that has all the
elements of cs except c. If c is in the list more than once, remove
only the first one. If c is not in the list, raise the exception
e. You can compare cards with =. *)

fun remove_card (cs: card list, c: card, e: exception)=
42;
