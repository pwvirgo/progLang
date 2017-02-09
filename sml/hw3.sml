(* Coursera Programming Languages, Homework 3, Provided Code *)

exception NoAnswer

datatype pattern = Wildcard
		 | Variable of string
		 | UnitP
		 | ConstP of int
		 | TupleP of pattern list
		 | ConstructorP of string * pattern

datatype valu = Const of int
	      | Unit
	      | Tuple of valu list
	      | Constructor of string * valu

fun g f1 f2 p =
    let 
	val r = g f1 f2 
    in
	case p of
	    Wildcard          => f1 ()
	  | Variable x        => f2 x
	  | TupleP ps         => List.foldl (fn (p,i) => (r p) + i) 0 ps
	  | ConstructorP(_,p) => r p
	  | _                 => 0
    end

(**** for the challenge problem only ****)

datatype typ = Anything
	     | UnitT
	     | IntT
	     | TupleT of typ list
	     | Datatype of string

(**** you can put all your code here ****)

(* 1.  only_capitals takes a string list and returns a string list
that has only the strings in the argument that start with an uppercase
letter. Assume all strings have at least 1 character. Use List.filter,
Char.isUpper, and String.sub to make a 1-2 line solution. *)

fun only_capitals(sl) =  
  List.filter (fn x => Char.isUpper(String.sub(x, 0))) sl


(* 2. longest_string1 takes a string list and returns the longest
string or "" if the list is empty. In the case of a tie, returns the
string closest to the beginning of the list. *)

fun longest_string1(sl: string list) =
  foldl (fn (a,b)=> if String.size a >  String.size b then a else b) "" sl


(* 3. longest_string2 is exactly like longest_string1 except in the
case of ties it returns the string closest to the end of the list. *)
fun longest_string2(sl: string list) =
  foldl (fn (a,b)=> if String.size a <  String.size b then b else a) "" sl

(* 4. longest_string_helper has type (int * int -> bool) -> string list
-> string *)

fun longest_string_helper F sl = 
  foldl (fn (a,b)=> if F(String.size a, String.size b)
                    then a else b) "" sl 

val longest_string3  = longest_string_helper
                 (fn (a,b) =>(if a >  b then true else false))

val longest_string4  = longest_string_helper
                 (fn (a,b) =>(if a >= b then true else false))

(* 5. longest_capitalized takes a string list and returns the longest
string in the list that begins with an uppercase letter, or "" if
there are no such strings. Assume all strings have at least 1
character. Use a val-binding and the ML library’s o operator for
composing functions. Resolve ties like in problem 2. *)

fun longest_capitalized (sl) = (longest_string3 o  only_capitals) sl 

(* 6. rev_string that takes a string and returns the string that is
the same characters in reverse order. Use ML’s o operator, the library
function rev for reversing lists, and two library functions in the
String module. (Browse the module documentation to find the most
useful functions.) *)

fun rev_string(sl) = (String.implode o List.rev o String.explode) sl


(* 7. first_answer of type (’a -> ’b option) -> ’a list -> ’b (notice
the 2 argu- ments are curried). The first argument should be applied
to elements of the second argument in order until the first time it
returns SOME v for some v and then v is the result of the call to
first_answer. If the first argument returns NONE for all list
elements, then first_answer should raise the exception
NoAnswer. Hints: Sample solution is 5 lines and does nothing fancy. *)

fun first_answer f lst =
   case lst of [] => raise NoAnswer 
            | h::t => case (f h) of SOME x => x 
                                 | _ => first_answer f t

(* 8. all_answers of type (’a -> ’b list option) -> ’a list -> ’b list
option (notice the 2 arguments are curried). The first argument should
be applied to elements of the second argument. If it returns NONE for
any element, then the result for all_answers is NONE. Else the calls
to the first argument will have produced SOME lst1, SOME lst2,
... SOME lstn and the result of all_answers is SOME lst where lst is
lst1, lst2, ..., lstn appended together (order doesn’t matter). Hints:
The sample solution is 8 lines. It uses a helper function with an
accumulator and uses @. Note all_answers f [] should evaluate to SOME
[]. *)

fun all_answers f lst = 
  let fun loop lst' acc =
     case lst' of [] => SOME  acc
            | h::t  => ( case f(h) of NONE =>  NONE
                         | SOME x  => loop t (acc @ x) )
  in loop lst [] end

(* (9a) Use g to define a function count_wildcards that takes a
pattern and returns how many Wildcard patterns it contains. *)

fun count_wildcards(pat) =  g (fn x => 1) (fn x => 0) pat 

(* (9b) Use g to define a function count_wild_and_variable_lengths that
takes a pattern and returns the number of Wildcard patterns it
contains plus the sum of the string lengths of all the variables in
the variable patterns it contains. (Use String.size. We care only
about variable names; the constructor names are not relevant.) *)

fun count_wild_and_variable_lengths(pat) =
   g (fn x => 1) (fn x =>String.size(x)) pat

(* (9c) Use g to define a function count_some_var that takes a string
and a pattern (as a pair) and returns the number of times the string
appears as a variable in the pattern. We care only about variable
names; the constructor names are not relevant. *)

fun count_some_var(str, pat) =
   g (fn x => 0) (fn x=> if str = x then 1 else 0) pat

(* 10. check_pat that takes a pattern and returns true if and only if
all the variables appearing in the pattern are distinct from each
other (i.e., use different strings). *)

fun check_pat p =
  let fun davai (p1) =
        case p1 of Variable x => [x] 
          | TupleP ps  => foldl(fn (a,b) => (davai a) @ b) [] ps
          | ConstructorP(_,p)  => davai(p)
          | _  => []
      fun drugoi l =
        case l of [] => true
          | h::t  => if (List.exists (fn x=> h=x)  t) then false
                     else drugoi t
  in 
      (drugoi o davai) p
  end


(* 11. function match that takes a valu * pattern and returns a
(string * valu) list option, namely NONE if the pattern does not match
and SOME lst where lst is the list of bindings if it does. Note that
if the value matches but the pattern has no patterns of the form
Variable s, then the result is SOME []. Hints: Sample solution has one
case expression with 7 branches. The branch for tuples uses
all_answers and ListPair.zip. Sample solution is 13 lines. Remember to
look above for the rules for what patterns match what values, and what
bindings they produce. These are hints: We are not requiring
all_answers and ListPair.zip here, but they make it easier. *)

