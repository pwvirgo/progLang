fun same_string(s1 : string, s2 : string) =  s1 = s2

(* 1a *)
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

fun aeo(s: string, sl: string list) =
       case sl of [] => []
          | h::sl'  =>  
             if same_string(s, h) then aeo(s, sl')
             else h:: aeo(s, sl');

val test1a  = aeo ("string", ["string"]) =[]
val test1b  = aeo("delete me", ["abc", "delete me", "efg"])
              = ["abc", "efg"]
val test1c  = aeo ("a", ["q", "r", "s"]) = ["q", "r", "s"];


(* (1b) Write a function get_substitutions1, which takes a string list
list (a list of list of strings, the substitutions) and a string s and
returns a string list. The result has all the strings that are in some
list in substitutions that also has s, but s itself should not be in
the result. Example: *)


fun get_substitutions1(x: string list list, s: string) = 
   case x of []::[] => []
          | head::[tail]  =>  aeo(s, head) @ aeo(s,tail);
 

 
 get_substitutions1([["a","b"],["WW","ZZ"]], "a");

(*
fun gsok(lls: string list list, s: string) = 
  case lls of [] => NONE
           | a :: [h2::t2]  =>  SOME h2;
 
 gsok([["a","b"],["WW","ZZ"]], "a");
*)




