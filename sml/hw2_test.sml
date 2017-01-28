(* Homework2 Simple Test *)
use "hw2.sml";
(* To run the test, add a new line to the top of this file: use
"homeworkname.sml"; All the tests should evaluate to true. For
example, the REPL should say: val test1 = true : bool *)

val test1a  = all_except_option ("string", ["string"]) = SOME []
val test1b  = all_except_option("delete me", ["abc", "delete me", "efg"])
              =SOME ["abc", "efg"]
val test1c  = all_except_option ("a", ["q", "r", "s"]) = NONE


val test2a = get_substitutions1 ([["foo"],["there"]], "foo") = []
val test2b = get_substitutions1([["Fred","Fredrick"],["Elizabeth",
         "Betty"],["Freddie","Fred","F"]], "Fred")=["Fredrick","Freddie","F"]
val test2c =  get_substitutions1([["Fred","Fredrick"],["Jeff","Jeffrey"],
    ["Geoff","Jeff","Jeffrey"]], "Jeff")=["Jeffrey","Geoff","Jeffrey"]


val test3a = get_substitutions2 ([["foo"],["there"]], "foo") = []
val test3b = get_substitutions2([["Fred","Fredrick"],["Elizabeth",
         "Betty"],["Freddie","Fred","F"]], "Fred")=["Fredrick","Freddie","F"]
val test3c =  get_substitutions2([["Fred","Fredrick"],["Jeff","Jeffrey"],
    ["Geoff","Jeff","Jeffrey"]], "Jeff")=["Jeffrey","Geoff","Jeffrey"]


val test4a = similar_names
   ([["Fred","Fredrick"],["Elizabeth","Betty"],["Freddie","Fred", "F"]],
	    {first="Fred", middle="W", last="Smith"}) =
	    [{first="Fred", last="Smith", middle="W"},
	    {first="Fredrick", last="Smith", middle="W"},
	    {first="Freddie", last="Smith", middle="W"}, {first="F",
	    last="Smith", middle="W"}]
val test4b = similar_names([[]], {first="f",middle="m", last="l"})=
             [{first="f",middle="m", last="l"}]

val test5a = card_color (Clubs, Num 2) = Black
val test5b = card_color (Hearts, Queen) = Red


val test6a = card_value (Clubs, Num 2) = 2
val test6b = card_value (Diamonds, Jack) = 10
val test6c = card_value (Spades, Ace) = 11


(*
val test7 = remove_card ([(Hearts, Ace)], (Hearts, Ace), IllegalMove) = []

val test8 = all_same_color [(Hearts, Ace), (Hearts, Ace)] = true

val test9 = sum_cards [(Clubs, Num 2),(Clubs, Num 2)] = 4

val test10 = score ([(Hearts, Num 2),(Clubs, Num 4)],10) = 4

val test11 = officiate ([(Hearts, Num 2),(Clubs, Num 4)],[Draw], 15) = 6

val test12 = officiate ([(Clubs,Ace),(Spades,Ace),(Clubs,Ace),(Spades,Ace)],
                        [Draw,Draw,Draw,Draw,Draw],
                        42)
             = 3

val test13 = ((officiate([(Clubs,Jack),(Spades,Num(8))],
                         [Draw,Discard(Hearts,Jack)],
                         42);
               false) 
              handle IllegalMove => true)
             
*)
