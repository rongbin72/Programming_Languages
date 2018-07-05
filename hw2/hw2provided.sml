(* Dan Grossman, Coursera PL, HW2 Provided Code *)

(* if you use this function to compare two strings (returns true if the same
   string), then you avoid several of the functions in problem 1 having
   polymorphic types that may be confusing *)
fun same_string(s1 : string, s2 : string) =
    s1 = s2

(* put your solutions for problem 1 here *)

fun all_except_option(str, []) = NONE
  | all_except_option(str, head :: tail) =
        case (same_string(str, head), all_except_option(str, tail)) of
            (true, _) => SOME(tail)
          | (false, SOME(ls)) => SOME(head :: ls) 
          | (false, NONE) => NONE


fun get_substitutions1([], s) = []
  | get_substitutions1(head :: tail, s) =
        case (all_except_option(s, head), get_substitutions1(tail, s)) of
            (NONE, _) => []
          | (SOME(ls), []) => ls
          | (SOME(ls), ll) => ls @ ll


fun get_substitutions2([], s) = []
  | get_substitutions2(sub, s) =
        let 
            fun aux([], acc) = acc
              | aux(head :: tail, acc) =
                    case all_except_option(s, head) of
                        NONE => aux(tail, acc)
                      | SOME(ls) => aux(tail, acc @ ls)
        in
            aux(sub, [])
        end


fun similar_names(sub, {first, middle, last}) =  
    let
        val ls = first :: get_substitutions2(sub, first)

        fun aux([], acc) = acc
           | aux(head :: tail, acc) = aux(tail, acc @ [{first = head, middle = middle, last = last}])
    in
        aux(ls, [])
    end


(* you may assume that Num is always used with values 2, 3, ..., 10
   though it will not really come up *)
datatype suit = Clubs | Diamonds | Hearts | Spades
datatype rank = Jack | Queen | King | Ace | Num of int
type card = suit * rank

datatype color = Red | Black
datatype move = Discard of card | Draw

exception IllegalMove

(* put your solutions for problem 2 here *)
fun card_color(suit, rank) =
    case suit of
        Spades => Black
      | Clubs  => Black
      | _      => Red  
      

(* fun card_value() *)