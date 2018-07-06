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
        case all_except_option(s, head) of
            NONE => get_substitutions1(tail, s)
          | SOME ls => ls @ get_substitutions1(tail, s)  


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
      

fun card_value(suit, rank) =
    case rank of
        Ace   => 11
      | Num n => n  
      | _     => 10


fun remove_card([], c, e) = raise e
  | remove_card(head :: tail, c, e) =
        if head = c
        then tail  (* only remove the first one if c is in the list more than once *)
        else head :: remove_card(tail, c, e)


(* If c is in the list more than once, remove all the cards
    raise except if c in not in the list *)
(* fun remove_card([], c, e) = raise e
  | remove_card(card, c, e) =
        let
            fun aux(card, f) =
                case (card, f) of
                    ([], true)        => []
                  | ([], false)       => raise e
                  | (head :: tail, _) => if head = c then aux(tail, true) else head :: aux(tail, false)
        in
            aux(card, false)
        end *)


fun all_same_color(card_ls) =
    case card_ls of 
        [] => true
      | _ :: [] => true
      | head :: neck :: tail => card_color(head) = card_color(neck) andalso all_same_color(neck :: tail)


fun sum_cards(card_ls) =
    let
        fun aux(ls, acc) =
            case ls of
                [] => acc
              | head :: tail => aux(tail, acc + card_value(head))
    in
        aux(card_ls, 0)
    end


fun score(card_ls, goal) =
    let
        val sum = sum_cards(card_ls)
    in
        case (sum > goal, all_same_color(card_ls)) of
            (true, true)   => 3 * (sum - goal) div 2
          | (true, false)  => 3 * (sum - goal)
          | (false, true)  => (goal - sum) div 2
          | (false, false) => goal - sum
    end


fun officiate(card_ls, move_ls, goal) =
    let
        fun aux(cards, held_cards, [], current_score) = current_score
          | aux([], held_cards, moves, current_score) = current_score
          | aux(h_card :: t_cards, held_cards, h_move :: t_moves, current_score) =  
                case h_move of
                    Discard c => let
                                    val current_held = remove_card(held_cards, c, IllegalMove)
                                 in
                                    aux(h_card :: t_cards, current_held, t_moves, score(current_held, goal))
                                 end
                  | Draw => let
                                val current_held = h_card :: held_cards
                            in
                                if sum_cards(current_held) > goal
                                then current_score
                                else aux(t_cards, current_held,t_moves, score(current_held, goal))
                            end
    in
        aux(card_ls, [], move_ls, 0)
    end