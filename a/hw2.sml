(* Dan Grossman, Coursera PL, HW2 Provided Code *)

(* if you use this function to compare two strings (returns true if the same
   string), then you avoid several of the functions in problem 1 having
   polymorphic types that may be confusing *)
fun same_string(s1 : string, s2 : string) =
    s1 = s2

(* put your solutions for problem 1 here *)

(* you may assume that Num is always used with values 2, 3, ..., 10
   though it will not really come up *)
datatype suit = Clubs | Diamonds | Hearts | Spades
datatype rank = Jack | Queen | King | Ace | Num of int 
type card = suit * rank

datatype color = Red | Black
datatype move = Discard of card | Draw 

exception IllegalMove

(* put your solutions for problem 2 here *)

(* HARD for me QAQ... *)
(* Q1 *)
fun all_except_option(str1, strs) =
    case strs of
        [] => NONE
      | str2::strs' => if same_string(str1, str2)
                       then SOME strs'
                       else case all_except_option(str1, strs') of
                                NONE => NONE
                              | SOME strlst => SOME(str2::strlst)

(* Q2 *)
fun get_substitutions1(substitutions, s) =
    case substitutions of
        [] => []
      | subsubstitutions::substitutions' => case all_except_option(s, subsubstitutions) of
                                                NONE => get_substitutions1(substitutions', s)
                                              | SOME sub_list => sub_list @ get_substitutions1(substitutions', s)

(* Q3 *)
fun get_substitutions2(substitutions, s) =
    let
        fun aux(substitutions, s, acc) =
            case substitutions of
                [] => acc
              | subsubstitutions::substitutions' => case all_except_option(s, subsubstitutions) of
                                                        NONE => aux(substitutions', s, acc)
                                                      | SOME sub_list => aux(substitutions', s, (acc @ sub_list))
    in
        aux(substitutions, s, [])
    end

(* Q4 *)
fun similar_names(substitutions, {first=f, middle=m, last=l}) =
    let
        fun aux(substitutions, {first=f, middle=m, last=l}) =
            case substitutions of
                [] => []
              | sub::substitutions' => {first=sub, middle=m, last=l}::aux(substitutions', {first=f, middle=m, last=l})
    in
        {first=f, middle=m, last=l}::aux(get_substitutions2(substitutions, f), {first=f, middle=m, last=l})
    end

(* Q5 *)
fun card_color(card) =
    case card of
        (Hearts, _) =>  Red
      | (Diamonds, _) => Red
      | (Spades, _) => Black
      | (Clubs, _) => Black

(* Q6 *)
fun card_value(card) =
    case card of
        (_, Num number) => number
      | (_, Ace) => 11
      | _ => 10

(* Q7 *)
fun remove_card(cs, c, e) =
    case cs of
        [] => raise e
      | card::cs' => if card = c
                     then cs'
                     else card::remove_card(cs', c, e)

(* Q8 *)
fun all_same_color(cs) =
    case cs of
        [] => true
      | _::[] => true  (* Maybe redundant? With this line reduce an error... *)
      | head::(neck::rest) => if card_color(head) = card_color(neck)
                              then all_same_color(rest)
                              else false

(* Q9 *)
fun sum_cards(cs) =
    let
        fun aux(cs, acc) =
            case cs of
                [] => acc
              | card::cs' => aux(cs', acc + card_value(card))
    in
        aux(cs, 0)
    end

(* Q10 *)
fun score(cs, goal) =
    let
        val sum = sum_cards(cs)
        val p = if sum >= goal
                then 3 * (sum - goal)
                else (goal - sum)
    in
        if all_same_color(cs)
        then p div 2
        else p
    end

(* Q11 *)
fun officiate(cs, moves, goal) =
    let
        fun play(cs, moves, held) =
            case moves of
                [] => held
              | (Discard c)::moves' => play(cs, moves', remove_card(held, c, IllegalMove))
              | Draw::moves' => case cs of
                                    [] => held
                                  | c::cs' => if sum_cards(c::held) >= goal
                                              then c::held
                                              else play(cs', moves', c::held)
    in
        score(play(cs, moves, []), goal)
    end
