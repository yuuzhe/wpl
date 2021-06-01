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

(* Q1 *)
fun only_capitals strs =
    List.filter (fn str => Char.isUpper(String.sub(str, 0))) strs

(* Q2 *)
fun longest_string1 strs =
    foldl (fn (s1, s2) => if String.size s1 > String.size s2 then s1 else s2) "" strs

(* Q3 *)
fun longest_string2 strs =
    foldl (fn (s1, s2) => if String.size s1 >= String.size s2 then s1 else s2) "" strs

(* Q4 *)
(* (int * int -> bool) -> string list -> string *)
fun longest_string_helper f strs =
    foldl (fn (s1, s2) => if f(String.size s1, String.size s2) then s1 else s2) "" strs

val longest_string3 = longest_string_helper (fn (s1, s2) => s1 > s2)

val longest_string4 = longest_string_helper (fn (s1, s2) => s1 >= s2)

(* Q5 *)
val longest_capitalized = longest_string1 o only_capitals

(* Q6 *)
val rev_string = String.implode o rev o String.explode

(* Q7 *)
fun first_answer f xs =
    case xs of
        [] => raise NoAnswer
      | x::xs' => case f x of
                      NONE => first_answer f xs'
                    | SOME v => v

(* Q8 *)
fun all_answers f xs =
    let fun aux(acc, xs) =
        case xs of
            [] => SOME acc
          | x::xs' => case f x of
                          NONE => NONE
                        | SOME v => aux(v @ acc, xs')
    in
        aux([], xs)
    end

(* Q9 *)
val count_wildcards = g (fn() => 1) (fn x => 0)

val count_wild_and_variable_lengths = g (fn() => 1) String.size

fun count_some_var (str, p) = g (fn() => 0) (fn x => if x = str then 1 else 0) p

(* Should review for Q10~Q11... *)

(* Q10 *)
fun check_pat p =
    let
        (* p -> string list *)
        fun extract (acc, p) =
            case p of
                Variable s => s::acc
              | TupleP ps => List.foldl (fn (p, tmp) => extract(acc, p) @ tmp) [] ps
              | ConstructorP(_,p) => extract(acc, p)
              | _ => acc
                
        (* string list -> bool *)
        fun uniq strs =
            case strs of
                [] => true
              | str::strs' => (not (List.exists (fn s => s = str) strs')) andalso uniq strs'
    in
        uniq (extract([], p))
    end

(* Q11 *)
(* fn (v,p) -> (string * valu) list option *)
fun match(valu, pat) =
    case (valu, pat) of
        (_, Wildcard) => SOME []  
      | (_, Variable s) => SOME [(s,valu)]
      | (Unit, UnitP) => SOME []
      | (Const v, ConstP p) => if p = v then SOME [] else NONE
      | (Tuple vs, TupleP ps) => if List.length vs = List.length ps
                                 then all_answers match (ListPair.zip(vs, ps))
                                 else NONE
      | (Constructor(_, v), ConstructorP(_, p)) => match(v,p)
      | _ => NONE
        
(* Q12 *)    
fun first_match valu pats =
    SOME (first_answer (fn p => match(valu,p)) pats)
    handle NoAnswer => NONE
