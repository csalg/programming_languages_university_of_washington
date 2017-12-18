fun testme (stuff_to_test, expected_output) =
	let
		exception test_exception
	in
		if stuff_to_test = expected_output then NONE else raise test_exception
	end;


(* =======================================================================================================================================
																QUESTION 1

a)

String, String List -> String List Option
Return NONE if the string is not in the list, else return SOME lst where lst is identical to the argument list except the string is not in it.
*)
fun all_except_option(str, lst) =
	let
		fun get_list(lst) = 
			case lst of
				[] => []
			|	x::xs => if str = x then get_list(xs) else x::(get_list(xs));
		val processed_lst = get_list(lst);
	in
		if processed_lst = lst
		then NONE
		else SOME processed_lst
	end;


testme(all_except_option("yo", ["my", "yo", "mama", "help"]), SOME ["my","mama","help"]);
testme(all_except_option("10", ["my", "yo", "mama", "help"]), NONE);


(*
(b) Write a function get_substitutions1, which takes a string list list (a list of list of strings, the substitutions) and a string s and returns a string list. The result has all the strings that are in some list in substitutions that also has s, but s itself should not be in the result. Example:
     get_substitutions1([["Fred","Fredrick"],["Elizabeth","Betty"],["Freddie","Fred","F"]],
                        "Fred")
                        *)
     (* answer: ["Fredrick","Freddie","F"]

Assume each list in substitutions has no repeats. The result will have repeats if s and another string are both in more than one list in substitutions. Example:
     get_substitutions1([["Fred","Fredrick"],["Jeff","Jeffrey"],["Geoff","Jeff","Jeffrey"]],
                        "Jeff")
answer: ["Jeffrey","Geoff","Jeffrey"] 
Use part (a) and ML’s list-append (@) but no other helper functions. Sample solution is around 6 lines.
	*)


fun get_substitutions1(str, lst) =
	let
		fun in_lst lst  =
			case lst of
			[] => false 
		| x::xs => if x=str then true else in_lst(xs);
	in
		case lst of
				[] => []
			|	x::xs => if in_lst(hd lst) then valOf(all_except_option(str, hd lst))@(get_substitutions1(str, xs)) else (get_substitutions1(str, xs))
	end;

get_substitutions1("Fred", [["Fred","Fredrick"],["Elizabeth","Betty"],["Freddie","Fred","F"]]);
get_substitutions1("Jeff", [["Fred","Fredrick"],["Jeff","Jeffrey"],["Geoff","Jeff","Jeffrey"]]);


(*
(c) Write a function get_substitutions2, which is like get_substitutions1 except it uses a tail-recursive local helper function.
*)


fun get_substitutions2(str, lst) =
	let
		fun in_lst lst  =
			case lst of
			[] => false 
		| x::xs => if x=str then true else in_lst(xs);
		fun tail_recursion (lst, acc) =
			case lst of
				[] => acc
			|	x::xs => 
					if in_lst(hd lst) then tail_recursion(xs, valOf(all_except_option(str, hd lst))@acc) else tail_recursion(xs, acc);
	in
		tail_recursion(lst, [])
	end;

get_substitutions2("Fred", [["Fred","Fredrick"],["Elizabeth","Betty"],["Freddie","Fred","F"]]);
get_substitutions2("Jeff", [["Fred","Fredrick"],["Jeff","Jeffrey"],["Geoff","Jeff","Jeffrey"]]);


(*
(d) Write a function similar_names, which takes a string list list of substitutions (as in parts (b) and (c)) and a full name of type {first:string,middle:string,last:string} and returns a list of full names (type {first:string,middle:string,last:string} list). The result is all the full names you can produce by substituting for the first name (and only the first name) using substitutions and parts (b) or (c). The answer should begin with the original name (then have 0 or more other names). Example:
     similar_names([["Fred","Fredrick"],["Elizabeth","Betty"],["Freddie","Fred","F"]],
                   {first="Fred", middle="W", last="Smith"})
		answer: [{first="Fred", last="Smith", middle="W"},
                 {first="Fredrick", last="Smith", middle="W"},
                 {first="Freddie", last="Smith", middle="W"},
                 {first="F", last="Smith", middle="W"}] 
Do not eliminate duplicates from the answer. Hint: Use a local helper function. Sample solution is around 10 lines.
*)

fun similar_names (name_lists, {first, last, middle}) =
	let
		val synonyms = get_substitutions2(first, name_lists);
		fun build_list(syns, acc) =
			case syns of
					[] => acc
				|	x::xs => build_list (xs, {first=x, last=last, middle=middle}::acc);
	in
		build_list(synonyms, [])		
	end;
	


similar_names([["Fred","Fredrick"],["Elizabeth","Betty"],["Freddie","Fred","F"]],
                   {first="Fred", middle="W", last="Smith"});

(* =======================================================================================================================================
																QUESTION 2
*)

(*     Data types     *)
datatype suit = Clubs | Diamonds | Hearts | Spades;
datatype rank = Jack | Queen | King | Ace | Num of int;
type card = suit * rank;

datatype color = Red | Black
datatype move = Discard of card | Draw 

exception IllegalMove

(*							Data Examples						*)

val bunch_of_cards = [(Spades,Num 1), (Spades,Num 2), (Spades,Num 3), (Diamonds, Num 5), (Clubs, Ace)];
val bunch_of_cards2 = [(Spades,Num 1), (Spades,Num 2), (Spades,Num 3), (Spades, Num 5), (Spades, Ace)];

val bunch_of_moves = [Draw,Draw,Draw, Discard (Spades, Num 1)];
val bunch_of_moves2 = [Draw,Draw,Draw,Draw];

(*
a)
Card -> String
Returns card color (spades and clubs are black, diamonds and hearts are red). Note: One case-expression is enough.
*)
fun card_color ((Spades|Clubs), _) = Black | card_color ((Diamonds|Hearts), _) = Red;

card_color(Spades, King);
card_color(Diamonds, Ace);


(*
b)
Card -> Natural[1,11]
Takes a card and returns its value (numbered cards have their number as the value, aces are 11, everything else is 10)
*)
fun card_value (_,Num x) = x | card_value (_,Ace) = 11 | card_value (_,(Jack | Queen | King)) = 10;

card_value(Spades,Num 5);
card_value(Spades,Ace);
card_value(Spades,King);


(*
c)
ListOfCard, Card, Exception -> ListOfCard
Returns a list that has all the elements of cs except c. If c is not in the list throws exception.
*)

exception CardNotFound;

fun remove_card (cards, chosen_card : card, ex) =
	let
		fun remove_card_tail (card_list, acc, found) =
			case card_list of
					[] => if found then acc else raise ex
				|	x::xs => if x = chosen_card then remove_card_tail(xs, acc, true) else remove_card_tail(xs, x::acc, found)
	in
		remove_card_tail(cards, [], false)
	end;

remove_card(bunch_of_cards, (Diamonds, Num 5), CardNotFound);

(*
d)
card list => boolean
Returns true if all the cards in the list are the same color.
*)

fun all_same_color lst = 
	case lst of
			[] => true
		|	card1::card2::cards => if card_color(card1) = card_color(card2) then all_same_color(card2::cards) else false
		|	card::[] => true;

all_same_color bunch_of_cards;
all_same_color [(Diamonds, Num 1), (Diamonds, Num 2)];


(*
e)
card list => int
Returns the sum of the card values. 
Uses a locally defined helper function that is tail recursive. (Take “calls use a constant amount of stack space” as a requirement for this problem.)
*)

fun sum_card lst =
	let
		fun sum_card_tail (cards_to_add, sum_so_far) =
			case cards_to_add of
				[] => sum_so_far
			|	x::xs => sum_card_tail (xs, (card_value(x)+ sum_so_far))
	in
		sum_card_tail (lst, 0)
	end;

sum_card(bunch_of_cards);

(*
f)
card list, int -> int
Write a function score, which takes a card list (the held-cards) and an int (the goal) and computes the score as described above.
*)
fun score (held_cards, goal) =
	let
		val sum_of_cards = sum_card(held_cards);
		val prelim = if sum_of_cards > goal then 3*(sum_of_cards - goal) else (goal - sum_of_cards)
	in
		if all_same_color(held_cards) then prelim div 2 else prelim
	end;

score(bunch_of_cards, 5);
score(bunch_of_cards2, 5);


(*
g)
card list, move list, goal -> int
Returns the score at the end of the game after processing (some or all of) the moves in the move list in order. 
Use a locally defined recursive helper function that takes several arguments that together represent the current state of the game.
*)


fun officiate (all_cards, move_list, goal) =
	let
		fun game_engine(remaining_cards, remaining_moves, held_cards) =
			case remaining_moves of
					[] => score(held_cards,goal)
				|	(Discard x)::xs =>  	game_engine(
														remaining_cards,
														xs,
														remove_card(held_cards, x, IllegalMove))
				|	(Draw)::xs => 			game_engine(
														tl remaining_cards,
														xs,
														hd remaining_cards::held_cards
														)
	in
		game_engine(all_cards, move_list, [])
	end;

officiate(bunch_of_cards, bunch_of_moves, 5);
officiate(bunch_of_cards, bunch_of_moves2, 5)







