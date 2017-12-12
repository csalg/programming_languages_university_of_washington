(* Homework1 Simple Test *)
(* These are basic test cases. Passing these tests does not guarantee that your code will pass the actual homework grader *)
(* To run the test, add a new line to the top of this file: use "homeworkname.sml"; *)
(* All the tests should evaluate to true. For example, the REPL should say: val test1 = true : bool *)

(* =================================================================================
	Helpers  *)
(* list list -> int list *)
fun return_last (lst : int list) = 
	if null(tl(lst))
	then hd lst
	else return_last(tl(lst));

(* =================================================================================
	Functions  *)

fun is_older (date1 : int*int*int,  date2 : int*int*int) =
	let
		fun to_days (date : int*int*int) =
			(#1 date * 365) + (#2 date * 31) + #3 date
	in
		to_days date1 < to_days date2
	end;


fun number_in_month (lst : (int*int*int) list, month : int) =
	let
		fun month_search (iter_lst : (int*int*int) list, month : int, count : int) =
			if null iter_lst
			then count
			else
				let
					val this_month = if (#2 (hd(iter_lst)) = month) then 1 else 0;
				in
					month_search(tl(iter_lst), month, count + this_month)
				end;
	in
		month_search (lst, month, 0)
	end;

fun number_in_months (lst : (int*int*int) list, months : int list) =
	let
		fun number_in_months_with_counter (lst : (int*int*int) list, months : int list, counter) =
			if null months
			then counter
			else number_in_months_with_counter(lst, tl months, counter + number_in_month(lst, hd months))
	in
		number_in_months_with_counter(lst, months, 0)
	end;


fun dates_in_month (lst : (int*int*int) list, month : int) =
	let 
		fun month_in_list (date: int*int*int, month : int) = (#2 date = month);
		fun new_list (lst : (int*int*int) list, month : int, result: (int * int * int) list) =
			if null lst
			then result
			else let
				val new_result = if month_in_list(hd(lst) : int*int*int, month : int) then hd(lst)::result else result;
			in
				new_list(tl lst, month, new_result)
			end;
	in
		new_list(lst, month, [])
	end

fun dates_in_months (lst : (int*int*int) list, months : int list) =
	let
		fun dates_in_months_with_list (lst : (int*int*int) list, months : int list, counter: (int*int*int) list) =
			if null months
			then counter
			else 
				let
					val new_counter = hd(dates_in_month(lst, hd months))::counter
				in
					dates_in_months_with_list(lst, tl months, new_counter)
				end;
	in
		dates_in_months_with_list(lst, months, [])
	end;


fun get_nth (strings: string list, position : int) =
	if position = 1
	then hd strings
	else get_nth(tl strings, position -1)

fun date_to_string (date : int*int*int) =
	let
		val months = ["January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"];
	in
		get_nth(months, #2 date)^" "^(Int.toString(#3 date))^", "^(Int.toString(#1 date))
	end

fun number_before_reaching_sum (upper_threshold : int, initial_list : int list) =
	let
		fun sum_iterator (upper_threshold : int, sum : int, lst : int list, previous_number : int) =
			if hd(lst) + sum >= upper_threshold
			then previous_number
			else sum_iterator (upper_threshold, hd(lst) + sum, tl(lst), hd(lst))
	in
		sum_iterator(upper_threshold, 0,initial_list, 0)
	end

fun what_month (days: int) =
	let
		val days_in_months = [31, 28, 31, 30, 31, 30, 31, 30, 31, 30, 31, 30, 31];
		fun month_counter(days : int, months : int list, counter : int) =
			if days <= hd months
			then counter
			else month_counter(days - hd months, tl months, counter + 1)
	in
		month_counter(days, days_in_months, 1)
	end;

fun month_range (day1: int, day2: int) =
	let
		fun month_range_with_list (day1: int, day2: int, days: int list) =
			let
				val day_updates = what_month(day1)::days;
			in
				if day1 = day2
				then day_updates
				else month_range_with_list((day1 + 1), day2, day_updates)
			end
	in
		month_range_with_list(day1,day2,[])
	end;

fun oldest(lst : (int*int*int) list) =
	let
		fun find_oldest(lst : (int*int*int) list, oldest: int*int*int) =
			if null lst
			then SOME oldest
			else
				let
					val new_oldest = (if is_older(hd lst, oldest) then hd lst else oldest)
				in
					find_oldest(tl lst, new_oldest)
				end

	in
		if null lst
		then NONE
		else find_oldest(lst, hd lst)
	end

(* =================================================================================
	Tests  *)

val test0 = return_last([3,4,5]) = 5;

val test1 = is_older ((1,2,3),(2,3,4)) = true;

val test2 = number_in_month ([(2012,2,28),(2013,12,1)],2) = 1;

val test3 = number_in_months ([(2012,2,28),(2013,12,1),(2011,3,31),(2011,4,28)],[2,3,4]) = 3;

val test4 = dates_in_month ([(2012,2,28),(2013,12,1)],2) = [(2012,2,28)]

val test5 = dates_in_months ([(2012,2,28),(2013,12,1),(2011,3,31),(2011,4,28)],[2,3,4]) = [(2012,2,28),(2011,3,31),(2011,4,28)]

val test6 = get_nth (["hi", "there", "how", "are", "you"], 2) = "there"

val test7 = date_to_string (2013, 6, 1) = "June 1, 2013";

val test8 = number_before_reaching_sum (10, [1,2,3,4,5]) = 3;

val test9 = what_month 70 = 3

val test10 = month_range (31, 34) = [1,2,2,2]

val test11 = oldest([(2012,2,28),(2011,3,31),(2011,4,28)]) = SOME (2011,3,31)
