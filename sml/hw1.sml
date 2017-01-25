(* homework 1 of programming languages part a *)

(* in this homework dates are represented as a 3-tuple of int (yy, mm, dd) *)

(* is the 1st of 2 dates earlier than the 2nd?  *) 
fun is_older (dt1: (int * int * int), dt2: (int * int * int))  =
   let fun max(j: int, k: int) = 
         if j < k then ~1 else if k < j then 1 else 0
       val yr=max(#1 dt1, #1 dt2) * 4
       val mo=max(#2 dt1, #2 dt2) * 2
       val da=max(#3 dt1, #3 dt2)
       val rslt = yr + mo + da
   in 
       rslt < 0          
   end

(* given a list of dates and a month - how many dates have that month? *)
fun number_in_month (dts : (int * int * int) list, month: int) =
  let fun cnt(ds: (int * int * int) list, j: int) =
        if null ds then j
        else if month = #2 (hd ds) then cnt(tl ds, j + 1)
        else cnt(tl ds, j)
  in
      cnt(dts, 0)
  end


(* 3. Write a function number_in_months that takes a list of dates and
a list of months (i.e., an int list) and returns the number of dates
in the list of dates that are in any of the months in the list of
months.  Assume the list of months has no number repeated. Hint: Use
your answer to the previous problem *)

fun number_in_months(dts : (int * int * int) list, months: int list) =
  let fun cnt(mon: int list, j) =
        if null mon then j
        else 
           cnt(tl mon, number_in_month(dts, hd mon) + j)
  in
     cnt(months, 0)   
  end


(* 4. Write a function dates_in_month that takes a list of dates and a
month (i.e., an int) and returns a list holding the dates from the
argument list of dates that are in the month. The returned list should
contain dates in the order they were originally given. *)

fun dates_in_month(dts: (int * int * int) list, month: int) =
  let fun dt_list(ds: (int * int * int) list, ret: (int * int * int) list) =
     if null ds then ret
     else if #2 (hd ds) = month then
        dt_list(tl ds, ret @ [hd ds])
     else
        dt_list(tl ds, ret)
  in
      dt_list(dts, [])
  end
  
(* 5. Write a function dates_in_months that takes a list of dates and
a list of months (i.e., an int list) and returns a list holding the
dates from the argument list of dates that are in any of the months in
the list of months. Assume the list of months has no number
repeated. Hint: Use your answer to the previous problem and SML’s
list-append operator (@). *)

fun dates_in_months(dts: (int * int * int) list, months: int list) =
   let fun dt_list(mos: int list, ret: (int * int * int) list ) =
         if null mos orelse null dts then ret
         else  dt_list(tl  mos, ret @ dates_in_month(dts, hd mos))
   in 
       dt_list(months, [])
   end

(* 6. Write a function get_nth that takes a list of strings and an int
n and returns the n th element of the list where the head of the list
is 1st. Do not worry about the case where the list has too few
elements: your function may apply hd or tl to the empty list in this
case, which is okay*)
fun get_nth(strings: string list, n: int) =
  let fun checkit(strs: string list, count: int) =
        if null strs then ""
        else  if count = n then hd strs
        else checkit(tl strs, count + 1)
  in
      checkit(strings, 1)
  end


(* 7. Write a function date_to_string that takes a date and returns a
string of the form January 20, 2013 (for example). Use the operator ^
for concatenating strings and the library function Int.toString for
converting an int to a string. For producing the month part, do not
use a bunch of conditionals.  Instead, use a list holding 12 strings
and your answer to the previous problem. For consistency, put a comma
following the day and use capitalized English month names: January,
February, March, April, May, June, July, August, September, October,
November, December. *)

fun date_to_string(dt: (int * int * int)) =
  let val months = ["January","February","March","April","May", 
                    "June","July","August","September",
                    "October","November","December"]
  in
      get_nth(months, #2 dt) ^ " " ^ Int.toString(#3 dt) ^ ", "
           ^ Int.toString(#1 dt)
  end


(* 8. Write a function number_before_reaching_sum that takes an int
called sum, which you can assume is positive, and an int list, which
you can assume contains all positive numbers, and returns an int.  You
should return an int n such that the first n elements of the list add
to less than sum, but the first n + 1 elements of the list add to sum
or more. Assume the entire list sums to more than the passed in value;
it is okay for an exception to occur if this is not the case. *)

fun number_before_reaching_sum(sum: int, intlist: int list) =
  let fun sumit(ilst: int list, ndx: int, total: int) =
     let val newtotal= total+hd ilst
     in
        if newtotal  >= sum then ndx
        else sumit(tl ilst, ndx + 1, newtotal)
     end
  in
      sumit(intlist, 0, 0)
  end

(* 9. Write a function what_month that takes a day of year (i.e., an
int between 1 and 365) and returns what month that day is in (1 for
January, 2 for February, etc.). Use a list holding 12 integers and
your answer to the previous problem. *)

fun what_month(day: int) =
    number_before_reaching_sum(day,[31,28,31,30,31,30,31,31,30,31,30,31] ) + 1
  
(* 10. Write a function month_range that takes two days of the year
day1 and day2 and returns an int list [m1,m2,...,mn] where m1 is the
month of day1, m2 is the month of day1+1, ..., and mn is the month of
day day2. Note the result will have length day2 - day1 + 1 or length 0
if day1>day2. *)

fun month_range(day1: int, day2: int) =
  if day1 > day2 orelse day1 < 1 orelse day2 > 365 then []
  else
      let fun make_mos(this_day: int, months: int list) =
            let val newmonths = months @ [what_month(this_day)]
            in
                if this_day=day2 then newmonths
                else make_mos(this_day + 1, newmonths)
            end
      in
          make_mos(day1, [])
      end;

(* 11. Write a function oldest that takes a list of dates and
evaluates to an (int*int*int) option. It evaluates to NONE if the list
has no dates and SOME d if the date d is the oldest date in the
list. *)

fun oldest(dts: (int * int * int) list) =
  if null dts then NONE
  else let
      fun z(ds: (int * int * int) list, best: (int * int * int)) =
        if null ds then best
        else if is_older(hd ds, best) then z(tl ds, hd ds)
             else z(tl ds, best)
  in
      SOME(z(tl dts, hd dts))
  end
