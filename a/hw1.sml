(* Q1. fn : is_older (int * int * int, int * int * int) -> boolean
       This function takes two arguments date1 and date2, it
       returns true if date1 comes before date2, otherwise returns false.
*)
fun is_older (date1 : int * int * int, date2 : int * int * int) =
    if (#1 date1) <> (#1 date2)
    then (#1 date1) < (#1 date2)
    else
        if (#2 date1) <> (#2 date2)
        then (#2 date1) < (#2 date2)
        else (#3 date1) < (#3 date2)

(* Q2. fn : number_in_month ((int * int * int) list, int) -> int
       This function takes two arguments dates and month, it
       returns how many dates in dates are in the month.
*)
fun number_in_month (dates : (int * int * int) list, month : int) =
    if null dates
    then 0
    else if #2 (hd dates) = month
    then 1 + number_in_month(tl dates, month)
    else number_in_month(tl dates, month)

(* Q3. fn : number_in_months ((int * int * int) list, int) -> int
       This function takes two arguments dates and months, it
       returns how many dates in dates are in the months.
*)
fun number_in_months (dates : (int * int * int) list, months : int list) =
    if null months
    then 0
    else number_in_month(dates, hd months) + number_in_months(dates, tl months)

(* Q4. fn : dates_in_month ((int * int * int) list, int) -> (int * int * int) list
       This function takes two arguments dates and month, it returns the list of 
       dates that are in the month.
*)
fun dates_in_month (dates : (int * int * int) list, month : int) =
    if null dates
    then []
    else if #2 (hd dates) = month
    then (hd dates) :: dates_in_month(tl dates, month)
    else dates_in_month(tl dates, month)

(* Q5. fn : dates_in_months ((int * int * int) list, int list) -> (int * int * int) list
       This function takes two arguments dates and months, it returns the list of dates 
       that are in the months. (@ is the append operator.)
*)
fun dates_in_months (dates : (int * int * int) list, months : int list) =
    if null months
    then []
    else dates_in_month(dates, hd months) @
         dates_in_months(dates, tl months)

(* Q6. fn : get_nth (string list, int) -> string
       This function takes two arguments strings and n, 
       it returns nth element of the strings.
*)
fun get_nth (strings : string list, n : int) =
    if n = 1
    then hd strings
    else get_nth(tl strings, n-1)

(* Q7. fn : date_to_string (int * int * int) -> string
       This function takes a argument date, it 
       converts the date into string and returns.
*)
fun date_to_string (date : int * int * int) =
    let
        val months = ["January", "Febuary", "March", "April", "May", "June", "July",
                      "August", "September", "October", "November", "December"]
    in
        get_nth(months, #2 date) ^ " " ^ Int.toString(#3 date) ^ ", " ^
        Int.toString(#1 date)
    end

(* Q8. fn : number_before_reaching_sum (int, int list) -> int
       This function takes arguments sum, positive integers,
       it returns the n which indicates first n number add to
       less than sum.
*)
fun number_before_reaching_sum (sum : int, ints : int list) =
    if sum <= 0
    then ~1
    else 1 + number_before_reaching_sum(sum - (hd ints), tl ints)

(* Q9. fn : what_month (int) -> int
       This function takes a argument 
       day_n, it returns what month
       that day is in.
*)
fun what_month (day_n : int) =
    let
        val months_day = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
    in
        number_before_reaching_sum(day_n, months_day) + 1
    end

(* Q10. fn : month_range (int, int) -> int
        This function takes two arguments
        day1 and day2, it returns the list
        of the months between day1 and day2.
*)
fun month_range (day1 : int, day2 : int) =
    if day1 = day2
    then [what_month(day2)]
    else what_month(day1) :: month_range(day1+1, day2)

(* Q11. fn : oldest ((int * int * int) list) -> (int * int * int) option
        This function takes a argument dates, it returns the oldest date
        in the list or NONE if the list is empty.
*)
fun oldest (dates : (int * int * int) list) =
    if null dates
    then NONE
    else 
        let
            fun nonempty_oldest (dates : (int * int * int) list) =
            if null (tl dates)
            then hd dates
            else 
                let
                    val oldest_date = nonempty_oldest(tl dates)
                in
                    if is_older(hd dates, oldest_date)
                    then hd dates
                    else oldest_date
                end
        in
            SOME(nonempty_oldest(dates))
        end
