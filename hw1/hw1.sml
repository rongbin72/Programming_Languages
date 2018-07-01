fun is_older(left : int*int*int, right : int*int*int) =
    if (#1 left < #1 right)
    then true
    else if (#1 left = #1 right) andalso (#2 left < #2 right)
    then true
    else if (#1 left = #1 right) andalso (#2 left = #2 right) andalso (#3 left < #3 right)
    then true
    else false

(* if the date matches, add 1, else add nothing, just keep doing recursive calls *)
fun number_in_month(dates : (int*int*int) list, month : int) =
    if null dates
    then 0
    else if #2 (hd dates) = month
    then 1 + number_in_month(tl dates, month)
    else number_in_month(tl dates, month)

fun number_in_months(dates : (int*int*int) list, months : int list) =
    if null months
    then 0
    else number_in_month(dates, hd months) + number_in_months(dates, tl months)

fun dates_in_month(dates : (int*int*int) list, month : int) =
    if null dates
    then []
    else if #2 (hd dates) = month
    then hd dates :: dates_in_month(tl dates, month)
    else dates_in_month(tl dates, month)

fun dates_in_months(dates : (int*int*int) list, months : int list) = 
    if null months
    then []
    else dates_in_month(dates, hd months) @ dates_in_months(dates, tl months)

fun get_nth(str : string list, n : int) =
    if n = 1
    then hd str
    else get_nth(tl str, n - 1)