(* Programming Languages, Dan Grossman *)
(* Section 2: Accumulators *)

fun sum1 xs =
    case xs of
        [] => 0
      | i::xs' => i + sum1 xs'

fun sum2 xs =
    let fun f (xs,acc) =
            case xs of
                [] => acc
              | i::xs' => f(xs',i+acc)
    in
        f(xs,0)
    end

fun rev1 xs =
   case xs of
       [] => []
     | x::xs' => (rev1 xs') @ [x]

fun rev2 xs =
    let fun aux(xs,acc) =
            case xs of
                [] => acc
              | x::xs' => aux(xs', x::acc)
    in
        aux(xs,[])
    end


fun fab1 n =
    if n <= 2 then 1
    else fab1 (n - 2) + fab1 (n - 1)


(* tail recursion, much faster than the previous one when n is getting large ~40 *)
fun fab2 n =
    let
        fun aux(n', acc1, acc2) =
            if n' <= 2 then acc1
            else aux(n' - 1, acc2, acc1 + acc2)
    in
        aux(n, 1, 2)
    end