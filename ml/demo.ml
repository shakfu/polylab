class person name_init =
    object
        val mutable name = name_init
        method get_name = name
        method move d = name <- name + d
    end;;

let l = ["a"; "d"; "b"];;

let rec sort lst =
    match lst with
        [] -> []
    | head :: tail -> insert head (sort tail)
and insert elt lst =
    match lst with
        [] -> [elt]
    | head :: tail -> if elt <= head then elt :: lst else head :: insert elt
    tail
;;

let deriv f dx = function x -> (f(x +. dx) -. f(x)) /. dx;;
let sin' = deriv sin 1e-6;;
let compose f g = function x -> f(g(x));;


type ratio = {num: int; denum: int};;
let add_ratio  r1 r2 = 
    {num = r1.num * r2.denum + r2.num * r1.denum;
     denum = r1.num * r2.denum};;
add_ratio {num=1; denum=3} {num=2; denum=5};;

type number = Int of int | Float of float | Error;;
type sign = Positive | Negative;;

let add_vect v1 v2 =
    let len = min (Array.length v1) (Array.length v2) in
    let res = Array.create len 0.0 in
    for i = 0 to len - 1 do
        res.(i) <- v1.(i) +. v2.(i)
    done;
    res;;

let result = add_vect [| 1.0; 2.0 |] [| 3.0; 4.0 |];;



Printf.printf("hello\n");;

