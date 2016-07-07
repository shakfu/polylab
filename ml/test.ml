(* File test.ml *)

module Machine = struct
    type server = { make : string; memory: int }
end

let lover =
    object
        val vertices = [ 1;2;3;4 ]
        method draw = List.map (fun x -> x+1) vertices
    end



let add = fun i j -> i + j

let inc i = i + 1

let rec power i x = 
    if i = 0 then
        1.0
    else
        x *. (power (i - 1) x)

let person ~age:i ~height:j = i * j

let employee ~age ~weight = age - weight

let org i =
    match i with 
     | 0 -> 0
     | 1 -> 1
     | j -> j + 1 

let rec fib = function
      0 -> 0
    | 1 -> 1
    | i -> fib (i - 1) + fib (i - 2)

let rec fib2 = function
    (0 | 1) -> 0
       | i -> fib (i - 1) + fib (i - 2)

let rec sum = function
    |      [] -> 0
    | x :: xs -> x + sum xs

let rec map f = function
    |      [] -> []
    | x :: xs -> f x :: map f xs

(*
let main () = 
    let arg = Sys.argv.(1) in
    print_string arg;
    print_newline();
    exit 0;;
main ()
*)


