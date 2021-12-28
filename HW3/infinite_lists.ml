type 'a str = Cons of 'a * ('a stream) | Nil
and  'a stream = unit -> 'a str

exception Subscript
exception Empty

let head (s :'a stream) : 'a =
  match s () with
    Cons (hd,tl) -> hd
  | Nil -> raise Empty

let tail (s :'a stream) : 'a stream =
  match s () with
    Cons (hd,tl) -> tl
  | Nil -> raise Empty

let null (s : 'a stream) =
  match s () with
    Nil -> true
  | _ -> false

let rec take (n: int) (s: 'a stream) : 'a list =
  match n with
    n when n > 0 -> head s :: take (n - 1) (tail s)
  | 0 -> []
  | _ -> raise Subscript

let rec nth (n: int) (s: 'a stream) : 'a =
  match n with
    n when n > 0 -> nth (n - 1) (tail s)
  | 0 -> head s
  | _ -> raise Subscript

let rec map (f: 'a -> 'b) (s:'a stream) : 'b stream =
  fun () -> Cons (f (head s), map f (tail s))

let rec filter (s: 'a stream) (f: 'a -> bool) : 'a stream =
  if f (head s)
  then fun () -> Cons (head s, filter (tail s) f)
  else filter (tail s) f

let rec sieve (s: int stream) : int stream =
  fun () -> Cons(head s, sieve (filter (tail s) (fun x -> x mod (head s) <> 0)))

let rec fromn (n: int) = fun () -> Cons (n, fromn (n + 1))
let rec fib n m = fun () -> Cons (n, fib m (n+m))

(* implement the streams and functions below *)

let even : int -> bool = fun x -> x mod 2 = 0
let odd  : int -> bool = fun x -> x mod 2 = 1

(*square function // similar with fromn but multiply n itself*)
let rec squareFun (n : int) = fun () -> Cons((n*n), squareFun(n+1))
let rec squares : int stream = squareFun 1

let fibs : int stream = (fib 0 1)
let evenFibs : int stream = (filter fibs even)
let oddFibs : int stream = (filter fibs odd)
let primes : int stream = sieve (fromn(2))

(*function for rev_zip_diff // should follow a s -> b s -> bac -> bac*)
let rec zipFun a b f = fun() -> Cons((head(a), head(b), (f (head(a), head(b)))), (zipFun (tail(a)) (tail(b)) f))

let rev_zip_diff : 'a stream -> 'b stream -> ('b * 'a -> 'c) -> ('b * 'a * 'c) stream =
  fun a b f -> zipFun b a f

let rec printGenList : 'a list -> ('a -> unit) -> unit =
  fun l f -> match l with
             |[] -> ()
             |hd::tl -> f(hd);
                        printGenList tl f

let rec printList : int list -> string -> unit =
  fun l f ->
    let oc = open_out f in
      printGenList l (fun a -> Printf.fprintf oc "%s " (string_of_int a));
    close_out oc

let rec printPairList : (int * int) list -> string -> unit =
  fun l f ->
    let oc = open_out f in
      printGenList l (fun (a,b) -> Printf.fprintf oc "(%s, %s) " (string_of_int a) (string_of_int b));

      close_out oc