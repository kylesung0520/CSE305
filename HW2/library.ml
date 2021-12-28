let welcome = "Welcome to the Oakland, California Municipal Library (OCaML)"

(* These types are defined for you. You should not modify them *)
type catalog_item = Book of string * string * string | Movie of string * int * string
                  | CD of string * string * string | Computer
type checkout_entry = Item of catalog_item | New of checkout_entry | Extend of checkout_entry
                    | Pair of checkout_entry * checkout_entry
type cart = CartEntry of checkout_entry * int option * cart | Empty

(* Examples *)
(* These are some examples of checkout_item. You should test locally with these before submitting *)
let i0 = Book ("Types and Programming Languages", "Benjamin Pierce", "The MIT Press")
let i1 = Movie ("The Imitation Game", 2014, "Morten Tyldum")
let i2 = Computer

(* These are some examples of checkout_entry. You should test locally with these before submitting *)
let e0 = Item i0
let e1 = Item i1
let e2 = Item i2

let e3 = Item (CD ("Songs to Test By", "Aperture Science Psychoacoustic Laboratories", "73:39"))
let e4 = New (Item (Book ("UNIX: A History and a Memoir", "Brian W. Kernighan", "Independently published")))

let e5 = Pair (
    Item (Movie ("WarGames", 1983, "John Badham")),
    Item (Movie ("Sneakers", 1992, "Phil Alden Robinson"))
)

let e6 = Pair (
    Pair (
        Item (Book ("The Unix Programming Environment", "Brian W. Kernighan and Rob Pike", "Prentice-Hall")),
        New (Item (Book ("The C Programming Language", "Brian Kernighan and Dennis Ritchie", "Pearson")))
    ),
    Extend (Item (Book ("The AWK Programming Language", "Alfred V. Aho, Brian W. Kernighan, and Peter J. Weinberger",
                        "Pearson")))
)

(* This is an exmaple of a cart. You should test locally with it before submitting *)
let checked_out = CartEntry (e1, Some 2,
                     CartEntry (e2, None,
                       CartEntry (e4, Some 1,
                         CartEntry (e5, Some 2, Empty))))

(* The following functions you must implement *)

(* Display item as string *)
let string_of_item (i : catalog_item) : string =
    match i with
    Book(title, author, press) -> let a = String.concat " by " [title; author] in
                                  let b = String.concat press ["("; ")"] in
                                  String.concat " " [a; b]
    | Movie(title, year, director) -> let a = String.concat " (" [title; string_of_int year] in
                                      String.concat ") by " [a; director]
    | CD(album, artist, length) -> String.concat " by " [album; artist]
    | Computer -> "Public Computer"

(* Display entry as string *)
let rec string_of_entry (e : checkout_entry) : string =
    match e with
    Item(catalog_item) -> string_of_item(catalog_item)
    |New(ce) -> let a = string_of_entry(ce) in
                String.concat " " ["(NEW)"; a]
    |Extend(ce) -> let a = string_of_entry(ce) in
                String.concat " " ["(EXT)"; a]
    |Pair(ce1, ce2) -> let a = string_of_entry(ce1) in
                       let b = string_of_entry(ce2) in
                       String.concat " and " [a; b]

(* Return the daily fine for an overdue item *)
let rec daily_fine (entry: checkout_entry) : float =
    match entry with
    Item(catalog_item) -> (match catalog_item with
                         Book(title, author, press) -> 0.25
                         | Movie(title, year, director) -> 0.50
                         | CD(album, artist, length) -> 0.50
                         | Computer -> 0.00)
    |New(ce) -> let a = daily_fine(ce) in
                        a *. 2.0
    |Extend(ce) -> let a = daily_fine(ce) in
                           a *. 3.0
    |Pair(ce1, ce2) -> let a = daily_fine(ce1) in
                       let b = daily_fine(ce2) in
                       a +. b

(* Given a list of items and days overdue, compute the total fine *)
let rec total_fine (l : cart) : float =
    match l with
    Empty -> 0.0
    |CartEntry(ce, so, cart) -> match so with
                                Some a -> total_fine(cart) +. daily_fine(ce) *. float_of_int a
                                |None -> total_fine(cart)