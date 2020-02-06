(* encoding instruction *)
(* 0  pen down *)
(* 1  pen up *)
(* 2  move n *)
(* 3  move e *)
(* 4  move s *)
(* 5  move w *)

type program = int list

let square : program =  [0; 2; 2; 3; 3; 4; 4; 5; 5; 1]
let letter_e : program =  [0; 2; 2; 3; 3; 5; 5; 4; 3; 5;4;3;3;5;5;1]
let rot_e : program = [0; 3; 3; 4; 4; 2; 2; 5; 4; 2; 5; 4; 4; 2; 2; 1]
let concen : program = [0;2; 2; 2; 2; 3; 3; 3; 3; 4; 4; 4; 4; 5; 5; 5; 5; 2; 2; 2; 2; 2; 2; 2; 2; 3; 3; 3; 3; 3; 3; 3; 3; 4; 4; 4; 4; 4; 4; 4; 4; 5; 5; 5; 5; 5; 5; 5; 5; 1]
let square_eq : program = [0; 3; 3; 2; 2; 5; 5; 4; 4; 1]
let mirror_e : program =  [0; 4; 4; 5; 5; 3; 3; 2; 5; 3; 2; 5; 5; 3; 3; 1]
let e_2 : program = [0; 2; 2; 2; 2; 3; 3; 3; 3; 5; 5; 5; 5; 4; 4; 3; 3; 5; 5; 4; 4; 3; 3; 3; 3; 5;5; 5; 5; 1]


let rec min_bounds : (int*int) list -> int*int -> int*int = fun pts acc ->
    match acc with 
    | (x, y) -> 
        (match pts with
        | h::t -> 
            (match h with
            | (hx, hy) -> 
                if hx < x then
                    if hy < y then
                        min_bounds t (hx, hy)
                    else
                        min_bounds t (hx, y)
                else
                    if hy < y then
                        min_bounds t (x, hy)
                    else
                        min_bounds t (x, y)
            | _ -> failwith"nonsense point")
        | [] -> acc)
    | _ -> failwith"bad bound found"
let rec max_bounds : (int*int) list -> int*int -> int*int = fun pts acc ->
    match acc with 
    | (x, y) -> 
        (match pts with
        | h::t -> 
            (match h with
            | (hx, hy) -> 
                if hx > x then
                    if hy > y then
                        max_bounds t (hx, hy)
                    else
                        max_bounds t (hx, y)
                else
                    if hy > y then
                        max_bounds t (x, hy)
                    else
                        max_bounds t (x, y)
            | _ -> failwith"nonsense point")
        | [] -> acc)
    | _ -> failwith"bad bound found"
let rec print_helper : int*int -> int*int -> (int*int) list -> int = fun cur bound points ->
    match cur with
    | (cx,cy) -> 
        (match bound with 
        | (bx,by) -> 
            if cx > bx then
                if cy > by then
                    failwith"unknown case"
                else
                    (print_string"\n";
                    print_helper (-10,cy+1) bound points)
            else
                if cy > by then
                    0
                else
                    (if List.mem (cx, cy) points then
                        (print_string"1";
                        print_helper (cx+1,cy) bound points)
                    else
                        (print_string"0";
                        print_helper (cx+1,cy) bound points))
        | _ -> failwith"invalid bound")
    | _ -> failwith"printing bad point"

let print : (int*int) list -> int = fun points ->
    print_helper (-10,-10) (10,10) points

let print_tuple = fun t ->
    match t with 
    | (x, y) -> 
        Printf.printf "(%d,%d)\n" x y
    | _ -> Printf.printf "\n"
let rec print_tuple_list = fun l ->
    match l with
    | h::t ->
        (print_tuple h;
        print_tuple_list t)
    | [] -> Printf.printf "[]\n"

let rec lac_helper : (int*int) list -> (int*int) list -> (int*int) list = fun list acc ->
    match list with
    | [] -> acc 
    | h::t -> 
        (if not (List.mem h acc) then 
            lac_helper t (h::acc)
        else 
            lac_helper t acc)
let laconic : (int*int) list -> (int*int) list = fun points ->
    List.rev (lac_helper points [])

let rec parse_colored : int*int -> program -> (int*int) list -> bool -> (int*int) list = fun point program acc down ->
    match point with
    | (x, y) ->
        (match program with
            | h::t -> 
                (match h with
                | 0 -> parse_colored point t (acc@[(x,y)]) true
                | 1 -> parse_colored point t acc false
                | 2 -> if down 
                    then parse_colored (x,y+1) t (acc@[(x,y+1)]) true 
                    else parse_colored (x,y+1) t acc false
                | 3 -> if down
                    then parse_colored (x+1,y) t (acc@[(x+1,y)]) true
                    else parse_colored (x+1,y) t acc false
                | 4 -> if down
                    then parse_colored (x,y-1) t (acc@[(x,y-1)]) true
                    else parse_colored (x,y-1) t acc false
                | 5 -> if down
                    then parse_colored (x-1,y) t (acc@[(x-1,y)]) true
                    else parse_colored (x-1,y) t acc false
                | _ -> failwith"invalid program")
            | [] -> acc)
    | _ -> failwith"invalid point"
let colored : int*int -> program -> (int*int) list = fun o p ->
    laconic (parse_colored o p [] false)

let equivalent : program -> program -> bool = fun p1 p2 ->
    ((List.length (List.filter (fun x -> not (List.mem x (colored (0,0) p1))) (colored (0,0) p2))) = 0)
    &&
    ((List.length (List.filter (fun x -> not (List.mem x (colored (0,0) p2))) (colored (0,0) p1))) = 0)

(* encoding instruction *)
(* 0  pen down *)
(* 1  pen up *)
(* 2  move n *)
(* 3  move e *)
(* 4  move s *)
(* 5  move w *)

let mirror : int -> int = fun i -> 
    match i with
    | 0 -> 0
    | 1 -> 1
    | 2 -> 4
    | 3 -> 5
    | 4 -> 2
    | 5 -> 3
    | _ -> failwith"invalid program"

let rotate_90 : int -> int = fun i -> 
    match i with
    | 0 -> 0
    | 1 -> 1
    | 2 -> 3
    | 3 -> 4
    | 4 -> 5
    | 5 -> 2
    | _ -> failwith"invalid program"

let mirror_image : program -> program = fun p -> 
    List.map mirror p

let rotate_image_90 : program -> program = fun p -> 
    List.map rotate_90 p

let rec repeat_helper : int -> 'a -> 'a list -> 'a list = fun n e acc -> 
    if n > 0 then
        repeat_helper (n-1) e (e::acc)
    else
        acc
let repeat : int -> 'a -> 'a list = fun n e -> 
    repeat_helper n e []

let rec pantograph_helper : int -> program -> program -> program = fun n p acc ->
    match p with
    | [] -> acc
    | h::t -> pantograph_helper n t acc@(repeat n h)
let pantograph : int -> program -> program = fun n p -> 
    List.rev (pantograph_helper n p [])

let pantograph_f : int -> program -> program = fun n p -> 
    List.fold_right (fun i j -> (repeat n i) @ j) p []

let pantograph_m : int -> program -> program = fun n p -> 
    failwith"implement"

let rec compress_helper : program -> (int*int) -> (int*int) list -> (int*int) list = fun p cur acc ->
    match cur with 
    | (x, n) -> 
        (if n = 0 then
            (match p with
            | [] -> []
            | h::t -> compress_helper)
        else
            )
    | _ -> failwith"invalid tuple"
let compress : program -> (int*int) list = fun p ->
    compress_helper p []