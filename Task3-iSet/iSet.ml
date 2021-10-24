(*Andrzej Sijka gr.1*)
(*code review: Joanna Dagil gr. 3*)

(*lewy syn, przedzial (zakladamy a<=b), prawy syn, wysokosc, liczba elementow w drzewie*)
type t=
 | Empty
 | Node of t * (int * int) * t * int * int;;

(*tworzy drzewo puste*)
let empty = Empty;;

(*zwraca wysokosc*)
let height = function
   | Node (_, _, _, h,_) -> h
   | Empty -> 0;;

(*zwraca liczbe elementow w drzewie*)
let b_h = function (*b_help*)
    | Node(_,_,_,_,b) -> b
    | Empty -> 0;;

(*ilosc elementow musi byc dodatnia jesli jest ujemna przekroczyla max_int*)
let max_i a = if a < 0 then
                max_int
              else a;;

(* tworzy drzewo z lewym synem l, prawym synem r, i wartoscia (x,y) *)
let make l (x, y) r = Node (l, (x, y), r, max (height l) (height r) + 1,max_i ((b_h l) + (b_h r) + (y - x + 1)));;

(*sprawdza czy wartosc x jest w przedzial (a,b)*)
let in_i x (a, b) = a <= x && x <= b;;

(* laczy drzewo l, r, oraz przedzial k
   powstale w wyniku drzewo ma roznice wysokosci miedzy podrzewami nie wieksza niz 2
   i jest spelnia warunek drzewa avl*)
let bal l k r =
   let hl = height l
   in let hr = height r
      in  if hl > hr + 2 then
            match l with
            | Node (ll, lk, lr, _, _) ->
                if height ll >= height lr then
                  make ll lk (make lr k r)
                else
                  (match lr with
                  | Node (lrl, lrk, lrr, _, _) ->
                      make (make ll lk lrl ) lrk (make lrr k r)
                  | Empty -> assert false)
            | Empty -> assert false
          else if hr > hl + 2 then
            match r with
            | Node (rl, rk, rr, _, _) ->
                if height rr >= height rl then
                   make (make l k rl) rk rr
                else
                  (match rl with
                  | Node (rll, rlk, rlr, _, _) ->
                      make (make l k rll) rlk (make rlr rk rr)
                  | Empty -> assert false)
            | Empty -> assert false
          else
            Node (l, k, r, max hl hr + 1,b_h l + (snd k-fst k+1) + b_h r);;

(*zwraca najmniejszy element w drzewie
 zlozonosc czasowa: O(log n)*)

let rec min_elt = function
  | Node (Empty, k, _, _,_) -> k
  | Node (l, _, _, _,_) -> min_elt l
  | Empty -> raise Not_found;;

(*zwraca drzewo uzyskane po usunieciu najmniejszego elementu
 zlozonosc czasowa: O(log n)*)
let rec remove_min_elt = function
  | Node (Empty, _, r, _,_) -> r
  | Node (l, k, r, _,_) -> bal (remove_min_elt l) k r
  | Empty -> invalid_arg "PSet.remove_min_elt";;

(*laczy dwa drzewa t1, t2 zakladajac ze
wartosci t1< wartosc t2, i ze oba sa drzewami spelniajacymi warunkek avl
 zlozonosc czasowa: O(log n)*)
let merge t1 t2 =
  match t1, t2 with
  | Empty, _ -> t2
  | _, Empty -> t1
  | _ -> let k = min_elt t2
         in bal t1 k (remove_min_elt t2);;

let a = fun (a,b,c) -> a;;
let b = fun (a,b,c) -> b;;
let c = fun (a,b,c) -> c;;

(*dodaje do drzewa przedzial zakladajac ze jest on rozlaczny z kazdym przedzialem z drzewa
 zlozonosc czasowa: O(log n)*)
let rec a_s (x, y) t = (* add simply*)
  match t with
    | Empty -> make Empty (x, y) Empty
    | Node (l, (a, b), r, _, _) ->
      if x > b then
        let nr = a_s (x, y) r
        in  bal l (a, b) nr
      else
        let nl = a_s (x, y) l
        in  bal nl (a, b) r;;

(*laczy dwa drzewa l i r oraz dodaje do nich przedzial v
zaklada, ze wartosc l<v<wartosc r, oraz upewnia sie by drzewo spelnialo warunki drzewa avl
 zlozonosc czasowa: O(log n)*)
let rec join l v r =
  match (l, r) with
    (Empty, _) -> a_s v r
  | (_, Empty) -> a_s v l
  | (Node(ll, lv, lr, lh, _), Node(rl, rv, rr, rh, _)) ->
      if lh > rh + 2 then
         bal ll lv (join  lr v r)
      else if rh > lh + 2 then
         bal (join  l v rl) rv rr
      else
         make l v r;;

let split x t=
  let rec loop  = function
      Empty ->
        (Empty, false, Empty)
    | Node (l, (a,b) , r, _, _) ->
        if in_i x (a,b) then
            (* (lewy syn (+ przedzial (a, x) jesli istnieje), true, prawy syn (+ przedzial (x, b) jesli istnieje))) *)
            ((if a = x then l else a_s (a, x - 1) l), true, (if b = x then r else a_s (x + 1, b) r))
        else if x < a then
            let (ll, pres, rl) = loop  l
            in (ll, pres, join  rl (a,b) r)
        else
            let (lr, pres, rr) = loop  r
            in (join  l (a,b) lr, pres, rr)
  in loop t;;

(*dzieli drzewa na drzewa jedno z wartosciami mniejszymi niz x,
drugie z wartosciami wiekszymi niz x i je laczy
 zlozonosc czasowa: O(log n)*)
let remove (x,y) t=
    let (l,_,_) = split x t
    in let (_,_,p) =  split y t
       in merge l p;;

(*zwraca pierwszy element przedzialu w drzewie
takiego z x do niego nalezy jesli go nie ma zwraca x+1
 zlozonosc czasowa: O(log n)*)
let rec get_left x t =
 match t with
    | Empty -> x + 1
    | Node(l, (a,b), r , _ , _) -> if x < a then
                                      get_left x l
                                   else if x>b then
                                      get_left x r
                                   else a;;

(*zwraca drugi element przedzialu w drzewie
takiego z x do niego nalezy jesli go nie ma zwraca x-1
 zlozonosc czasowa: O(log n)*)
let rec get_right x t =
 match t with
    | Empty -> x - 1
    | Node(l, (a,b), r , _ , _) -> if x < a then
                                        get_right x l
                                   else if x>b then
                                        get_right x r
                                   else b;;

(* laczy przedzialy nierozlaczne z przedzialem (x,y) i skleja je w jeden przedzial i go usuwa
a nastepnie dodaje ten przedzial juz rozlaczny ze wszystkimi
remove, get_left, get_right oraz a_s maja zlozonosc O(log n) a jako ze wywolujemy je raz to
 zlozonosc czasowa: O(log n)*)
let add (x, y) t =
    let mi = (*merged interval*)
        if x = min_int && y = max_int then
            (x, y)
        else if x = min_int then
            (x, get_right (y + 1) t)
        else if y = max_int then
            (get_left (x - 1) t, y)
        else
            (get_left (x - 1) t, get_right (y + 1) t)
    in a_s mi (remove mi t);;

let is_empty t=
    t = Empty;;

let iter f t=
    let rec loop = function
        | Empty -> ()
        | Node (l, k, r, _, _) -> loop l; f k; loop r
    in loop t;;

let fold f t acc =
    let rec loop acc = function
        | Empty -> acc
        | Node (l, k, r, _, _) ->
            loop (f k (loop acc l)) r
    in loop acc t;;

let elements t =
    let rec loop acc = function
        Empty -> acc
        | Node(l, k, r, _, _) -> loop (k :: loop acc r) l
    in loop [] t;;

let mem x t =
     match (split x t) with
     | (_,boolean,_) -> boolean;;



let below x t =
  let rec aux acc t =
    match t with
      | Empty -> acc
      | Node (l, (a, b), r, _, _) ->
        if b = max_int && a = min_int && x > 0 then
            max_int
        else if x > b then
            aux (acc + b_h l + b - a + 1) r
        else if x < a then
            aux acc l
        else
            aux (acc + x - a + 1) l
  in let fix = aux 0 t
     in if fix < 0 then
            max_int
        else
            fix;;(*Andrzej Sijka gr.1*)
(*code review: Joanna Dagil gr. 3*)

(*lewy syn, przedzial (zakladamy a<=b), prawy syn, wysokosc, liczba elementow w drzewie*)
type t=
 | Empty
 | Node of t * (int * int) * t * int * int;;

(*tworzy drzewo puste*)
let empty = Empty;;

(*zwraca wysokosc*)
let height = function
   | Node (_, _, _, h,_) -> h
   | Empty -> 0;;

(*zwraca liczbe elementow w drzewie*)
let b_h = function (*b_help*)
    | Node(_,_,_,_,b) -> b
    | Empty -> 0;;

(*ilosc elementow musi byc dodatnia jesli jest ujemna przekroczyla max_int*)
let max_i a = if a < 0 then
                max_int
              else a;;

(* tworzy drzewo z lewym synem l, prawym synem r, i wartoscia (x,y) *)
let make l (x, y) r = Node (l, (x, y), r, max (height l) (height r) + 1,max_i ((b_h l) + (b_h r) + (y - x + 1)));;

(*sprawdza czy wartosc x jest w przedzial (a,b)*)
let in_i x (a, b) = a <= x && x <= b;;

(* laczy drzewo l, r, oraz przedzial k
   powstale w wyniku drzewo ma roznice wysokosci miedzy podrzewami nie wieksza niz 2
   i jest spelnia warunek drzewa avl*)
let bal l k r =
   let hl = height l
   in let hr = height r
      in  if hl > hr + 2 then
            match l with
            | Node (ll, lk, lr, _, _) ->
                if height ll >= height lr then
                  make ll lk (make lr k r)
                else
                  (match lr with
                  | Node (lrl, lrk, lrr, _, _) ->
                      make (make ll lk lrl ) lrk (make lrr k r)
                  | Empty -> assert false)
            | Empty -> assert false
          else if hr > hl + 2 then
            match r with
            | Node (rl, rk, rr, _, _) ->
                if height rr >= height rl then
                   make (make l k rl) rk rr
                else
                  (match rl with
                  | Node (rll, rlk, rlr, _, _) ->
                      make (make l k rll) rlk (make rlr rk rr)
                  | Empty -> assert false)
            | Empty -> assert false
          else
            Node (l, k, r, max hl hr + 1,b_h l + (snd k-fst k+1) + b_h r);;

(*zwraca najmniejszy element w drzewie
 zlozonosc czasowa: O(log n)*)

let rec min_elt = function
  | Node (Empty, k, _, _,_) -> k
  | Node (l, _, _, _,_) -> min_elt l
  | Empty -> raise Not_found;;

(*zwraca drzewo uzyskane po usunieciu najmniejszego elementu
 zlozonosc czasowa: O(log n)*)
let rec remove_min_elt = function
  | Node (Empty, _, r, _,_) -> r
  | Node (l, k, r, _,_) -> bal (remove_min_elt l) k r
  | Empty -> invalid_arg "PSet.remove_min_elt";;

(*laczy dwa drzewa t1, t2 zakladajac ze
wartosci t1< wartosc t2, i ze oba sa drzewami spelniajacymi warunkek avl
 zlozonosc czasowa: O(log n)*)
let merge t1 t2 =
  match t1, t2 with
  | Empty, _ -> t2
  | _, Empty -> t1
  | _ -> let k = min_elt t2
         in bal t1 k (remove_min_elt t2);;

let a = fun (a,b,c) -> a;;
let b = fun (a,b,c) -> b;;
let c = fun (a,b,c) -> c;;

(*dodaje do drzewa przedzial zakladajac ze jest on rozlaczny z kazdym przedzialem z drzewa
 zlozonosc czasowa: O(log n)*)
let rec a_s (x, y) t = (* add simply*)
  match t with
    | Empty -> make Empty (x, y) Empty
    | Node (l, (a, b), r, _, _) ->
      if x > b then
        let nr = a_s (x, y) r
        in  bal l (a, b) nr
      else
        let nl = a_s (x, y) l
        in  bal nl (a, b) r;;

(*laczy dwa drzewa l i r oraz dodaje do nich przedzial v
zaklada, ze wartosc l<v<wartosc r, oraz upewnia sie by drzewo spelnialo warunki drzewa avl
 zlozonosc czasowa: O(log n)*)
let rec join l v r =
  match (l, r) with
    (Empty, _) -> a_s v r
  | (_, Empty) -> a_s v l
  | (Node(ll, lv, lr, lh, _), Node(rl, rv, rr, rh, _)) ->
      if lh > rh + 2 then
         bal ll lv (join  lr v r)
      else if rh > lh + 2 then
         bal (join  l v rl) rv rr
      else
         make l v r;;

let split x t=
  let rec loop  = function
      Empty ->
        (Empty, false, Empty)
    | Node (l, (a,b) , r, _, _) ->
        if in_i x (a,b) then
            (* (lewy syn (+ przedzial (a, x) jesli istnieje), true, prawy syn (+ przedzial (x, b) jesli istnieje))) *)
            ((if a = x then l else a_s (a, x - 1) l), true, (if b = x then r else a_s (x + 1, b) r))
        else if x < a then
            let (ll, pres, rl) = loop  l
            in (ll, pres, join  rl (a,b) r)
        else
            let (lr, pres, rr) = loop  r
            in (join  l (a,b) lr, pres, rr)
  in loop t;;

(*dzieli drzewa na drzewa jedno z wartosciami mniejszymi niz x,
drugie z wartosciami wiekszymi niz x i je laczy
 zlozonosc czasowa: O(log n)*)
let remove (x,y) t=
    let (l,_,_) = split x t
    in let (_,_,p) =  split y t
       in merge l p;;

(*zwraca pierwszy element przedzialu w drzewie
takiego z x do niego nalezy jesli go nie ma zwraca x+1
 zlozonosc czasowa: O(log n)*)
let rec get_left x t =
 match t with
    | Empty -> x + 1
    | Node(l, (a,b), r , _ , _) -> if x < a then
                                      get_left x l
                                   else if x>b then
                                      get_left x r
                                   else a;;

(*zwraca drugi element przedzialu w drzewie
takiego z x do niego nalezy jesli go nie ma zwraca x-1
 zlozonosc czasowa: O(log n)*)
let rec get_right x t =
 match t with
    | Empty -> x - 1
    | Node(l, (a,b), r , _ , _) -> if x < a then
                                        get_right x l
                                   else if x>b then
                                        get_right x r
                                   else b;;

(* laczy przedzialy nierozlaczne z przedzialem (x,y) i skleja je w jeden przedzial i go usuwa
a nastepnie dodaje ten przedzial juz rozlaczny ze wszystkimi
remove, get_left, get_right oraz a_s maja zlozonosc O(log n) a jako ze wywolujemy je raz to
 zlozonosc czasowa: O(log n)*)
let add (x, y) t =
    let mi = (*merged interval*)
        if x = min_int && y = max_int then
            (x, y)
        else if x = min_int then
            (x, get_right (y + 1) t)
        else if y = max_int then
            (get_left (x - 1) t, y)
        else
            (get_left (x - 1) t, get_right (y + 1) t)
    in a_s mi (remove mi t);;

let is_empty t=
    t = Empty;;

let iter f t=
    let rec loop = function
        | Empty -> ()
        | Node (l, k, r, _, _) -> loop l; f k; loop r
    in loop t;;

let fold f t acc =
    let rec loop acc = function
        | Empty -> acc
        | Node (l, k, r, _, _) ->
            loop (f k (loop acc l)) r
    in loop acc t;;

let elements t =
    let rec loop acc = function
        Empty -> acc
        | Node(l, k, r, _, _) -> loop (k :: loop acc r) l
    in loop [] t;;

let mem x t =
     match (split x t) with
     | (_,boolean,_) -> boolean;;



let below x t =
  let rec aux acc t =
    match t with
      | Empty -> acc
      | Node (l, (a, b), r, _, _) ->
        if b = max_int && a = min_int && x > 0 then
            max_int
        else if x > b then
            aux (acc + b_h l + b - a + 1) r
        else if x < a then
            aux acc l
        else
            aux (acc + x - a + 1) l
  in let fix = aux 0 t
     in if fix < 0 then
            max_int
        else
            fix;;
