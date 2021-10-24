(*typ reprezentujacy kolejke priorytetowa jako drzewo lewicowe
lewy syn,prawy syn, wartosc , odleglosc od nulla czyli skrajna prawa wysokosc | Null dla pustej kolejki*)
type 'a queue = Node of 'a queue * 'a queue * 'a * int | Null ;;

(*typ reprezentyjacy pusta kolejke priorytetowa*)
let empty = Null;;

(* zwraca odlegosc od Nulla dla Noda*)
let npl =
    function
        | Null -> 0
        | Node (_,_,_,pw) -> pw;;

(* tworzy kolejke priorytetowa z jednym elementem czyli drzewo z jednym elementem*)
let queue_1_elemenent k  = Node(Null,Null,k,1);;

(* laczy dwie kolejki czyli dwa drzewa lewicowe *)
let rec join d1 d2 =
    match d1,d2 with
        | d1,Null -> d1
        | Null,d2 -> d2
        | Node (l,p,w_1,_) ,Node(_,_,w_2,_) ->
            if (w_1<=w_2) (* porownojemy wartosci chcemy by ta mniejsza czyli maja wiekszy
                               priorytet byla w korzeniu w przeciwnym razie obracamy*)
            then
                let
                    d3 = join p d2 (* laczymy prawego syna i drzewo d2*)
                in
                    let npl_1 = npl l and npl_2 = npl d3
                    in
                        if (npl_1<=npl_2) (*porownojemy prawe wysokosci chcemy by ta wieksza byla po lewej*)
                        then
                            Node(d3,l,w_1,npl_2+1)
                        else
                            Node(l,d3,w_1,npl_1+1)

            else
                join d2 d1;;

(* laczymy kolejke priorytetowa z elementem przez laczenie kolejki z kolejka jednoelementowa*)
let add a q= join q (queue_1_elemenent a);;

(*sprawdza czy kolejka jest pusta*)
let is_empty x = if(x=Null) then true else false;;

exception Empty;;

(* usuwa element minimalny czyli majacy najwiekszy priorytet czyli znajdujacy sie w korzeniu
    i zwraca kolejke powstala przez polacznie prawe i lewego syna*)
let delete_min x=
    match x with
     | Null -> raise Empty
     | Node(l,p,w,_) -> (w,(join l p));;

