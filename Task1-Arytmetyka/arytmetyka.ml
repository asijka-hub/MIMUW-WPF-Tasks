(* Andrzej Sijka gr. 1 *)
(* Code reviewer - Mikolaj Uzarski gr. 5 *)

(* ---------------------------------------------- *)


(*typ pomocniczny przechowujacy zakres wartosci mieszczacy sie w dwoch floatach*)
type przedzial = float * float;;

(*typ przechowujacy przyblizona wartosc skladajacy sie z dwoch typow przedzial*)
type wartosc = przedzial * przedzial;;


let przedzial (a: float) (b: float) : przedzial= (a,b);; (*konstruktor typu przedzial*)
let wartosc (a: przedzial) (b: przedzial) : wartosc = (a,b);; (*konstruktor typu wartosc*)
let zero = przedzial 0.0 0.0;; (*wartosc przechowujaca same zera*)
let pusty = przedzial nan nan;; (*wartosc pusta*)


(* konstruktor wartosci, jeden przedzial zawiera wartosci podane przez uzytkownika drugi jest zawsze pusty *)
let wartosc_od_do (a: float) (b: float)=
    wartosc (przedzial a b ) pusty;;


(* konstruktor zwracajacy x z dana dokladnascia p*)
let wartosc_dokladnosc (x: float) (p: float)=
    let d = p*.x/.100.
    in wartosc_od_do (x-.d) (x+.d);;

(* konstruktor tworzaca przedzial [x,x] dla x *)
let wartosc_dokladna (x: float) = wartosc_od_do x x;;

(* funkcja pomocnicza sprawdza czy przedzial jest pusty*)
let czy_puste (a: float)=
    compare a nan=0;;

(*let czy_puste (a: float)= // TO DO*)
(*    if (a==nan)*)
(*    then*)
(*        true*)
(*    else*)
(*        false;;*)

(* funkcja pomocnicza sprawdza czy wartosc jest pusta*)
let czy_pusty_przedzial ((a,b): przedzial)=
    if (czy_puste a || czy_puste b )
        then
            true
    else
        false;;


(* funkcja pomocnicza zwraca mniejsza z dwoch liczb lub nan jesli OBIE sa nan*)
let min ((a,b): przedzial)=
    if (czy_puste a)
        then
            b
    else if (czy_puste b)
        then
            a
    else if a>b
        then
            b
    else
            a

let min_wartosc ((a,b): wartosc) =
    min(min(a),  min(b));;

(* funkcja pomocnicza zwraca wieksza z dwoch liczb lub nan jesli OBIE sa nan*)
let max ((a,b): przedzial)=
    if czy_puste a
        then
            b
    else if czy_puste b
        then
            a
    else if a>b
        then
            a
    else
            b

let max_wartosc ((a,b): wartosc) =
    max( max(a), max(b));;

(* funkcja pomocnicza sprawdza czy liczba jest w przedzial*)
let in_przedzial ((a,b):przedzial) (c: float)=
    if( not (czy_pusty_przedzial(a,b))&& c>=a && c<=b)
    then
        true
    else
        false;;

(* funkcja pomocnicza sprawdza czy liczba jest w wartosci*)
let in_wartosc ((a,b): wartosc) (y: float) =
    if( in_przedzial a y || in_przedzial b y)
        then
            true
    else
        false;;


let sr_wartosc (x: wartosc) =
    if( (max_wartosc x=infinity || max_wartosc x=neg_infinity)&&(min_wartosc x=infinity || min_wartosc x=neg_infinity))
        then
            nan
    else ((min_wartosc x +. max_wartosc x)/.2.);;

(* funkcja pomocniczna laczy wartosc z przedzialem
   dziala tak jak suma przedzialow matematycznych
   z ta uwaga ze bedziemy do niej przekazywac jedynie
   poprawne przedzialy i wartosci
   np. dla ([2,4],[6,8])u([9,10] nie zwroci poprawnej wartosci poniewaz jesli
   wartosci jest postaci ([a,b],[c,d]) to jest to postac ([-oo,a],[b,+oo])
*)
let polacz_wartosc_przedzial ( (((a1,b1),(a2,b2)): wartosc) , ((c,d): przedzial) ) =
    if czy_pusty_przedzial(c, d)
        then
            wartosc (a1,b1) (a2,b2)
    else if (in_przedzial (a1,b1) c || in_przedzial (a1,b1) d)
        then
           wartosc (min(a1,c),max(b1,d)) (a2,b2)
    else
        wartosc  (a1,b1) ((min(a2,c)), (max(b2,d)));;


(* funkcja pomocnicza laczy wartosc wartosc*)
let polacz_wartosc_wartosc ( ((a1,b1):wartosc) , ((a2,b2):wartosc)) =
    polacz_wartosc_przedzial  ( (polacz_wartosc_przedzial ( (a1,b1) ,a2 ) ) , b2 );;

(* funkcja pomocnicza laczy wartosc,wartosc,wartosc,wartosc*)
let polacz_w_w_4 ((a: wartosc), (b: wartosc),(c: wartosc),(d: wartosc)) =
     polacz_wartosc_wartosc( (polacz_wartosc_wartosc( (polacz_wartosc_wartosc(a,b)),c )) , d );;

(*funkcja pomocnicza zwraca typ przedzial a w nim maksymalny i minimalny zakres liczb dla dzialania '+' dla dwoch
    dwoch przedzialow*)
let zakres_dodawania((a,b): przedzial) ((c,d): przedzial)=
    przedzial (min_wartosc( ((a +. c),(a +. d)),((b +. c),(b +. d)) ))
              (max_wartosc( ((a +. c),(a +. d)),((b +. c),(b +. d)) ) );;

(* analogicznie dla '-' *)
let zakres_odejmowania((a,b): przedzial) ((c,d): przedzial)=
    przedzial (min_wartosc( ((a -. c),(a -. d)),((b -. c),(b -. d)) ))
              (max_wartosc( ((a -. c),(a -. d)),((b -. c),(b -. d)) ) );;

(* analogicznie dla '*' *)
(* oprocz tego osobno rozpatrujemy przypadek gdy jeden z przedzialow jest rowny (0.,0.)
   w przecinych wypadku otrzymamy nieporzadane (-0.0,-0.0) *)
let zakres_mnozenia_podstawowy ((a, b) : przedzial) ((c, d) : przedzial)=
    if( ( przedzial a b) = zero && c<0. && d<0. )
            then zero
        else if( ( przedzial c d) = zero && a<0. && b<0. )
            then zero
    else przedzial (min_wartosc( ((a *. c),(a *. d)),((b *. c),(b *. d)) ))
              (max_wartosc( ((a *. c),(a *. d)),((b *. c),(b *. d)) ) );;

(* analogicznie tylko '/'
 oprocz tego osobno rozpatrujemy przypadek gdy jeden z przedzialow jest rowny (0.,0.) a drugi
   ma dwie wartosci ujemne w przecinych wypadku otrzymamy nieporzadane (-0.0,-0.0) *)
let zakres_dzielenia_podstawowy ((a, b) : przedzial) ((c, d) : przedzial)=
    if( ( przedzial a b) = zero && c<0. && d<0. )
        then zero
    else if( ( przedzial c d) = zero && a<0. && b<0. )
        then zero
    else przedzial (min_wartosc( ((a /. c),(a /. d)),((b /. c),(b /. d)) ))
              (max_wartosc( ((a /. c),(a /. d)),((b /. c),(b /. d)) ) );;

(* zwraca typ wartosc utworzony z danego przedzialu*)
let podziel_przedzial ((a, b) : przedzial) =
    if (a, b) = zero
        then
            wartosc zero zero
    else if a < 0.0 && b > 0.0
        then
            wartosc (przedzial a (-0.0)) (przedzial 0.0 b)
    else if a < 0.0 && b = 0.0
        then
            wartosc (przedzial a (-0.0)) pusty
    else
        wartosc (przedzial a b) pusty;;

(* zwraca typ wartosc dla mnozenia dwoch przedzialow [a,b] [c,d]
    poprzez polacznie wynikow zakres_mnozenia_podstawowy czyli WARTOSCI! (a*c),(a*d),(b*c),(b*d) *)
let zakres_mnozenia ((a1, b1) : przedzial) ((a2, b2) : przedzial) =
        let (c1, d1) = podziel_przedzial (a1, b1) and (c2, d2) = podziel_przedzial (a2, b2)
        in
            polacz_wartosc_wartosc  ( (zakres_mnozenia_podstawowy c1 c2,zakres_mnozenia_podstawowy c1 d2) ,
                                      (zakres_mnozenia_podstawowy d1 c2,zakres_mnozenia_podstawowy d1 d2) ) ;;

(* analogicznie jak dla mnozenia tylko rozpatrujemy dodatkowo dzielenie przez 0 *)
let zakres_dzielenia ((a1, b1) : przedzial) ((a2, b2) : przedzial) =
    if  (a2, b2) = zero
        then wartosc pusty pusty
    else
        let (c1, d1) = podziel_przedzial (a1, b1) and (c2, d2) = podziel_przedzial (a2, b2)
        in
            polacz_wartosc_wartosc  ( (zakres_dzielenia_podstawowy c1 c2,zakres_dzielenia_podstawowy c1 d2) ,
                                      (zakres_dzielenia_podstawowy d1 c2,zakres_dzielenia_podstawowy d1 d2) );;

(* zwraca typ wartosc dla dzialania dodawania dwoch wartosci postaci [a,b] [c,d]
    utorzone przez polaczenie  wynikow zakres_dodawania czyli PRZEDZIALOW! (a+b),(a+c),(b+c),(b+c)*)
let plus ((a,b): wartosc ) ((c,d): wartosc ) =
    polacz_wartosc_wartosc  ( (zakres_dodawania a c,zakres_dodawania a d) , (zakres_dodawania b c,zakres_dodawania b d) );;


(* analogicznie jak plus tylko z funkcjami dzialania '-'
    w tym miejscu warto zaznaczyc ze zamiast osobnej funkcji dla odejmowania
    mozna by uzyc kombinacji dodwania i mnozenia przez -1 jednak bylo by to mniej wydajne*)
let minus ((a,b): wartosc ) ((c,d): wartosc ) =
    polacz_wartosc_wartosc ( ((zakres_odejmowania a c),(zakres_odejmowania a d)) , ((zakres_odejmowania b c),(zakres_odejmowania b d)) );;


(* zwraca typ wartosc dla dzialania mnozenia dwoch wartosci postaci [a,b] [c,d]
    utorzone przez polaczenie  wynikow zakres_mnozenia czyli WARTOSCI! (a+b),(a+c),(b+c),(b+c) *)
let razy ((a,b): wartosc ) ((c,d): wartosc ) =
    polacz_w_w_4 ( (zakres_mnozenia a c) ,(zakres_mnozenia a d), (zakres_mnozenia b c) ,(zakres_mnozenia b d) );;

(* analogicznie jak dla mnozenia tylko z funkcjami dla dzialania '/' *)
let podzielic ((a,b): wartosc ) ((c,d): wartosc ) =
    polacz_w_w_4 ( (zakres_dzielenia a c),(zakres_dzielenia a d), (zakres_dzielenia b c),(zakres_dzielenia b d) );;

(*dzialanie funkcji razy i podzielic moze byc na poczatku trudne do zrozumienia dlatego podaje rozpisany przyklad

  podzielic ((1.,1.) (nan,nan) ((-1.,1.) (nan,nan)) wynik to (-inf,-1) (1,inf)

  wywolujmy dzielenie dla
  (1.,1.) (nan,nan) , (-1.,1.) (nan,nan) ->

  zakres_dzielenia (1.,1.) (-1.,1.)    -> wynik to (-inf,-1) (1,inf) rozpisuje nizej
  zakres_dzielenia (1.1,) (nan,nan)    -> wyjdzie(nan,nan) (nan,nan)
  zakres_dzielenia (nan,nan) (-1.,1.)  -> wyjdzie (nan,nan) (nan,nan)
  zakres_dzielenia (nan,nan) (nan,nan) -> wyjdzie (nan,nan) (nan,nan)


  rozpiszemy tylko zakres_dzielenia (1.,1.) (-1.,1.) pozostale sa podobne i we wszystkich dostajemy (nan,nan) (nan,nan)
  dzielimy te dwa przedzialy funkcja podziel_przedzial
  (1.,1.) (nan,nan) (-1,(-0.)) (0.,1.)

  zakres_dzielenia_podstawowy (1.,1.) (-1.,(-0.))   -> (-inf,-1)
  zakres_dzielenia_podstawowy (1.,1.) (0.,1.)       -> (1,+inf)
  zakres_dzielenia_podstawowy (nan,nan) (-1.,(-0.)) -> (nan,nan)
  zakres_dzielenia_podstawowy (nan,nan) (0.,1.)     -> (nan,nan)

  łączymy te 4 przedzialy czyli wychodzi (-inf,-1) (1,inf) ktore nastepnie samo jest laczone z 3 pozostalymi wynikami*)


