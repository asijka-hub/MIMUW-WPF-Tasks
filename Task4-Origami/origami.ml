(*Andrzej Sijka gr.1*)
(*code review - Sylwia Szunejko gr.1*)

type point  = float * float;;

type kartka = point -> int;;

let epsilon = 1e-14;;

(*(*sprawdza po ktorej stronie prostej jest punkt (x,y),*)
(*  zwraca 0 jesli na prostej, 1 jesli po lewej, -1 jesli po prawej*)*)
(*let strona ((a_1,b_1): point) ((a_2,b_2): point) ((x,y): point)=*)
(*    let a = (a_2-.a_1) and b=(b_2-.b_1)*)
(*    in  let det= b*.x-.a*.y-.(a_1*.b-.a*.b_1)*)
(*        in if (det=0.0) then 0*)
(*           else if(det>0.0) then -1*)
(*           else 1;;*)

(* sprawdza po ktorej stronie prostej jest punkt (x,y),
  zwraca 0 jesli na prostej, 1 jesli po lewej, -1 jesli po prawej *)
let strona ((p1, p2) : point) ((q1, q2) : point) ((x, y) : point) =
   let det = (q1 -. p1) *. (y -. q2) -. (q2 -. p2) *. (x -. q1)
   in if det < 0. then -1
      else if det = 0. then 0
      else 1;;

(*liczy obraz punktu (p,q) wzgledem zadanej prostej,
  oblicza wspolczynniki a,b,c w rownaniu prostej a*y+b*x+c=0 i na tej podstawie oblicza wspolrzedne obrazu*)
let obraz ((x_1,y_1): point) ((x_2,y_2): point) ((p,q): point)=
    let a = x_1 -. x_2 and b = y_2-.y_1 and c = (x_2-.x_1)*.y_1-.x_1*.(y_2-.y_1)
    in ( (p*.(a*.a-.b*.b)-.2.0*.b*.(a*.q+.c))/.(a*.a+.b*.b) , (q*.(b*.b-.a*.a)-.2.0*.a*.(b*.p+.c))/.(a*.a+.b*.b) );;


let prostokat ((a_1,b_1): point) ((a_2,b_2): point) =
    (function ((p,q): point) ->  if( p>=a_1 && p<=a_2  && q>=b_1 && q<=b_2 ) then 1 else 0: kartka);;

let kolko ((a_1,b_1): point) (r: float) =
    (function ((p,q): point) -> if ( sqrt((p-.a_1)*.(p-.a_1) +. (q-.b_1)*.(q-.b_1)) <= r ) then 1 else 0: kartka);;

(* zwraca zlozona kartke wzgledem punktow a,b
   dla punktow o wspolrzednych (p,q) ustala po ktorej strone prostej jest punkt
   i na tej podstawie oblicza liczbe warstw w punkcie (p,q)
   (jesli po lewej: to zero, jesli na lini: tyle co przed zlozeniem,
   po prawej: tyle co przed zlozeniem i tyle co w obrazie*)
let zloz (a: point) (b: point) (k: kartka)= (function ((p,q) : point) ->
    let str = strona a b (p,q) and (x,y) = obraz  a b (p,q)
    in if str < 0 then 0
       else if str = 0 then k (p,q)
       else k (p,q) + k (x,y));;

let skladaj l_p (k: kartka)=
    List.fold_left (fun w (p1,p2) -> zloz p1 p2 w) k l_p;;
