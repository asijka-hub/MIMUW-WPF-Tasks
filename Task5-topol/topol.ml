(*Andrzej Sijka gr.1*)
(*Sortowanie topologiczne*)
(*code review - Piotr Kamiński gr.1*)

exception Cykliczne;;

(*typ rezprezentujacy w jakim stanie jest obecnie wierzcholek
Marked - odwiedzony i przetworzony
Unmarked - nieodwiedzony
Temporary - odwiedzony i nieprzetworzony calkowicie*)
type mark = Marked |  Unmarked |  Temporary;;

let topol list=
    (*funkcja tworzaca mape przypisujaca dla danego wierzcholka jego lista sasiedztwa *)
    let make l =
        let f acc (a,alist)=
            if(PMap.mem a acc = false) then
                PMap.add a (alist) acc
            else
                PMap.add a (alist@( (PMap.find a acc))) acc
        in  List.fold_left f (PMap.create compare) l
    in  let graph = ref (make list) (*tworzymy mape dla wejsciowej listy*)
        and res = ref [] (*pusta lista wynikowa, w niej umiescimy elementy posortowane topologicznie*)
        and mark_map =  ref PMap.empty (*mapa reprezentujaca w jakim stanie jest wierzcholek np. Marked *)
        and mark = ref Unmarked
        and cyclic_flag = ref false (*flaga w ktorej bedziemy pamietac czy w grafie jest cykl*)

        (*funkcja przechodzaca caly graf, wywoluje sie dla danego wiercholka i ustawia go na Temporary
          nastepnie wywoluje sie dla jego listy sasiedztwa (jesli istnieje) jesli wewnatrz kolejnych przejsc
          trafila na wiercholek ktory juz jest Temporary zapamietujemy ze byl cykl, w przeciwnym razie ustawiamy
          wierzcholek na Marked i dodajemy go na tablice wynikowa*)
        in  let rec dfs a =
                if(PMap.mem a !mark_map = true) then begin
                    mark := PMap.find a !mark_map;
                    if(!mark = Temporary) then
                        cyclic_flag := true
                    end
                else begin
                    mark_map := PMap.add a Temporary !mark_map;
                    if(PMap.mem a !graph = true) then
                        List.iter dfs (PMap.find a !graph);
                    mark_map := PMap.add a Marked !mark_map;
                    res := a::!res
                    end
            in List.iter (fun (a,_) -> dfs a) list;
            if(!cyclic_flag <> true) then
                !res
            else
                raise Cykliczne;;

