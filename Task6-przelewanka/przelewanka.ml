let przelewanka (arr: (int * int) array)=
    match arr with
    |[||] -> 0
    | _ ->
    let flag_wrong = ref false (* tej flagi uzyje do sprawdzenia czy y1,y2,..,yn dziela sie wszystkie przez NWD(x1,x2,..,x3)*)
                               (* musi tak byc poniewaz w wyniku operacji z zadania podzielnosc przez to nwd sie nie zmieia*)
    and flag_wrong2 = ref false (* tej flagi uzyje do sprawdzenia czy chos jeden jest pusty lub pelny
                                   musi tab byc poniewaz po operacjach z zadania zawsze jeden jest pusty lub pelny*)
    and n = Array.length arr
    and res = ref (-1)
    in  let rec nwd a b =
            if a * b = 0 then
                (a + b)
            else
                nwd b (a mod b)
        and d = ref (fst arr.(0))
        in  Array.iter (fun x -> d := nwd (fst x) !d) arr;
        Array.iter (fun x -> if ( snd x<> 0 ) then if(snd x mod !d <> 0) then flag_wrong := true) arr; (*sprawdza warunek z 5*)
        Array.iter (fun x -> if( snd x = 0 || snd x = fst x) then flag_wrong2 := true) arr; (*sprawdza warunek z 7*)
        if (not (!flag_wrong)) && !flag_wrong2 then begin (*optymalizacja - wykonujemy dalej tylko jesli obie flagi nie weszly*)
            let q = Queue.create () (*kolejka do przechowywania elementow*)
            in  Queue.add (Array.make n 0) q; (* dodajemy stan poczatkowy same 0*)
            let hash = Hashtbl.create n (*tablica hashujaca jako slownik*)
            in  let add s t= (*pomocnicza funkcja dodajemy tylko gdy s czyli stanu nie w slowniku*)
                    if !res = -1 && not (Hashtbl.mem hash s) then
                    (
                        let found = ref true in
                        for i = 0 to (n-1) do
                            if s.(i) != snd arr.(i) then
                                found := false;
                        done;
                        if !found then
                        (
                            Queue.clear q;
                            res := t + 1;
                        )
                        else
                        (
                            Hashtbl.add hash (Array.copy s) (t + 1);
                            Queue.add (Array.copy s) q;
                        )
                    )
                in  add (Array.make n 0 ) (-1); (* dodajemy stan poczatkowy do slownika,same 0*)
            while not (Queue.is_empty q) do
                let s = Queue.pop q
                in  let t = Hashtbl.find hash s
                    in  for i = 0 to (n-1) do
                            let prev = s.(i) in
                            s.(i) <- fst arr.(i); (*sprawdzamy operacje napelniania do pelna*)
                            add s t;
                            s.(i) <- 0; (*sprawdzamy operacje wylewania*)
                            add s t;
                            s.(i) <- prev;
                            for j = 0 to (n-1) do (*sprawdzamy operacje przelewania ze szkanki do szklanki*)
                                if i <> j then (
                                let prevj = s.(j) in
                                let diff = min (fst arr.(j) - s.(j)) s.(i) in
                                s.(j) <- s.(j) + diff;
                                s.(i) <- s.(i) - diff;
                                add s t;
                                s.(i) <- prev;
                                s.(j) <- prevj;
                                )
                            done
                        done
            done
        end;
        !res;;




