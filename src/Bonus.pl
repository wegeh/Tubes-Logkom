winProbabilities(X, Y, Z) :-
    (   X > 6*Y ->
        Z is 100 
    ;   X < 6*Y -> 
        Z is 0
    ;   Z is (X / (X + Y))
    ).

cheatTentara :-
    queue_player([P|_]),
    tentara_idle(P, Awal),
    retract(tentara_idle(P, _)),
    write('Jumlah tentara yang ingin ditambahkan: '),
    read(Tentara_Tambahan), nl,
    write('Tentara berhasil ditambahkan ke tentara tamnbahan'),
    Akhir is Awal + Tentara_Tambahan,
    asserta(tentara_idle(P, Akhir)).

cheatWilayah :-
    queue_player([P|_]),
    write('Masukkan nama wilayah yang ingin diambil: '),
    read(Wilayah), nl,
    info_wilayah(Wilayah, _, J),
    retract(info_wilayah(Wilayah, _, J)),
    asserta(info_wilayah(Wilayah, P, J)),

    findall(X, info_wilayah(X, P, Y), WilayahList),
    length(WilayahList, L),

    (L < 24 -> 
    write('Wilayah berhasil diambil oleh Player '),
    write(P)
    ; menang(P)
    ).

cheatKartu :- 
    queue_player([P|_]),
    write('List kartu :'), nl,
    write('1. Ceasefire Order'), nl,
    write('2. Super Soldier Serum'), nl,
    write('3. Auxiliary Troops'), nl,
    write('4. Disease Outbreak'), nl,
    write('5. Supply Chain Issue'), nl,
    write('6. Rebellion'), nl,
    write('Masukkan angka: '),
    read(Angka), nl,
    (retract(risk_card(_, P)) -> true;true),
    (Angka == 1 ->
        asserta(risk_card(ceasefireorder, P)),
        write('Player '), write(P), write(' mendapatkan risk card CEASEFIRE ORDER'),nl,
        write('Hingga giliran berikutnya, wilayah pemain tidak dapat diserang oleh lawan.')
    ; Angka == 2 ->
        asserta(risk_card(supersoldierserum, P)),
        write('Player '), write(P), write(' mendapatkan risk card SUPER SOLDIER SERUM'),nl,
        write('Hingga giliran berikutnya, semua hasil lemparan dadu saat penyerangan dan pertahanan akan bernilai 6.')
    ; Angka == 3 ->
        asserta(risk_card(auxiliarytroops, P)),
        write('Player '), write(P), write(' mendapatkan risk card AUXILIARY TROOPS'),nl,
        write('Pada giliran berikutnya, Tentara tambahan yang didapatkan pemain akan bernilai 2 kali lipat.')
    ; Angka == 4 ->
        asserta(risk_card(diseaseoutbreak, P)),
        write('Player '), write(P), write(' mendapatkan risk card DISEASE OUTBREAK'),nl,
        write('Hingga giliran berikutnya, semua hasil lemparan dadu saat penyerangan dan pertahanan akan bernilai 1.')
    ; Angka == 5 ->
        asserta(risk_card(supplychainissue, P)),
        write('Player '), write(P), write(' mendapatkan risk card DISEASE OUTBREAK'),nl,
        write('Hingga giliran berikutnya, semua hasil lemparan dadu saat penyerangan dan pertahanan akan bernilai 1.')
    ; Angka == 6 ->
        asserta(risk_card(rebellion, P)),
        write('Player '), write(P), write(' mendapatkan risk card REBELLION'),nl,
        write('Salah satu wilayah acak pemain akan berpindah kekuasaan menjadi milik lawan. '),
        findall(Territory,info_wilayah(Territory,P,N),ListTer),
        length(ListTer,Len),
        Len1 is Len+1,
        random(1,Len1,Resulte),
        getElement(ListTer,Resulte,Element),
        length(T,Lene),
        Lene1 is Lene+1,
        random(1,Lene1,Resultee),
        getElement(T,Resultee,Elemente),
        retract(info_wilayah(Element,P,N)),
        asserta(info_wilayah(Element,Elemente,N)),
        write('Wilayah '), write(Element), write(' sekarang dimiliki oleh '), write(Elemente),
        retract(risk_card(rebellion,P)), !
    ).