endTurn :-
    game_state(main_game),
    write('Player '),
    queue_player([P|_]),
    write(P),
    write(' mengakhiri giliran.'), nl, nl,
    write('Sekarang giliran Player '),
    changeGiliran,
    queue_player([Y|_]),
    write(Y),
    write('!'), nl,
    write('Player '),
    write(Y),
    write(' mendapatkan '),
    tentara_idle(Y, Awal),
    retract(tentara_idle(Y, _)),
    totalWilayahPemain(Y, L),
    filterBenua(Y, [north_america, europe, asia, south_america, africa, australia], Benua),
    troopsBenua(Benua, 0, J),
    T is floor(L/2),
    JB is J,
    Total is T + JB,
    findall(Card,risk_card(Card,Y),ListCard),
    ((ListCard == [] -> (Total1 is Total);
    (ListCard \== [] ->
    ((search_risk_card(ListCard,auxiliarytroops,Result), 
        Result == true -> Total1 is 2*Total,
        retract(risk_card(_,Y)),
        write('AUXILIARY TROOPS menambah player ini tentara itu 2 kali lebih banyak'),
        nl,!);
    (search_risk_card(ListCard,supplychainissue,Result), 
        Result == true -> Total1 is 0,
        retract(risk_card(_,Y)),
        write('SUPPLY CHAIN ISSUE membuat player ini tidak mendapatkan tambahan tentara'),
        nl,!);
    (Total1 is Total))))),
    Akhir is Awal + Total1,
    asserta(tentara_idle(Y, Akhir)),
    write(Total1),
    write(' tentara tambahan.'), nl,!,
    (findall(Carde,risk_card(Carde,Y),ListCarde)),
    (((search_risk_card(ListCarde,ceasefireorder,Result1), Result1 == true -> 
      write('CEASEFIRE sudah selesai, player ini sudah bisa diserang'),
      retract(risk_card(_,Y))),
      nl, !);true),
    (((search_risk_card(ListCarde,supersoldierserum,Result1), Result1 == true -> 
      write('Giliran anda sudah datang, SUPER SOLDIER SERUM sudah tidak berlaku lagi'),
      retract(risk_card(_,Y))),
      nl, !);true),
    (((search_risk_card(ListCarde,diseaseoutbreak,Result1), Result1 == true -> 
      write('Sekarang sudah giliran anda, DISEASE OUTBREAK sudah tidak berlaku lagi'),
      retract(risk_card(_,Y))),
      nl, !);true),
    retract(risk_allow(_)),
    assertz(risk_allow(0)),
    retract(move_state(_)),
    asserta(move_state(0)),
    retract(attack_state(_)),
    assertz(attack_state(0)),
    displayMap, !.

draft(X, Y) :-
    game_state(main_game),
    queue_player([P|_]),
    tentara_idle(P, Awal),
    kode(X, K, _),
    (info_wilayah(X, P, _) ->
        (Y > Awal ->
        write('Player'),
        write(P),
        write(' meletakkan '),
        write(Y), 
        write(' tentara tambahan di '),
        write(K), nl, nl,
        write('Pasukan tidak mencukupi.'), nl, 
        write('Jumlah Pasukan Tambahan Player '),
        write(P),
        write(': '),
        write(Awal), nl,
        write('draft dibatalkan.'), !;
        write('Player '),
        write(P),
        write(' meletakkan '),
        write(Y), 
        write(' tentara tambahan di '),
        write(K), nl, nl,
        write('Tentara total di '),
        write(K),
        write(': '),
        info_wilayah(X, P, J),
        retract(info_wilayah(X, P, _)),
        J1 is J + Y,
        asserta(info_wilayah(X, P, J1)),
        write(J1), nl,
        write('Jumlah Pasukan Tambahan Player '),
        write(P),
        write(': '),
        retract(tentara_idle(P, _)),
        Akhir is Awal - Y,
        asserta(tentara_idle(P, Akhir)),
        write(Akhir), nl, !);
    write('Player '),
    write(P), 
    write(' tidak memiliki wilayah '),
    write(K), !).

move(X, Y, Z) :-
    game_state(main_game),
    (X = Y ->
        write('Wilayah tidak boleh sama'), nl,
        write('pemindahan dibatalkan.'), nl, nl
        ;
        move_state(State),
        (State is 3 ->
            write('Pemindahan tentara dapat dilakukan maksimum 3 kali per turn.'), nl,
            write('pemindahan dibatalkan.'), nl, nl
        ;
            queue_player([P|_]),
            kode(X, K1, _),
            kode(Y, K2, _),
            (info_wilayah(X, P, XAwal) -> 
                (info_wilayah(Y, P, YAwal) -> 
                    (Z > XAwal ->
                    write(P),
                    write(' memindahkan '),
                    write(Z),
                    write(' tentara dari '),
                    write(K1),
                    write(' ke '),
                    write(K2),
                    write('.'), nl, nl,
                    write('Tentara tidak mencukupi.'), nl, 
                    write('pemindahan dibatalkan.'), !;
                    XAkhir is XAwal - Z,
                    (XAkhir > 0 ->
                        write(P),
                        write(' memindahkan '),
                        write(Z),
                        write(' tentara dari '),
                        write(K1),
                        write(' ke '),
                        write(K2),
                        write('.'), nl, nl,
                        write('Jumlah tentara di '),
                        write(K1),
                        write(': '),
                        retract(info_wilayah(X, P, _)),
                        asserta(info_wilayah(X, P, XAkhir)),
                        write(XAkhir), nl,
                        write('Jumlah tentara di '),
                        write(K2),
                        write(': '),
                        retract(info_wilayah(Y, P, _)),
                        YAkhir is YAwal + Z,
                        asserta(info_wilayah(Y, P, YAkhir)),
                        State1 is State + 1,
                        retract(move_state(State)),
                        asserta(move_state(State1)),
                        write(YAkhir), nl, !;
                        write(P),
                        write(' memindahkan '),
                        write(Z),
                        write(' tentara dari '),
                        write(K1),
                        write(' ke '),
                        write(K2),
                        write('.'), nl, nl,
                        write('Tentara tidak boleh tersisa 0.'), nl, 
                        write('pemindahan dibatalkan.'), !), !);
                write(P),
                write(' tidak memiliki wilayah '),
                write(K2),
                write('.'), nl,
                write('pemindahan dibatalkan.'), nl, !);
            write(P),
            write(' tidak memiliki wilayah '),
            write(K1),
            write('.'), nl,
            write('pemindahan dibatalkan.'), nl, !)
        )
    ).

attack :- 
    game_state(main_game),
    attack_state(AttState), 
    (AttState is 1 -> write('Attack hanya dapat dilakukan satu kali tiap turn pemain.'), nl, !
    ;
    queue_player([P|T]),
    write('Sekarang giliran Player ' ),
    write(P),
    write(' menyerang.'), nl,
    displayMap,
    repeat,
        write('Pilihlah daerah yang ingin Anda mulai untuk melakukan penyerangan: '),
        read(Wilayah), nl,
        (info_wilayah(Wilayah, P, Army) -> true; write('Daerah tidak valid. Silahkan diinput kembali.'), nl, fail),
    write('Player '),
    write(P),
    write(' ingin memulai penyerangan dari daerah '), write(Wilayah), nl,
    tetangga(Wilayah, L), filter(L, L1), length(L1, Pjg),
    (Pjg > 0 -> 
        write('Dalam daerah '), write(Wilayah), write(' Anda memiliki sebanyak '), write(Army), write(' tentara.'), nl,
        (Army == 1 -> write('Jumlah tentara Anda tidak mencukupi untuk melakukan penyerangan, penyerangan Anda gagal. Silahkan melakukan penyerangan lagi.'), nl;
            repeat,
                write('Masukkan banyak tentara yang akan bertempur: '),
                read(NumArmy),
                (NumArmy >= 1, NumArmy < Army -> true; write('Banyak tentara tidak valid. Silahkan input kembali.'), nl, fail),
            write('Player '),
            write(P),
            write(' mengirim sebanyak '),
            write(NumArmy),
            write(' untuk berperang.'), nl,
            displayMap,
            retract(info_wilayah(Wilayah, P, _)),
            CurrentArmy is Army - NumArmy,
            assertz(info_wilayah(Wilayah, P, CurrentArmy)),
            write('Pilihlah daerah yang ingin Anda serang.'), nl,
            printAllNeighbour(Wilayah), nl,
            repeat,
                write('Pilih: '),
                read(Index),
                getElement(L1, Index, WilayahEnemy1),
                info_wilayah(WilayahEnemy1, Enemy1, NumEnemyArmy1),
                findall(Card,risk_card(Card,Enemy1),ListCard),
                ((ListCarde \== [], search_risk_card(ListCard, ceasefireorder, Result), Result == true -> 
                    (queue_player([H|T]), filterCeasefire(T, FilteredT), T == FilteredT -> write('Semua wilayah di sekitar Anda terkena CEASEFIRE ORDER. Silahkan lakukan perintah attack kembali!'), !, fail ;write('Tidak bisa menyerang!'),nl,write('Wilayah ini dalam pengaruh CEASEFIRE ORDER'),nl,fail);true)),
                (((Index >= 1, Index =< Pjg ) -> true; write('Input tidak valid. Silahkan input kembali.'), nl, fail),
            write('Perang telah dimulai.'), nl,
            findall(Card,risk_card(Card,P),ListCarde),
            write('Player '), write(P), nl,
            ((ListCarde \== [], search_risk_card(ListCarde,diseaseoutbreak,Result), Result == true ->
                rollDaduAttackSpecific(1,NumArmy,Total,1),nl,retract(risk_card(diseaseoutbreak,P)));
            (ListCarde \== [], search_risk_card(ListCarde,supersoldierserum,Result), Result == true ->
                rollDaduAttackSpecific(1,NumArmy,Total,6),nl,retract(risk_card(supersoldierserum,P)));
            (((search_risk_card(ListCarde,diseaseoutbreak,Result), Result == false, search_risk_card(ListCarde,supersoldierserum,Result1), Result1 == false);(ListCarde == [])) -> rollDaduAttack(1, NumArmy, Total),nl));
            (rollDaduAttack(1,NumArmy,Total)),nl),
            write('Total : '), write(Total), nl, nl,
            getElement(L1, Index, WilayahEnemy),
            info_wilayah(WilayahEnemy, Enemy, NumEnemyArmy),
            write('Player '), write(Enemy), nl, 
            rollDaduAttack(1, NumEnemyArmy, TotalEnemy),nl,
            write('Total : '), write(TotalEnemy), nl, nl,
            (Total > TotalEnemy ->
                write('Player '), write(P), write(' menang! Wilayah '),
                write(WilayahEnemy), write(' sekarang dikuasai oleh Player '),  write(P), write('.'), nl,
                repeat,
                    write('Silahkan tentukan banyaknya tentara yang menetap di wilayah '), write(WilayahEnemy), write(': '), 
                    read(PlaceTentara), 
                    (PlaceTentara >= 1 , PlaceTentara =< NumArmy -> 
                        NewArmy is CurrentArmy + NumArmy - PlaceTentara,
                        write('Tentara yang menetap di wilayah '), write(Wilayah), write(': '), write(NewArmy), write('.'),nl,
                        write('Tentara yang menetap di wilayah '), write(WilayahEnemy), write(': '), write(PlaceTentara), write('.'),nl,
                        retract(info_wilayah(WilayahEnemy, _, _)), retract(info_wilayah(Wilayah, _, _)),
                        assertz(info_wilayah(WilayahEnemy, P, PlaceTentara)), assertz(info_wilayah(Wilayah, P, NewArmy)),
                        kalah(Enemy),nl,
                        menang(P)
                    ;
                        write('Banyaknya tentara yang ditempatkan minimal 1 dan maksimal sejumlah tentara yang berperang'), nl, fail
                    )
            ;
                write('Player '), write(Enemy),  write(' menang! Sayang sekali penyerangan Anda gagal :(.'),nl
            ), retract(attack_state(_)), AttStateNew is AttState + 1, asserta(attack_state(AttStateNew))
        ),
        (Army == 1 -> asserta(attack_state(0)); true)
    ;
        write('Seluruh wilayah yang bertetangga dengan '), write(Wilayah), write(' telah Anda kuasai. Penyerangan dibatalkan')
    )
    ).

risk :-
    game_state(main_game),
    queue_player([P|T]),
    risk_allow(X),
    random(1,7,Random),
    ((X == 0 -> ((Random == 1 ->
        assertz(risk_card(ceasefireorder,P)),
        write('Player '), write(P), write(' mendapatkan risk card CEASEFIRE ORDER'),nl,
        write('Hingga giliran berikutnya, wilayah pemain tidak dapat diserang oleh lawan.'));
    (Random == 2 ->
        assertz(risk_card(supersoldierserum,P)),
        write('Player '), write(P), write(' mendapatkan risk card SUPER SOLDIER SERUM'),nl,
        write('Hingga giliran berikutnya, semua hasil lemparan dadu saat penyerangan dan pertahanan akan bernilai 6.'));
    (Random == 3 ->
        assertz(risk_card(auxiliarytroops,P)),
        write('Player '), write(P), write(' mendapatkan risk card AUXILIARY TROOPS'),nl,
        write('Pada giliran berikutnya, Tentara tambahan yang didapatkan pemain akan bernilai 2 kali lipat.'));
    (Random == 4 ->
        assertz(risk_card(diseaseoutbreak,P)),
        write('Player '), write(P), write(' mendapatkan risk card DISEASE OUTBREAK'),nl,
        write('Hingga giliran berikutnya, semua hasil lemparan dadu saat penyerangan dan pertahanan akan bernilai 1.'));
    (Random == 5 ->
        assertz(risk_card(supplychainissue,P)),
        write('Player '), write(P), write(' mendapatkan risk card SUPPLY CHAIN ISSUE'),nl,
        write('Pada giliran berikutnya, pemain tidak mendapatkan tentara tambahan.'));
    (Random == 6 ->
        assertz(risk_card(rebellion,P)),
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
        retract(risk_card(rebellion,P)))),!,
    retract(risk_allow(0)),
    assertz(risk_allow(1)));
    (X == 1 -> ((write('Anda tidak boleh mengambil kartu lagi'))), !)).
    