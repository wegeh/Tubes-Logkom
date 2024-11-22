initializeGiliran :-
    asserta(dadu_awal_player([])),
    nama_player(DaftarPlayer),
    rollGiliranUntilValid,
    dadu_awal_player(DaftarDadu),
    max(DaftarDadu, Max),
    getIndex(DaftarDadu, Max, Index),
    getElement(DaftarPlayer, Index, FirstPlayer),
    slice(DaftarPlayer, 1, Index, Prev),
    count(DaftarPlayer, Length),
    Length1 is Length + 1,
    Index1 is Index + 1,
    slice(DaftarPlayer, Index1, Length1, Next),
    append([FirstPlayer], Next, Temp),
    append(Temp, Prev, DaftarPlayerTerurut),
    asserta(urutan_player(DaftarPlayerTerurut)),
    asserta(queue_player(DaftarPlayerTerurut)),
    displayUrutanPemain, !.

takeLocation(Wilayah) :-
    (game_state(pembagian_wilayah) ->
        (isWilayahValid(Wilayah) ->
            (isWilayahTaken(Wilayah) ->
                write('Wilayah sudah dikuasai. Tidak bisa mengambil.'), nl, nl
            ;
                queue_player([H | _]),
                asserta(info_wilayah(Wilayah, H, 1)),
                tentara_idle(H, X),
                X1 is X - 1,
                retract(tentara_idle(H, X)),
                asserta(tentara_idle(H, X1)),
                write(H), write(' mengambil wilayah '), write(Wilayah), write('.'), nl, nl,
                changeGiliran
            )
        ;
            write('Wilayah tidak ditemukan. '), nl, nl
        ),
        (allWilayahTaken ->
            write('Seluruh wilayah telah diambil pemain.'), nl,
            write('Memulai pembagian sisa tentara.'), nl,
            resetGiliran,
            retract(game_state(_)),
            asserta(game_state(pembagian_tentara)),
            queue_player([H1 | T1]),
            write('Giliran '), write(H1), write(' untuk meletakkan tentaranya.'), nl, nl
        ;
            writeGiliranWilayah
        )
    ;
        write('takeLocation sedang tidak bisa dilakukan.'), nl, nl
    ).

placeAutomatic :-
    game_state(pembagian_tentara),
    queue_player([H|T]),
    repeat,
    getRandomWilayah(Wilayah),
    getSeed,
    random(1, 6, N),
    placeTroopsAuto(Wilayah, N),
    tentara_idle(H, X),
    X is 0, !.

placeTroops(Wilayah, N) :-
    (game_state(pembagian_tentara) ->
        (isWilayahValid(Wilayah) ->
            queue_player([P|T]),
            (info_wilayah(Wilayah, P, Y) ->
                tentara_idle(P, X),
                (X >= N ->
                    write(P), write(' meletakkan '), write(N), 
                    write(' tentara di wilayah '), write(Wilayah), write('.'), nl,
                    X1 is X - N,
                    retract(tentara_idle(P, X)),
                    asserta(tentara_idle(P, X1)),
                    Y1 is N + Y,
                    retract(info_wilayah(Wilayah, P, Y)),
                    asserta(info_wilayah(Wilayah, P, Y1)),
                    (X1 is 0, \+donePlacingTroops ->
                        write('Seluruh tentara '), write(P), write(' sudah diletakkan.'), nl, nl,
                        changeGiliran,
                        writeGiliranTentara
                    ;
                        write('Terdapat '), write(X1), write(' tentara yang tersisa.'), nl, nl
                    )
                ;
                    write('Pasukan tidak cukup. '), nl,
                    write('Jumlah pasukan tersisa '), write(P), write(': '), write(X), nl, nl
                )
            ;
                write('Wilayah tersebut dimiliki pemain lain.'), nl,
                write('Silahkan pilih wilayah lain.'), nl, nl
            )
        ;
            write('Wilayah tidak ditemukan. '), nl, nl
        )
    ;
        write('placeTroops sedang tidak bisa dilakukan.'), nl, nl
    ),
    (donePlacingTroops ->
        write('Seluruh pemain telah meletakkan sisa tentara.'), nl,
        write('Memluai permainan.'), nl, nl,
        resetGiliran,
        retract(game_state(_)),
        asserta(game_state(main_game)),
        asserta(move_state(0)),
        asserta(attack_state(0)),
        asserta(risk_allow(0)),
        write('Sekarang giliran Player '),
        queue_player([Z|_]),
        write(Z),
        write('!'), nl,
        write('Player '),
        write(Z),
        write(' mendapatkan '),
        filterBenua(Z, [north_america, europe, asia, south_america, africa, australia], Benua),
        troopsBenua(Benua, 0, J),
        totalWilayahPemain(Z, L),
        tentara_idle(Z, Awal),
        retract(tentara_idle(Z, _)),
        T1 is floor(L/2),
        JB is J,
        Total is T1 + JB,
        Akhir is Awal + Total,
        asserta(tentara_idle(Z, Akhir)),
        write(Total),
        write(' tentara tambahan.'), nl,
        displayMap
    ;
        true
    ), !.