validasiJumlahPlayer :-
    repeat,
    write('Masukkan jumlah pemain: '),
    read(Jumlah_Player),
    (
        (Jumlah_Player > 4 ; Jumlah_Player < 2) ->
        write('Mohon masukkan angka antara 2 - 4.'), nl, fail;
        true
    ), assertz(jumlah_player(Jumlah_Player)), !.

readNamaPlayer :-
    jumlah_player(Jumlah_Player),
    assertz(nama_player([])),
    readNamaPlayer(0, Jumlah_Player).

readNamaPlayer(I, Jumlah_Player) :-
    I < Jumlah_Player,
    I1 is I + 1,
    write('Masukan nama pemain '),
    write(I1),
    write(': '),
    read(Nama_Player),
    nama_player(L),
    append(L, [Nama_Player], New),
    retract(nama_player(_)),
    assertz(nama_player(New)),
    readNamaPlayer(I1, Jumlah_Player).

readNamaPlayer(I, I).

rollDaduAwal([]) :- nl, !.
rollDaduAwal([H|T]) :-
    write(H),
    write(' melempar dadu dan mendapatkan '),
    roll2Dadu,
    dadu(X),
    write(X),
    write('.'), nl,
    dadu_awal_player(L),
    append(L, [X], L1),
    retractall(dadu_awal_player(_)),
    asserta(dadu_awal_player(L1)),
    rollDaduAwal(T), !.

rollGiliranUntilValid :-
    repeat,
    nl,
    retractall(dadu_awal_player(_)),
    asserta(dadu_awal_player([])),
    nama_player(DaftarPlayer),
    rollDaduAwal(DaftarPlayer),
    dadu_awal_player(DaftarDadu),
    isUniqueMax(DaftarDadu), !.

displayUrutanPemain :-
    write('Urutan pemain: '),
    urutan_player(UrutanPlayer),
    displayUrutanPemain(UrutanPlayer), nl,
    getElement(UrutanPlayer, 1, X),
    write(X),
    write(' dapat mulai terlebih dahulu.'), nl.

displayUrutanPemain([H|T]) :-
    length(T, 0),
    write(H).

displayUrutanPemain([H|T]) :-
    length(L, Len),
    Len > 0,
    write(H),
    write(' - '),
    displayUrutanPemain(T).

distribusiTentara2([], X) :- !.

distribusiTentara2([H | T], X) :-
    asserta(tentara_idle(H, X)),
    distribusiTentara2(T, X).

distribusiTentara :-
    jumlah_player(Jumlah_Player),
    Tentara_Player is (48 div Jumlah_Player),
    nama_player(Nama_Player),
    distribusiTentara2(Nama_Player, Tentara_Player),
    nl, write('Setiap pemain mendapatkan '), write(Tentara_Player), write(' tentara.'), nl, nl,
    asserta(game_state(pembagian_wilayah)),
    writeGiliranWilayah.

writeGiliranWilayah :-
    write('Giliran '),
    queue_player([H | T]),
    write(H),
    write(' Untuk memilih wilayahnya.'), nl, nl. 

changeGiliran :-
    queue_player([H | T]),
    retract(queue_player(_)),
    append(T, [H], New),
    asserta(queue_player(New)).

resetGiliran :-
    retract(queue_player(_)),
    urutan_player(X),
    asserta(queue_player(X)).

isWilayahValid(Wilayah) :-
    wilayah(X, Y),
    member(Wilayah, Y), !.

isWilayahTaken(Wilayah) :-
    info_wilayah(Taken, _, _),
    Taken == Wilayah, !.

allWilayahTaken :-
    findall(_, info_wilayah(X, _, Y), WilayahList),
    length(WilayahList, 24).

getAnyRandomWilayah(Wilayah) :-
    queue_player([H|T]),
    findall(X, wilayah(X), WilayahList),
    length(WilayahList, Temp),
    N is Temp + 1,
    getSeed,
    random(1, N, I),
    getElement(WilayahList, I, Wilayah).

takeLocationAuto(Wilayah) :-
    (isWilayahTaken(Wilayah) ->
        true
    ;
        queue_player([H | _]),
        asserta(info_wilayah(Wilayah, H, 1)),
        tentara_idle(H, X),
        X1 is X - 1,
        retract(tentara_idle(H, X)),
        asserta(tentara_idle(H, X1)),
        write(H), write(' mengambil wilayah '), write(Wilayah), write('.'), nl, nl,
        changeGiliran
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
        true
    ), !.

totalWilayahPemain(X, Y) :-
    findall(X, info_wilayah(_, X, _), WilayahList),
    length(WilayahList, L),
    Y is L.

totalTentaraAktif(X, Sum) :-
    findall(N, info_wilayah(_, X, N), JumlahPlayerList),
    sum(JumlahPlayerList, Sum).

takeAutomatic :-
    repeat,
    getAnyRandomWilayah(Wilayah),
    takeLocationAuto(Wilayah),
    allWilayahTaken, !.

placeTroopsAuto(Wilayah, N) :-
    queue_player([P|T]),
    tentara_idle(P, X),
    info_wilayah(Wilayah, P, Y),
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
            true
        )
    ;
    true
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
    ).

writeGiliranTentara :-
    write('Giliran '),
    queue_player([H | T]),
    write(H),
    write(' Untuk meletakkan tentaranya.'), nl, nl.

donePlacingTroops :-
    \+ (tentara_idle(_, X), X =\= 0).

getRandomWilayah(Wilayah) :-
    queue_player([H|T]),
    findall(X, info_wilayah(X, H, _), WilayahList),
    length(WilayahList, Temp),
    N is Temp + 1,
    getSeed,
    random(1, N, I),
    getElement(WilayahList, I, Wilayah).

rollDaduAttack(Start, Start, Total) :- write('Dadu '), write(Start), write(': '), rollDadu, dadu(N), write(N), write('.'), Total is N, !.
rollDaduAttack(Start, End, Total) :- 
    write('Dadu '), write(Start), write(': '), rollDadu, dadu(N), write(N),  write('.'), nl,
    Next is Start + 1, rollDaduAttack(Next, End, NextTotal), Total is N + NextTotal.

rollDaduAttackSpecific(Start, Start, Total, N) :- write('Dadu '), write(Start), write(': '), write(N), write('.'), Total is N, !.
rollDaduAttackSpecific(Start, End, Total, N) :- 
    write('Dadu '), write(Start), write(': '), write(N),  write('.'), nl,
    Next is Start + 1, rollDaduAttackSpecific(Next, End, NextTotal,N), Total is N + NextTotal.

search_risk_card([],Find_What,Result) :- Result = false,!.
search_risk_card([A|B],Find_What,Result) :- A == Find_What, Result = true,!.
search_risk_card([A|B],Find_What,Result) :- A \== Find_What, search_risk_card(B,Find_What,Result),!.
    
enumerateList([], I) :- !.
enumerateList(L, I) :- L = [H|T], I1 is I+1, write(I), write('. '), write(H), nl, enumerateList(T, I1).

printAllNeighbour(Wilayah) :-
    tetangga(Wilayah, L),
    filter(L, L1),
    enumerateList(L1,1).

printWilayah([]) :- !.
printWilayah([H]) :- kode(H, _, N), write(N), !.
printWilayah(L) :- L = [H|T], kode(H, _, N), write(N), write(', '), printWilayah(T).

filter([], []).

filter([Head | Tail], [Head | FilteredTail]) :-
    queue_player([P | T]),
    (\+info_wilayah(Head, P, _)),
    filter(Tail, FilteredTail).

filter([Head | Tail], FilteredTail) :-
    queue_player([P | T]),
    info_wilayah(Head, P, _),
    filter(Tail, FilteredTail).

is_subset([], _).
is_subset([H|T], List) :-
    select(H, List, UpdatedList),
    is_subset(T, UpdatedList). 

hasBenua(Player, Benua) :-
    findall(W, info_wilayah(W, Player, _), WilayahPemain),
    wilayah(Benua, WilayahBenua),
    is_subset(WilayahBenua, WilayahPemain), !.

filterBenua(P, [], []) :- !.

filterBenua(P, [Head | Tail], [Head | FilteredTail]) :-
    hasBenua(P, Head),
    filterBenua(P, Tail, FilteredTail), !.

filterBenua(P, [Head | Tail], FilteredTail) :-
    \+hasBenua(P, Head),
    filterBenua(P, Tail, FilteredTail), !.

displayBenuaPlayer([]) :- !.
displayBenuaPlayer([H]) :- kodeBenua(H, S), write(S), !.
displayBenuaPlayer(L) :- L = [H|T], kodeBenua(H, S), write(S), write(', '), displayBenuaPlayer(T).

troopsBenua([], Count, Count) :- !.
troopsBenua([H|T], Acc, Count) :-
    (   H == australia -> NewAcc is Acc + 1
    ;   H == africa -> NewAcc is Acc + 2
    ;   H == south_america -> NewAcc is Acc + 2
    ;   H == asia -> NewAcc is Acc + 5
    ;   H == europe -> NewAcc is Acc + 4
    ;   H == north_america -> NewAcc is Acc + 3
    ;   NewAcc is Acc
    ),
    troopsBenua(T, NewAcc, Count).

displayBenuaTroops([], Count) :- !.
displayBenuaTroops([H|T], Acc) :-
    (   H == australia -> NewAcc is Acc + 1,
        write('Bonus benua australia                   : '),
        X is NewAcc,
        write(X), nl
    ;   H == africa -> NewAcc is Acc + 2,
        write('Bonus benua afrika                      : '),
        X is NewAcc,
        write(X), nl
    ;   H == south_america -> NewAcc is Acc + 2,
        write('Bonus benua amerika selatan             : '),
        X is NewAcc,
        write(X), nl
    ;   H == asia -> NewAcc is Acc + 5,
        write('Bonus benua asia                        : '),
        X is NewAcc,
        write(X), nl
    ;   H == europe -> NewAcc is Acc + 4,
        write('Bonus benua eropa                       : '),
        X is NewAcc,
        write(X), nl
    ;   H == north_america -> NewAcc is Acc + 3,
        write('Bonus benua amerika utara               : '),
        X is NewAcc,
        write(X), nl
    ;   NewAcc is Acc
    ),
    displayBenuaTroops(T, Acc).

getPlayer(X, Y) :-
    nama_player(L),
    (X == p1 ->
        getElement(L, 1, Y)
    ; X == p2 ->
        getElement(L, 2, Y)
    ; X == p3 ->
        getElement(L, 3, Y)
    ; X == p4 ->
        getElement(L, 4, Y)
    ), !.

intersection([], _, []).

intersection([X | Rest1], List2, [X | Intersection]) :-
    member(X, List2),
    !,
    intersection(Rest1, List2, Intersection).

intersection([_ | Rest1], List2, Intersection) :-
    intersection(Rest1, List2, Intersection).

checkWilayah(Benua, []).

checkWilayah(Benua, [H | T]) :-
    kode(H, Kode, Nama),
    write(Kode), nl,
    info_wilayah(H, _, Jumlah),
    write('Nama              : '), write(Nama), nl,
    write('Jumlah Tentara    : '), write(Jumlah), nl, nl,
    checkWilayah(Benua, T).
    
checkBenua(Benua, Nama) :-
    wilayah(Benua, WilayahBenua),
    kodeBenua(Benua, NamaBenua),
    findall(X, info_wilayah(X, Nama, _), WilayahList),
    intersection(WilayahBenua, WilayahList, WilayahBenuaPemain),
    length(WilayahBenua, LengthBenua),
    length(WilayahBenuaPemain, Length),
    (Length is 0 ->
        true
    ;
        write('Benua '), write(NamaBenua),  
        write(' ('), write(Length), write('/'), write(LengthBenua), write(')'), nl,
        checkWilayah(Benua, WilayahBenuaPemain)
    ).

deletePlayer(P, [], []) :- !.
deletePlayer(P, [Head | Tail], [Head | FilteredTail]) :-
    Head \== P,
    deletePlayer(P, Tail, FilteredTail), !.
deletePlayer(P, [Head | Tail], FilteredTail) :-
    Head == P,
    deletePlayer(P, Tail, FilteredTail), !.

filterCeasefire([], []).

filterCeasefire([Head | Tail], [Head | FilteredTail]) :-
    findall(Card,risk_card(Card,Head),ListCard),
    search_risk_card(ListCard, ceasefireorder, Result),
    Result == true,
    filter(Tail, FilteredTail).

filterCeasefire([Head | Tail], FilteredTail) :-
    findall(Card,risk_card(Card,Head),ListCard),
    search_risk_card(ListCard, ceasefireorder, Result),
    Result == false,
    filter(Tail, FilteredTail).