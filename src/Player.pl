:- include('Progres1_G14').

checkPlayerDetail(X) :-
    game_state(main_game),
    write('PLAYER '),
    kodePlayer(X, Y),
    write(Y), nl, nl,
    getPlayer(X, N),
    totalWilayahPemain(N, T),
    tentara_idle(N, JT),
    filterBenua(N, [north_america, europe, asia, south_america, africa, australia], Benua),
    totalTentaraAktif(N, JA),
    write('Nama                  : '),
    write(N), nl,
    write('Benua                 : '),
    displayBenuaPlayer(Benua), nl,
    write('Total Wilayah         : '),
    write(T), nl,
    write('Total Tentara Aktif   : '),
    write(JA), nl,
    write('Total Tentara Tambahan: '), 
    write(JT), nl, !.

checkPlayerTerritories(X) :-
    game_state(main_game),
    getPlayer(X, Nama),
    write('Nama              : '), write(Nama), nl, nl,
    checkBenua(africa, Nama),
    checkBenua(australia, Nama),
    checkBenua(asia, Nama),
    checkBenua(europe, Nama),
    checkBenua(north_america, Nama),
    checkBenua(south_america, Nama),
    !.

checkIncomingTroops(X) :-
    game_state(main_game),
    getPlayer(X, N),
    totalWilayahPemain(N, T),
    JT is floor(T/2),
    filterBenua(N, [north_america, europe, asia, south_america, africa, australia], Benua),
    troopsBenua(Benua, 0, J),
    JB is J,
    write('Nama                                    : '),
    write(N), nl,
    write('Total wilayah                           : '),
    write(T), nl,
    write('Jumlah tentara tambahan dari wilayah    : '),
    write(JT), nl,
    displayBenuaTroops(Benua, 0),
    write('Total tentara tambahan                  : '),
    Total is JT + JB,
    write(Total).

kalah(Player) :-
    queue_player(Q),
    findall(X, info_wilayah(X, Player, _), WilayahList),
    length(WilayahList, Length),
    (Length == 0 ->  write('Jumlah wilayah player '), write(Player), write(' 0.'), nl, write(Player), write(' keluar dari permainan!'), nl, 
    deletePlayer(Player, Q, NewQ), retract(queue_player(_)), asserta(queue_player(NewQ))
    ; true
    ). 

menang(Player) :- 
    findall(X, info_wilayah(X, Player, _), WilayahList),
    findall(Y, info_wilayah(Y, _, _), AllWilayah),
    (WilayahList == AllWilayah -> write('Selamat '), write(Player), write(' telah menguasai dunia '), nl, exitGame; true).

exitGame :-
    asserta(game_state(a)),
    asserta(jumlah_player(a)),
    asserta(nama_player(a)),
    asserta(urutan_player(a)),
    asserta(queue_player(a)),
    asserta(dadu(a)),
    asserta(info_wilayah(a,a,a)),
    asserta(nama_wilayah(a,a)),
    asserta(current_player(a)),
    asserta(dadu_awal_player(a)),
    asserta(tentara_idle(a,a)),
    asserta(move_state(a)),
    asserta(attack_state(a)),
    asserta(risk_card(a,a)),
    asserta(risk_allow(a)),
    retract(game_state(_)),
    retract(jumlah_player(_)),
    retract(nama_player(_)),
    retract(urutan_player(_)),
    retract(queue_player(_)),
    retract(dadu(_)),
    retract(info_wilayah(_,_,_)),
    retract(nama_wilayah(_,_)),
    retract(current_player(_)),
    retract(dadu_awal_player(_)),
    retract(tentara_idle(_,_)),
    retract(move_state(_)),
    retract(attack_state(_)),
    retract(risk_card(_,_)),
    retract(risk_allow(_)).