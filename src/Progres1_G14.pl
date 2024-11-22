/* Fakta Benua */
benua(north_america).
benua(europe).
benua(asia).
benua(south_america).
benua(africa).
benua(australia).

kodeBenua(north_america, 'Amerika Utara').
kodeBenua(europe, 'Eropa').
kodeBenua(asia, 'Asia').
kodeBenua(south_america, 'Amerika Selatan').
kodeBenua(africa, 'Afrika').
kodeBenua(australia, 'Australia').

/* Fakta Wilayah */
wilayah(north_america, [na1, na2, na3, na4, na5]).
wilayah(europe, [e1, e2, e3, e4, e5]).
wilayah(asia, [a1, a2, a3, a4, a5, a6, a7]).
wilayah(south_america, [sa1, sa2]).
wilayah(africa, [af1, af2, af3]).
wilayah(australia, [au1, au2]).

tetangga(na1, [na2, na3, a3]).
tetangga(na2, [na1, na4, na5]).
tetangga(na3, [na1, na4, sa1, a3]).
tetangga(na4, [na2, na3, na5]).
tetangga(na5, [na2, na4, e1]).
tetangga(e1, [e2, e3, na5]).
tetangga(e2, [e1, e4, a1]).
tetangga(e3, [e1, e4, af1]).
tetangga(e4, [e2, e3, e5, af2]).
tetangga(e5, [e4, a4, af2]).
tetangga(a1, [e2, a4]).
tetangga(a2, [a4, a5, a6]).
tetangga(a3, [a5, na1, na3]).
tetangga(a4, [e5, a1, a2, a5, a6]).
tetangga(a5, [a2, a3, a4, a6]).
tetangga(a6, [a2, a4, a5, a7, au1]).
tetangga(a7, [a6]).
tetangga(au1, [a6, au2]).
tetangga(au2, [au1, sa2]).
tetangga(sa1, [sa2, na3]).
tetangga(sa2, [au2, sa1, af1]).
tetangga(af1, [sa2, e3, af2, af3]).
tetangga(af2, [af1, e4, e5, af3]).
tetangga(af3, [af1, af2]).

wilayah(na1).
wilayah(na2).
wilayah(na3).
wilayah(na4).
wilayah(na5).
wilayah(e1).
wilayah(e2).
wilayah(e3).
wilayah(e4).
wilayah(e5).
wilayah(a1).
wilayah(a2).
wilayah(a3).
wilayah(a4).
wilayah(a5).
wilayah(a6).
wilayah(a7).
wilayah(sa1).
wilayah(sa2).
wilayah(af1).
wilayah(af2).
wilayah(af3).
wilayah(au1).
wilayah(au2).

kode(na1, 'NA1', 'USA').
kode(na2, 'NA2', 'CANADA').
kode(na3, 'NA3', 'MEXICO').
kode(na4, 'NA4', 'GREENLAND').
kode(na5, 'NA5', 'JAMAICA').
kode(e1, 'E1', 'ENGLAND').
kode(e2, 'E2', 'NETHERLANDS').
kode(e3, 'E3', 'GERMANY').
kode(e4, 'E4', 'FRANCE').
kode(e5, 'E5', 'ITALY').
kode(a1, 'A1', 'INDONESIA').
kode(a2, 'A2', 'SINGAPORE').
kode(a3, 'A3', 'MALAYSIA').
kode(a4, 'A4', 'CHINA').
kode(a5, 'A5', 'JAPAN').
kode(a6, 'A6', 'INDIA').
kode(a7, 'A7', 'KOREA').
kode(sa1, 'SA1', 'ARGENTINA').
kode(sa2, 'SA2', 'BRAZIL').
kode(af1, 'AF1', 'ZIMBABWE').
kode(af2, 'AF2', 'NIGERIA').
kode(af3, 'AF3', 'MOROCCO').
kode(au1, 'AU1', 'AUSTRALIA').
kode(au2, 'AU2', 'NEW ZEALAND').

kodePlayer(p1, 'P1').
kodePlayer(p2, 'P2').
kodePlayer(p3, 'P3').
kodePlayer(p4, 'P4').

:- dynamic(game_state/1).
:- dynamic(jumlah_player/1).
:- dynamic(nama_player/1).
:- dynamic(urutan_player/1).
:- dynamic(queue_player/1).
:- dynamic(dadu/1).
:- dynamic(info_wilayah/3).
:- dynamic(nama_wilayah/2).
:- dynamic(current_player/1).
:- dynamic(dadu_awal_player/1).
:- dynamic(tentara_idle/2).
:- dynamic(move_state/1).
:- dynamic(attack_state/1).
:- dynamic(risk_card/2).
:- dynamic(risk_allow/1).

/* Rule */
getSeed :-
    sleep(0.1),
    real_time(Seed),
    set_seed(Seed).

roll2Dadu :-
    getSeed,
    asserta(dadu(0)),
    retract(dadu(_)),
    random(2, 13, NewRandomNumber),
    asserta(dadu(NewRandomNumber)), !.

rollDadu :-
    getSeed,
    asserta(dadu(0)),
    retract(dadu(_)),
    random(1, 7, NewRandomNumber),
    asserta(dadu(NewRandomNumber)), !.