:- include('Progres1_G14').
:- include('Map.pl').
:- include('utils/utils.pl').
:- include('Turn.pl').
:- include('utils/functions.pl').
:- include('Wilayah.pl').
:- include('Player.pl').
:- include('Bonus.pl').
:- include('Initiating.pl').

startGame :-
    validasiJumlahPlayer,
    readNamaPlayer,
    initializeGiliran,
    distribusiTentara.