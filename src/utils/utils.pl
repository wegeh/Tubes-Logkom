/* Bagian I */
/* Deklarasi Fakta */
pria(athif).
pria(dillon).
pria(henri).
pria(michael).
pria(hanif).
pria(robert).
pria(bagas).
pria(fio).
pria(daniel).
pria(rupert).

wanita(joli).
wanita(elysia).
wanita(margot).
wanita(jena).
wanita(jeni).
wanita(ana).
wanita(emma).

usia(athif, 60).
usia(joli, 58).
usia(dillon, 63).
usia(elysia, 500).
usia(henri, 48).
usia(margot, 43).
usia(michael, 28).
usia(hanif, 47).
usia(robert, 32).
usia(bagas, 29).
usia(jena, 27).
usia(fio, 30).
usia(jeni, 28).
usia(ana, 23).
usia(daniel, 7).
usia(rupert, 6).
usia(emma, 6).

menikah(athif, joli).
menikah(joli, athif).
menikah(dillon, elysia).
menikah(elysia, dillon).
menikah(henri, margot).
menikah(margot, henri).
menikah(jena, fio).
menikah(fio, jena).
menikah(jeni, fio).
menikah(fio, jeni).

anak(margot, athif).
anak(margot, joli).
anak(michael, athif).
anak(michael, joli).
anak(hanif, dillon).
anak(hanif, elysia).
anak(robert, henri).
anak(robert, margot).
anak(bagas, henri).
anak(bagas, margot).
anak(jena, henri).
anak(jena, margot).
anak(jeni, hanif).
anak(ana, hanif).
anak(daniel, jena).
anak(daniel, fio).
anak(rupert, jena).
anak(rupert, fio).
anak(emma, fio).
anak(emma, jeni).

/* Deklarasi Rules */
saudara(X, Y) :- anak(X, Z), anak(Y, Z), X \== Y.

saudaratiri(X, Y) :- anak(X, Z), anak(Y, Z), anak(X, W), anak(Y, V), V \== W, W \== Z, V \== Z.

kakak(X, Y) :- saudara(X, Y), usia(X, A), usia(Y, B), A > B.

keponakan(X, Y) :- anak(X, Z), saudara(Z, Y).   

mertua(X, Y) :- menikah(Y, Z), anak(Z, X).

nenek(X, Y) :- wanita(X), anak(Y, Z), anak(Z, X).

keturunan(X, Y) :- anak(X, Y).
keturunan(X, Y) :- anak(X, Z), anak(Z, Y).
keturunan(X, Y) :- anak(X, W), anak(W, V), anak(V, Y).

lajang(X) :- pria(X), \+menikah(X, _).
lajang(X) :- wanita(X), \+menikah(X, _).

anakbungsu(X) :- anak(X, Y), pria(Y), \+kakak(X, _).

yatimpiatu(X) :- pria(X), \+anak(X, _).
yatimpiatu(X) :- wanita(X), \+anak(X, _).

/* Bagian II */
exponent(A, 0, 1) :- !.
exponent(A, B, X) :- C is B - 1, exponent(A, C, Y), X is Y * A.

/* Rule Tambahan */
/* T Awal ganjil */
population_ganjil(P, R, 0, C, P) :- !.
population_ganjil(P, R, T, C, X) :- T1 is T - 1, C1 is C + 1, 1 is T mod 2, P1 is P * R + C1, population_ganjil(P1, R, T1, C1, X).
population_ganjil(P, R, T, C, X) :- T1 is T - 1, C1 is C + 1, 0 is T mod 2, P1 is P * R - C1, population_ganjil(P1, R, T1, C1, X).

/* T Awal genap */
population_genap(P, R, 0, C, P) :- !.
population_genap(P, R, T, C, X) :- T1 is T - 1, C1 is C + 1, 0 is T mod 2, P1 is P * R + C1, population_genap(P1, R, T1, C1, X).
population_genap(P, R, T, C, X) :- T1 is T - 1, C1 is C + 1, 1 is T mod 2, P1 is P * R - C1, population_genap(P1, R, T1, C1, X).
/**/

population(P, R, T, C, X) :- 1 is T mod 2, population_ganjil(P, R, T, C, X).
population(P, R, T, C, X) :- 0 is T mod 2, population_genap(P, R, T, C, X).

perrin(0, 3) :- !.
perrin(1, 0) :- !.
perrin(2, 2) :- !.
perrin(N, X) :- N2 is N - 2, N3 is N - 3, perrin(N2, Y), perrin(N3, Z), X is Y + Z.

hcf(A, 0, A) :- !.
hcf(0, A, A) :- !.
hcf(B, A, X) :- A > B, hcf(A, B, X), !.
hcf(A, B, X) :- C is A - B, hcf(B, C, X) , !.

/* Rule tambahan untuk makePattern */
hitungBatas(N, Batas) :- 1 is N mod 2, Batas is N // 2.
hitungBatas(N, Batas) :- 0 is N mod 2, Batas is (N // 2) - 1.

prosesDepth(Depth, Batas, I, N, Depth1) :- 
    Depth < Batas,
    I =< N - (Batas + 2),
    Depth1 is Depth + 1.
prosesDepth(Depth, Batas, I, N, Depth1) :-
    Depth >= Batas,
    I =< N - (Batas + 2),
    Depth1 = Depth.
prosesDepth(Depth, Batas, I, N, Depth1) :-
    I > N - (Batas + 2),
    Depth1 is Depth - 1.

while1(I, Hdepth, Batas, N) :-
    I < N,
    J = 0,
    Wdepth = 0,
    while2(I, J, Hdepth, Wdepth, N),
    nl,
    prosesDepth(Hdepth, Batas, I, N, Hdepth1),
    I1 is I + 1,
    while1(I1, Hdepth1, Batas, N).
while1(I, Hdepth, Batas, N) :- I >= N.

while2(I, J, Hdepth, Wdepth, N) :-
    J < N,
    write(Wdepth),
    prosesDepth(Wdepth, Hdepth, J, N, Wdepth1),
    J1 is J + 1,
    while2(I, J1, Hdepth, Wdepth1, N).
while2(I, J, Hdepth, Wdepth, N) :- J >= N.
/**/

makePattern(N) :-
    I = 0,
    Hdepth = 0,
    hitungBatas(N, Batas),
    while1(I, Hdepth, Batas, N).

/* Bagian III */
/* 1. List Statistic */
min([X], X) :- !.
min([X|L], Min) :- min(L, MinTail), X < MinTail, Min is X.
min([X|L], Min) :- min(L, MinTail), X >= MinTail, Min is MinTail.

max([X], X) :- !.
max([X|L], Max) :- max(L, MaxTail), X > MaxTail, Max is X.
max([X|L], Max) :- max(L, MaxTail), X =< MaxTail, Max is MaxTail.

range(L, Range) :- max(L, Max), min(L, Min), Range is Max - Min.

count([X], 1) :- !.
count([X|L], Count) :- count(L, TailCount), Count is TailCount + 1.

sum([X], X) :- !.
sum([X|L], Sum) :- sum(L, TailSum), Sum is TailSum + X.

/* 2. List Manipulation */
getIndex([], X, Index) :- fail.
getIndex([H|T], X, Index) :- H is X, Index is 1.
getIndex([H|T], X, Index) :- H \== X, getIndex(T, X, Temp), Index is Temp + 1.

getElement([], Index, Element) :- fail.
getElement([H|T], 1, H).
getElement([H|T], Index, Element) :- Index \== 1, TailIndex is Index - 1, getElement(T, TailIndex, Element).

/* Rule Tambahan */
setElement([H|T], Idx, Elm, Result) :- Idx is 1, Result = [Elm|T].
setElement([H|T], Idx, Elm, Result) :- TempIdx is Idx - 1, setElement(T, TempIdx, Elm, Temp), Result = [H|Temp].
/**/

swap(L, Idx1, Idx2, Result) :- getElement(L, Idx1, Elm1), getElement(L, Idx2, Elm2), setElement(L, Idx2, Elm1, Temp), setElement(Temp, Idx1, Elm2, Result).

slice([], Start, End, Result) :- Result = [], !.
slice([H|T], Start, End, Result) :- Start >= End, Result = [], !.
slice([H|T], Start, End, Result) :- Start == 1, End == 2, Result = [H], !.
slice([H|T], Start, End, Result) :- Start == 1, End \== 2, TempEnd is End - 1, slice(T, Start, TempEnd, TempResult), Result = [H|TempResult], !.
slice([H|T], Start, End, Result) :- Start \== 1, TempStart is Start - 1, TempEnd is End - 1, slice(T, TempStart, TempEnd, Result), !.

sortList([H], Result) :- Result = [H], !.
sortList([H|T], Result) :- min(T, MinTail), getIndex([H|T], MinTail, MinTailIndex), MinTail < H, swap([H|T], 1, MinTailIndex, Temp), [H1|T1] = Temp, sort(T1, Temp2), Result = [H1|Temp2].
sortList([H|T], Result) :- min(T, MinTail), getIndex([H|T], MinTail, MinTailIndex), MinTail >= H, sort(T, Temp), Result = [H|Temp].

isUniqueMax(L) :-
    max(L, Max),
    \+(appearsTwice(Max, L)).

appearsTwice(Element, List) :-
    select(Element, List, Rest),
    member(Element, Rest), !.

/* Bonus */
:- dynamic(randomNumber/1).
:- dynamic(skor/1).
:- dynamic(hasStarted/1).

getRandomNumber :-
    real_time(Seed),
    set_seed(Seed),
    random(1, 101, NewRandomNumber),
    asserta(randomNumber(NewRandomNumber)), !.

resetRandomNumber :-
    retract(randomNumber(_)),
    getRandomNumber, !.

writeScore :-
    write('Score: '),
    skor(Skor),
    write(Skor), nl.

checkStart :-
    hasStarted(Check),
    Check is 1.

start :-
    getRandomNumber,
    asserta(skor(0)),
    asserta(hasStarted(1)),
    nl,
    write('Welcome to Number Guesser Prolog!'), nl,
    write('Guess a Number between 1 to 100'), nl,
    writeScore.

guess(X) :-
    checkStart,
    randomNumber(RandomNumber),
    RandomNumber < X,
    nl, nl, write('Too Large.'), nl,
    skor(PrevSkor),
    retract(skor(_)),
    NewSkor is PrevSkor - 1,
    asserta(skor(NewSkor)),
    writeScore, nl, !.
guess(X) :-
    checkStart,
    randomNumber(RandomNumber),
    RandomNumber > X,
    nl, nl, write('Too Small.'), nl,
    skor(PrevSkor),
    retract(skor(_)),
    NewSkor is PrevSkor - 1,
    asserta(skor(NewSkor)),
    writeScore, nl, !.
guess(X) :-
    checkStart,
    randomNumber(RandomNumber),
    X == RandomNumber,
    nl, nl, write('Correct.'), nl,
    skor(PrevSkor),
    retract(skor(_)),
    NewSkor is PrevSkor + 10,
    asserta(skor(NewSkor)),
    writeScore, nl, !.

reset :-
    checkStart,
    nl, write('Secret Number is '),
    randomNumber(RandomNumber),
    write(RandomNumber), nl,
    writeScore,
    write('resetting the game..'), nl,
    retract(randomNumber(_)),
    retract(skor(_)),
    retract(hasStarted(_)),
    start, !.

exit :-
    checkStart,
    nl, write('Thankyou for Playing!'), nl,
    retract(skor(_)),
    retract(randomNumber(_)),
    retract(hasStarted(_)).