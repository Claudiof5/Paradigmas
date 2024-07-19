concatenar( [], L2, L2 ).
concatenar( [H|T], L2, [H|X] ) :- concatenar(T, L2, X).

concatenarInv( L1, L2, X ) :- concatenar(L2, L1, X).

concatenarll( [], []):- !.
concatenarll( [X], X ) :- !.
concatenarll( [X,Y|T], F) :- concatenar(X,Y, R), concatenarll([R|T], F), !. 



adicionarFinal( X, [], [X]) :- !.
adicionarFinal( X, [H|T], [H|R]) :- adicionarFinal(X, T, R), !.

adicionarFinal2(X, L, R) :- concatenar( L, [X], R), !.

inverter( [], []) :-!.
inverter( [X|T], R1) :- inverter(T, R2), adicionarFinal2(X, R2, R1).


islist( [] ) :- !.
islist( [_|_] ) :- !.

inverterLG( [], []) :-!.
inverterLG( [H|T], R1) :- islist(H), inverterLG(H, H2), inverterLG(T, R2), adicionarFinal( H2, R2, R1), !.
inverterLG( [H|T], R1) :- inverterLG(T, R2), adicionarFinal( H, R2, R1).

parear( _, [], [] ) :- !.
parear( X,[H|T], [[X,H]|R] ) :- parear(X, T, R), !.

pares( [], []) :- !.
pares( [_], []) :- !.
pares( [X|T], R1) :- parear(X,T,R2), pares(T, R3), concatenar(R2, R3, R1), !.

remova(X,[X|T],T).
remova(X,[H|T],[H|NT]):- remova(X,T,NT).


permutar([], []) :- !.
permutar([H|T], R) :-  permutar( T, R2), remova(H, R ,R2).

conjunto([]) :- !.
conjunto( [X|T]) :- \+ member(X, T), conjunto(T), !.

prefixo( [], _) :-!.
prefixo( [H1|T1], [H1|T2]) :- prefixo(T1,T2), !.


subsequencia(L1, L2) :- prefixo(L1, L2), !.
subsequencia(L1,[_|T]) :-subsequencia(L1, T), !.


mescle(L, [], L) :- !.
mescle([], L, L) :- !.
mescle( [H1|T1], [H2|T2], [H1|R]) :- H1 =< H2 , mescle(T1, [H2|T2], R), !.
mescle( [H1|T1], [H2|T2], [H2|R]) :- mescle(T2, [H1|T1], R), !.

divide([], [], []) :- !.
divide([X],[X],[] ) :- !.
divide( [X,Y|T], [X|R1], [Y|R2]) :- divide(T, R1, R2).


mergesort( [], []) :- !.
mergesort( [X], [X]) :- !.
mergesort( L, R) :- divide(L, L1, L2),
mergesort(L1, R1),
mergesort(L2, R2),
mescle(R1,R2,R), !.

liste(0, []) :- !.
liste(N, [N|R]) :- N1 is N-1, liste(N1, R), !.

iguais( [], []) :-!.
iguais( [H|T], L) :- member(H, L), delete(L, H, R), iguais(T, R), !.

mescleLL([], []) :- !.
mescleLL( [X], X) :- !.
mescleLL([X,Y|T], R1) :- mescle(X,Y,R2), mescleLL([R2|T],R1), !.

decodifica( [], []) :- !.
decodifica( [[0,_]|T], R) :- decodifica( T, R), !.
decodifica( [[N,C]|T], [C|R]) :- N1 is N-1, decodifica( [[N1, C]|T], R), !.

profundidade([], 1) :- !.

profundidade([H|T], N) :-
    is_list(H),
    profundidade(H, NH),
    profundidade(T, NT), 
    N is max(NH + 1, NT), 
    !.
profundidade([_|T], N) :-
    profundidade(T, N),
    !.  
profundidade([_], 1) :- !.

oculte( _, [], []) :- !.
oculte( X, [H|T], [xxxx|R]) :- X == H, oculte(X, T, R), !.
oculte( X, [H|T], [H|R]) :- oculte(X, T, R), !.

oculteConj( [], L, L ) :- !.
oculteConj( [H|T], L, R1) :- oculte(H, L, R2),  oculteConj(T, R2, R1), !.

intercala( X, _, 1, [X]) :- !.
intercala( X, Y, N, [X|R]) :- N1 is N-1, intercala(Y,X,N1,R), !.

sumarize( [],[],[]) :-!.
%sumarize( [H|T], L1, L2) :- member(H, L1), findindex(H, L1, I), incindex(I, L2, L3), sumarize(T, L1, L3), !.
sumarize( [H|T], [H|L1], [1|L2]) :- summarize(T, L1, L2), !.


findindex( X, L, R) :- findindex2( X, L, 0, R), !.

findindex2(X,[X|_], R, R) :- !.
findindex2(X,[_|T], I, R) :- I2 is I+1, findindex2(X,T,I2,R), !.

incindex( 0, [H|T], [H2|T]) :- H2 is H + 1, !.
incindex( N, [H|T], [H|R]) :- N2 is N - 1, incindex(N2, T, R), !.
