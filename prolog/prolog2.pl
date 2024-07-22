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
%---------------------------------------------------------------------------------------------------------
sumarize(L, L1, L2) :- return_set(L, L1), count_set( L1, L, L2), !.
return_set( L, S) :- return_set1( L, [], S), !.

return_set1([], S, S) :-!.
return_set1([X|T], Acc, R) :- member(X, Acc), return_set1(T, Acc, R), !.
return_set1([X|T], Acc, R) :- append( Acc, [X], Acc2), return_set1(T, Acc2, R), !.


count_set( S, L, R) :- count_set1( S, L, [], R), !.
count_set1( [], _, R, R ) :- !.
count_set1( [X|T], L , Acc, R) :- count_element(X, L, Xcount), append( Acc, [Xcount], Acc2),count_set1(T, L, Acc2, R), !.

count_element( X, L, R) :- count_element1(X, L, 0, R), !.
count_element1( _, [], R, R) :- !.
count_element1( X, [X|T], Acc, R) :- Acc2 is Acc + 1, count_element1(X, T, Acc2, R), !.
count_element1( X, [_|T], Acc, R) :- count_element1(X, T, Acc, R), !.

%---------------------------------------------------------------------------------------------------------
empacote([], []) :- !.
empacote( L, Result) :- empacote1( L, [], Result), !.

empacote1( [], R, R) :- !.
empacote1( L, Acc, R) :- biggest_equal_predicate(L, Result, Rest), append(Acc, [Result], Acc2) ,empacote1(Rest,Acc2, R), !.

biggest_equal_predicate( [X|T], Result, Rest) :- biggest_equal_predicate1( X, T, [X], Result,Rest), !.

biggest_equal_predicate1( _,[], Result, Result, []) :- !.
biggest_equal_predicate1(X, [X|T], Acc, Result, Rest) :- append([X],Acc, Acc2),biggest_equal_predicate1( X, T, Acc2, Result, Rest), !.
biggest_equal_predicate1(_, [Y|Rest], Result, Result, [Y|Rest]) :- !.


%---------------------------------------------------------------------------------------------------------

circule( L, 0, L) :- !.
circule( [H|T], N, Result) :- N > 0, N1 is N-1, append(T,[H],R), circule(R, N1, Result), !. 
circule( [H|T], N, Result) :- N1 is N+1, get_last_element([H|T], LE, Rest), circule( [LE|Rest], N1, Result), !.


get_last_element(L, LastElement, Rest) :- get_last_element1(L, [], LastElement, Rest),  !.

get_last_element1( [X], Rest, X, Rest) :- !.
get_last_element1( [X|T], Acc, LE, R) :-append(Acc, [X], Acc2), get_last_element1( T, Acc2, LE , R), !.

%---------------------------------------------------------------------------------------------------------
profundidade([],1) :- !.
profundidade([H|T],R):-atomic(H), profundidade(T,R), !.
profundidade([H|T],R):- profundidade(H,R1), profundidade(T,R2), R3 is R1+1, max(R3,R2,R), !.

max(N1, N2, N1) :-
    N1 >= N2.
max(N1, N2, N2) :-
    N1 < N2.
%---------------------------------------------------------------------------------------------------------
%https://users.utcluj.ro/~cameliav/lp/7_Deep_Lists.pdf
