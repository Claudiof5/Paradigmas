concatenar([], L2, L2).
concatenar([H|T], L2, [H|R]) :- concatenar(T, L2, R).

concatenar2(L1, L2, R) :- concatenar(L2, L1, R).


concatenarLL([X], X) :- !.
concatenarLL([X,Y|R], L):- concatenar(X,Y,Z), concatenarLL([Z|R], L), !.

mesclando( [], L2, L2).
mesclando( L1, [], L1).
mesclando( [H1|T1], L2 , [H1|R] ) :- mesclando(L2, T1, R ).

adicionarFinal(X, [], [X]).
adicionarFinal(X, [H|T], [H|R]) :- adicionarFinal(X, T, R).

adicionaInicio(X, [], [X]).
adicionaInicio(X, L2, [X|L2]).

inverter( [], []) :- !.
inverter([H|T], R1) :- inverter(T, R2), adicionarFinal(H, R2, R1) ,!.

inverterLG( [], []).
inverterLG( [[H1|T1]|T2], R2) :- inverter([H1|T1], LR), inverterLG(T2, R) ,adicionarFinal(LR, R, R2 ), !.
inverterLG([H|T], R1) :- inverter(T, R2), adicionarFinal(H, R2, R1) ,!.

parear(X, [Y], [X,Y]).  
parear(X, [H|T],[X,H]|R ) :- parear(X, T,R).

contem(X, [X|_]) :- !.
contem(X, [_|T]) :- contem(X, T), !.

naocontem(_, []) :- !.
naocontem(X, [H|T]) :- X \= H, naocontem(X, T).

uniao( L1, [], L1) :- !.
uniao( [], L2, L2) :- !.
uniao([H|T], L2, R) :- contem(H, L2), uniao(T, L2, R), !.
uniao([H|T], L2, [H|R]) :- uniao(T, L2, R), !.

pares( [], []) :- !.
pares( [H|T], [TodosParesDeH|TodosPares]) :- parear(H, [H|T], TodosParesDeH), pares(T, TodosPares),!.

remova(X,[X|T],T).
remova(X,[H|T],[H|NT]):- remova(X,T,NT).

permutar([],[]).
permutar([H|T], R):- permutar(T, R2), remova(H,R, R2).

conjunto([_]).
conjunto([H|T]) :- naocontem(H, T), conjunto(T).

prefixo([], _).
prefixo( [H|T1], [H|T2]) :- prefixo(T1,T2). 

subsequencia([],_).
subsequencia( L1, [H2|T2]) :- prefixo(L1, [H2|T2]), !.
subsequencia( L1, [_|T2]) :- subsequencia(L1, T2), !.

mescle([],[]).
mescle( L1, [], L1).
mescle( [], L2, L2).
mescle([H1|T1],[H2|T2], [H1|R]) :- H1 < H2, mescle(T1, [H2|T2], R).
mescle([H1|T1],[H2|T2], [H2|R]) :- mescle([H1|T1], T2, R).

divide([], [], []) :- !.
divide([X],[X],[] ) :- !.
divide( [X,Y|T], [X|R1], [Y|R2]) :- divide(T, R1, R2).


mergesort( [], []) :- !.
mergesort( [X], [X]) :- !.
mergesort( L, R) :- divide(L, L1, L2),
mergesort(L1, R1),
mergesort(L2, R2),
mescle(R1,R2,R), !.


mescleV2([],[], [], _):- !.
mescleV2( L1, [], L1,_):- !.
mescleV2( [], L2, L2,_) :- !.
mescleV2([H1|T1],[H2|T2], [H1|R], FC) :- call(FC,H1, H2), mescleV2(T1, [H2|T2], R, FC), !.
mescleV2([H1|T1],[H2|T2], [H2|R], FC) :- mescleV2([H1|T1], T2, R, FC), !.

first([],[]).
first([H|_], H).

mergesortV2( [], [], _) :- !.
mergesortV2( [X], [X], _) :- !.
mergesortV2( L, R, FC) :- divide(L, L1, L2),
mergesortV2(L1, R1, FC),
mergesortV2(L2, R2, FC),
mescleV2(R1,R2,R, FC), !.


mescleV3([],[], [], _ , _):- !.
mescleV3( L1, [], L1,_ , _):- !.
mescleV3( [], L2, L2,_ , _) :- !.
mescleV3([H1|T1],[H2|T2], [H1|R], FC, CH) :- call(CH,H1, R1), call(CH,H2,R2), call(FC,R1, R2), mescleV3(T1, [H2|T2], R, FC, CH), !.
mescleV3([H1|T1],[H2|T2], [H2|R], FC, CH) :- mescleV3([H1|T1], T2, R, FC, CH).

mergesortV3( [], [], _, _).
mergesortV3( [X], [X], _, _).
mergesortV3( L, R, FC, CH) :- 
    divide(L, L1, L2),
    mergesortV3(L1, R1, FC, CH),    
    mergesortV3(L2, R2, FC, CH),
    mescleV3(R1,R2,R, FC, CH).