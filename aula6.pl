/*
PFL - Sheet TP6 - Beginning of exercise 1
By: Gonçalo Leão
*/

male(frank).
male(jay).
male(javier).
male(merle).
male(phil).
male(mitchell).
male(cameron). % Example of a homosexual relationship. This affects how we define siblings!
male(joe).
male(manny).
male(bo).
male(dylan).
male(alex).
male(luke).
male(rexford).
male(calhoun).
male(george).

female(grace).
female(dede).
female(gloria).
female(barb).
female(claire).
female(pameron).
female(haley).
female(lily).
female(poppy).

parent(grace,phil).
parent(frank,phil).

parent(dede,claire).
parent(jay,claire).
parent(dede,mitchell).
parent(jay,mitchell).

parent(jay,joe).
parent(gloria,joe).

parent(gloria,manny).
parent(javier,manny).

parent(barb,cameron).
parent(merle,cameron).
parent(barb,pameron).
parent(merle,pameron).

parent(phil,haley).
parent(claire,haley).
parent(phil,alex).
parent(claire,alex).
parent(phil,luke).
parent(claire,luke).

parent(mitchell,lily).
parent(cameron,lily).
parent(mitchell,rexford).
parent(cameron,rexford).

parent(pameron,calhoun).
parent(bo,calhoun).

parent(dylan,george).
parent(dylan,poppy).

parent(haley,george).
parent(haley,poppy).

father(X,Y):-
	male(X),
	parent(X,Y).

mother(X,Y):-
	female(X),
	parent(X,Y).

grandparent(X,Y):-
	parent(X,Z),
	parent(Z,Y).

grandfather(X,Y):-
	male(X),
	grandparent(X,Y).

grandmother(X,Y):-
	female(X),
	grandparent(X,Y).

siblings(X,Y):-
	parent(P1,X),
	parent(P1,Y),
	parent(P2,X),
	parent(P2,Y),
	X\=Y,
	P1@<P2.

halfSiblings(X,Y):-
	parent(P1,X),
	parent(P2,X),
	parent(P2,Y),
	parent(P3,Y),
	X\=Y,
	P1\=P3,
	\+parent(P1,Y),
	\+parent(P3,X).

cousins(X,Y):-
	parent(Px,X),
	parent(Py,Y),
	siblings(Px,Py),
	X\=Y,
	Px\=Py.

uncle(X,Y):-
	male(X),
	parent(Z,Y),
	siblings(X,Z).

aunt(X,Y):-
	female(X),
	parent(Z,Y),
	siblings(X,Z).


married(jay,gloria,2008).
married(jay,dede,1968).

divorced(jay,dede,2003).

areMarried(X,Y,Year):-
	married(X,Y,Year1),
	Year1 =< Year,
	\+divorced(X,Y,_).

areMarried(X,Y,Year):-
	married(X,Y,Year1),
	Year1 =< Year,
	divorced(X,Y,Year2),
	Year2 > Year.


% exercise 2

teaches(adalberto, algorithms).
teaches(bernaderte, databases).
teaches(capitolino, compilers).
teaches(diogenes, statistics).
teaches(ermelinda, networks).

attends(alberto,algorithms).
attends(bruna,algorithms).
attends(cristina,algorithms).
attends(diogo,algorithms).
attends(eduarda,algorithms).

attends(antonio,databases).
attends(bruno,databases).
attends(cristina,databases).
attends(duarte,databases).
attends(eduardo,databases).

attends(alberto,compilers).
attends(bernardo,compilers).
attends(clara,compilers).
attends(diana,compilers).
attends(eurico,compilers).

attends(antonio,statistics).
attends(bruna,statistics).
attends(claudio,statistics).
attends(duarte,statistics).
attends(eva,statistics).

attends(alvaro,networks).
attends(beatriz,networks).
attends(claudio,networks).
attends(diana,networks).
attends(eduardo,networks).

colleagues(X,Y):-
	attends(X,C),
	attends(Y,C),
	X\=Y.

colleagues(X,Y):-
	teaches(X,C_),
	teaches(Y,C_),
	X\=Y.

student(S,P):-
	attends(S,C),
	teaches(P,C).

professor(P,S):-
	teaches(P,C),
	attends(S,C).


% exercise 4

translate(1,'Integer Overflow').
translate(2,'Division by zero').
translate(_,'Unkown ID').

% bonus: recursive factorial

fact(0,1).

fact(N,F):-
	N > 0,
	N1 is N-1,
	fact(N1, F1),
	F is N*F1.

fib(0,0).
fib(1,1).

fib(N,F):-
	N > 1,
	N1 is N-1,
	N2 is N-2,
	fib(N1,F1),
	fib(N2,F2),
	F is F1+F2.