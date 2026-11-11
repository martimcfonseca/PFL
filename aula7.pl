pairs(X,Y):- d(X), q(Y).
pairs(X,X):- u(X).

u(1).
d(2).
d(4).
q(4).
q(16).

fact(0,1).
fact(X,F):-
	X > 0,
	X1 is X-1,
	fact(X1, F1),
	F is X*F1.

fact2(X,F):-
    fact2aux(X,1,F).

fact2_aux(0,Acc,Acc).
fact2_aux(X,Acc,F):-
    X>=1,
    X1 is X-1,
    Acc is Acc*X,
    fact2aux(X1,Acc,F).


fib(0,0).
fib(1,1).

fib(X,F):-
    X>=2,
    X1 is X-1,
    X2 is X-2,    
    fib(X1,F1),
    fib(X2,F2),
    F is F1+F2.

fib2(0,0).
fib2(X,F):-
    fib2_aux(X,0,1,F).

fib2_aux(1,_,F,F).
fib2_aux(X,F1,F2,F):-
    X>=2,
    X1 is X-1,
    F3 is F1+F2,
    fib2_aux(X1,F2,F3,F).

gates(porto,5).
gates(lisbon,8).
gates(paris,5).
gates(madrid,4).
gates(berlin,4).
gates(rome,6).
gates(london,6).

most_gates(C,N):-
    gates(C,N),
    \+((
        gates(_,N1),
        N1>N
    )).

second_most_gates(C,N):-
    gates(C,N),
    most_gates(_,Max),
    N<Max,
    \+((
        gates(_,N1),
        N1 \= Max,
        N1 > N
    )).

least_gates(C,N):-
    gates(C,N),
    \+((
        gates(_,N1),
        N1<N
    )).

second_least_gates(C,N):-
    least_gates(_,Min),!,
    gates(C,N),
    N>Min,
    \+((
        gates(_,N1),
        N1 \= Min,
        N1<N
    )).

pow_rec(_,0,1).
pow_rec(X,Y,P):-
    Y>=2,
    Y1 is Y-1,
    pow_rec(X,Y1,P1),
    P is X*P1.

pow_rec2(_,0,1).
pow_rec2(X,Y,P):-
    pow_rec2_aux(X,Y,X,P).

pow_rec2_aux(_,1,Acc,Acc).
pow_rec2_aux(X,Y,Acc,P):-
    Y>=2,
    Acc1 is Acc*X,
    Y1 is Y-1,
    pow_rec2_aux(X,Y1,Acc1,P).


square_rec(N,S):-
    square_rec_aux(N,N,0,S).

square_rec_aux(_,0,N,N).
square_rec_aux(N,R,Acc,S):-
    R>0,
    R1 is R-1,
    Acc1 is Acc+N,
    square_rec_aux(N,R1,Acc1,S).