
when(275,10).
when(261,12).
when(381,11).
when(398,12).
when(399,12).

where(275,owen102).
where(261,dear118).
where(381,cov216).
where(398,dear118).
where(399,cov216).

enroll(mary,275).
enroll(john,275).
enroll(mary,261).
enroll(john,381).
enroll(jim,399).

/* Exercise 1*/

% Part a
schedule(N, P, T) :- enroll(N, X), where(X, P), when(X, T).

% Part b
usage(P, T) :- where(X, P), when(X,T).

% Part c
conflict(A, B) :- where(A,P), where(B,P), when(A,T), when(B,T), A \= B.

% Part d
meet(S1, S2) :- schedule(S1, P, T), schedule(S2, P, T), S1\=S2;
	        schedule(S1, P, T1), schedule(S2, P, T2), S1\=S2, T2 =:= T1+1.

/* Exercise 2 */
rdup(L,M) :- removedup(L,M,[]).
removedup([],[],_).
removedup([A|L],[A|M],B) :- not(member(A,B)),removedup(L,M,[A|B]).
removedup([A|L],[C|M],B) :- member(A,B),removedup(L,[C|M],B).
removedup([A|L],[],B) :- member(A,B),removedup(L,[],B).

flat(L,F) :- flatten(L,[],F).
flatten([],F,F).
flatten([A|L],B,F) :- flatten(A,Z,F),flatten(L,B,Z).
flatten(A,F,[A|F]) :- not(is_list(A)).
