replace(E1,[E1|List],E2,[E2|List]).
replace(E1,[X|L1],E2,[X|L2]) :-
	replace(E1,L1,E2,L2).
	
zip([],[],[]).
zip([A|Xa],[B|Xb],[(A-B)|Xc]) :-
	zip(Xa,Xb,Xc).

sublist([],_).
sublist([H|T1],[H|T2]) :-
		sublist(T1,T2).
sublist([H|T1],[_|T2]) :-
	sublist([H|T1],T2).