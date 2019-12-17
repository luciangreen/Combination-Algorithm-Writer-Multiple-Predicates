%% remove_duplicate_predicates

%%remove_duplicate_predicates(Predicates1
%%	Predicates1=[Predicate2|Predicates3],

%% remvdup([a,a,a,b,b,c],[],A)
%% A = [a, b, c].

remvdup([],A,A):-!.
remvdup(A,B,C):-
	A=[A1|As],
	delete(As,A1,B2),
	append(B,[A1],D),
	remvdup(B2,D,C).
