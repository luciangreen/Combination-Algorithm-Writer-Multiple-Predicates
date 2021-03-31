
%% cawptest(Debug[on/off],Total,Score).

cawptest(Debug,NTotal,Score) :- cawptest(Debug,0,NTotal,0,Score),!.
cawptest(_Debug,NTotal,NTotal,Score,Score) :- NTotal=9, !.
cawptest(Debug,NTotal1,NTotal2,Score1,Score2) :-
	NTotal3 is NTotal1+1,
	cawptest2(NTotal3,Function,Rules,MaxLength,MaxPredicates,TotalVars,Numinputs,Numoutputs,Specifications,AlgDict,Program1),
	
	%%writeln([cawptest2(NTotal3,Specifications,Program1)]),
	(((%%writeln(caw00(Debug,function0,[],5,TotalVars,Specifications,[],Program1)),
	caw00(Debug,Function,Rules,MaxLength,MaxPredicates,TotalVars,Numinputs, Numoutputs,Specifications,AlgDict,[],Program1)
	
	%%sort(Program1,ProgramA),
	%%sort(Program2,ProgramA)
	%%writeln1(Program1),writeln1(Program2)
	%%Program1=Program2
	))->(Score3 is Score1+1,writeln([cawptest,NTotal3,passed]));(Score3=Score1,writeln([cawptest,NTotal3,failed]))),
	writeln(""),
	cawptest(Debug,NTotal3,NTotal2,Score3,Score2),!.

%% Test individual cases, Debug=trace=on or off, N=case number, Passed=output=result

cawptest1(Debug,N,Passed) :-
	cawptest2(N,Function,Rules,MaxLength,MaxPredicates,TotalVars,Numinputs, Numoutputs,Specifications,AlgDict,Program1),
	%%writeln1([cawptest2(N,Specifications,Program1)]),
	%%writeln1(caw00(Debug,Function,Rules,MaxLength,MaxPredicates,TotalVars,Numinputs, Numoutputs,Specifications,AlgDict,[],Program2)),

	(((caw00(Debug,Function,Rules,MaxLength,MaxPredicates,TotalVars,Numinputs, Numoutputs,Specifications,AlgDict,[],Program1)
	%%sort(Program1,ProgramA),
	%%sort(Program2,ProgramA)

	%writeln(Program1),
	%writeln(Program2),
	%Program1=Program2
	))->(Passed=passed,writeln([cawptest,N,passed]));(Passed=failed,writeln([cawptest,N,failed]))),!.


/**
cawptest2(1,add0,[[[n,+],2,1%% Modes=2 inputs, 1 output
]],2,2,%% MaxPredicates is not the number of predicates in the result, it is the number of non-dictionary predicates in the result.
4,
[2],[1],%% Numinputs, Numoutputs tested for
[
    [[[[[v,a],1],[[v,b],1]],[[[v,c],2]],true]],
    [[[[[v,a],1],[[v,b],1]],[[[v,c],2]],true]]
]
,
[], %% Algorithm dictionary
[ %% Result
        [[n,1],[[v,a],[v,b],[v,c]],":-",
        [       [[n,+],[[v,a],[v,b],[v,d]]],
                [[n,=],[[v,d],[v,c]]]]],
        [[n,add0],[[v,a],[v,b],[v,c]],":-",
        [       [[n,1],[[v,a],[v,b],[v,d]]],
                [[n,=],[[v,d],[v,c]]]]]
]).
**/
cawptest2(1,add0,[[[n,+],2,1%% Modes=2 inputs, 1 output
]],2,2,%% MaxPredicates is not the number of predicates in the result, it is the number of non-dictionary predicates in the result.
4,
[2],[1],%% Numinputs, Numoutputs tested for
[
    [[[[[v,a],1],[[v,b],1]],[[[v,c],2]],true]],
    [[[[[v,a],2],[[v,b],1]],[[[v,c],3]],true]]
]
,
[], %% Algorithm dictionary
[ %% Result
        [[n,1],[[v,a],[v,b],[v,c]],":-",
        [       [[n,+],[[v,a],[v,b],[v,d]]],
                [[n,=],[[v,d],[v,c]]]]],
        [[n,add0],[[v,a],[v,b],[v,c]],":-",
        [       [[n,1],[[v,a],[v,b],[v,d]]],
                [[n,=],[[v,d],[v,c]]]]]
]).

%% With a=input and b=input, returns [[[n,1],[[v,a],[v,b]],:-,[[[n,+],[[v,a],1,[v,c]]],[[n,=],[[v,c],[v,b]]]]],[[n,1],[[v,a],[v,b]],:-,[[[n,-],[[v,a],1,[v,c]]],[[n,=],[[v,c],[v,b]]]]],[[n,add0],[[v,a],[v,b]],:-,[[[n,1],[[v,b],[v,c]]],[[n,=],[[v,a],[v,a]]]]]] which is incorrect, and with a=input and b=output nondeterministic clauses fail
%% Non-determinism is not supported in List Prolog.  List Prolog should use if-then instead of non-deterministic clauses.
%% Use if-then with calls to predicates as consequents.
cawptest2(2,add0,[],2,1,3,[1,2],[0,1],
[[[[[[v,a],1],[[v,b],2]],[],true],[[[[v,a],2],[[v,b],1]],[],true]]],

[ %% Algorithm dictionary
        [[[n,1],1,1],[[v,a],[v,b]],":-",
        [       [[n,+],[[v,a],1,[v,c]]],
                [[n,=],[[v,c],[v,b]]]]],

        [[[n,1],1,1],[[v,a],[v,b]],":-",
        [       [[n,-],[[v,a],1,[v,c]]],
                [[n,=],[[v,c],[v,b]]]]]
],

 %% Result

[[[n,1],[[v,a],[v,b]],":-",
	[[[n,+],[[v,a],1,[v,c]]],
	[[n,=],[[v,c],[v,b]]]]],
[[n,1],[[v,a],[v,b]],":-",
	[[[n,-],[[v,a],1,[v,c]]],
	[[n,=],[[v,c],[v,b]]]]],
[[n,add0],[[v,a],[v,b]],":-",
	[[[n,1],[[v,a],[v,c]]],
	[[n,=],[[v,b],[v,c]]]]]]

).

%%[[[n,a2],[[v,a],[v,b]],:-,[[[n,+],[[v,a],1,[v,c]]],[[n,=],[[v,c],[v,b]]]]],[[n,add0],[[v,a],[v,b]],:-,[[[n,a2],[[v,a],[v,c]]]]]]


%% () Can underscore vars in spec, ignore in choose var, everyvarcovered

%% Was test 9 on bu16

cawptest2(3,add,[[[n,[]],1,0],[[n,"_"],1,0]],1,1,1,[1],[0],
[
/**[[[[[v,a],[1,2,3]],[[v,b],3],[[v,c],[]]],
	[[[v,d],[4,5,6]],[[v,e],[2,3]]],true],**/
	[[[[[v,a],3]],
	[],true]]

	/**
[[[[[[v,a],[1,2,3]],[[v,b],3],[[v,c],[]]],
	[[[v,d],[4,5,6]],[[v,e],[2,3]]],true],
[[[[v,a],[]],[[v,b],3],[[v,c],[4,5,6]]],
	[[[v,d],[4,5,6]],[[v,e],5]],true]]]],
**/
],
[ %% Algorithm dictionary
],
%% Result

	[[[n,add],[[v,a]],":-",[[[n,"_"],[[v,a]]]]]]

	/**
[[n,1],[[v,a],[v,b],[v,c],[v,d]],":-", %% Test by self
	[[[n,[]],[[v,c]]]]],
[[n,add],[[v,a],[v,b],[v,c],[v,d]],":-",
	[[n,1],[[v,a],[v,b],[v,c],[v,d]]]]**/
).



cawptest2(4,function3,[[[n,+],2,1]],3,1,5,[2],[1],
[[[[[[v,a],1],[[v,b],1]],[[[v,c],3]],true],
[[[[v,a],1],[[v,b],2]],[[[v,c],5]],true]]],
[ %% Algorithm dictionary
],
[ %% Result
[[n,function3],[[v,a],[v,b],[v,c]],":-",
	[[[n,+],[[v,a],[v,b],[v,d]]],
	[[n,+],[[v,b],[v,d],[v,e]]],
	[[n,=],[[v,e],[v,c]]]]]]).


cawptest2(5,add0,[[[n,+],2,1]],1,1,4,% 3 x 5 %% 2,2,3
[2],[1],
[
    [[[[[v,a],1],[[v,b],1]],[[[v,c],2]],true]]
%%  [[[[[v,a],1]],[[[v,b],2]],true]],
%%  [[[[[v,a],1],[[v,b],1]],[[v,c],2],true]]
]
,
[ %% Algorithm dictionary
],
[ %% Result
        [[n,add0],[[v,a],[v,b],[v,c]],":-",
        [       [[n,+],[[v,a],[v,b],[v,d]]],
                [[n,=],[[v,d],[v,c]]]]]

]).

%%%%*****


cawptest2(6,function3,[],2,1,4,[2],[1],
[[[[[[v,a],1],[[v,b],1]],[[[v,c],2]],true],
	[[[[v,a],1],[[v,b],2]],[[[v,c],3]],true],
[[[[v,a],1],[[v,b],1]],[[[v,c],1]],fail],
	[[[[v,a],1],[[v,b],1]],[[[v,c],3]],fail]]],
[ %% Algorithm dictionary

	[[[n,function1],2,1],[[v,a],[v,b],[v,c]],":-", 
	[
		[[n,+],[[v,a],[v,b],[v,c]]]
	]
	]

],
[ %% Result
[[n,function1],[[v,a],[v,b],[v,c]],":-",
	[[[n,+],[[v,a],[v,b],[v,c]]]]],
[[n,function3],[[v,a],[v,b],[v,c]],":-",
	[[[n,function1],[[v,a],[v,b],[v,d]]],
	[[n,=],[[v,d],[v,c]]]]]]).

/**
%%**
cawptest2(7,add0,[],3,3,%% it could be 5
4,
[1],[1],
[
[[[[[v,a],[1,2]]],[[[v,b],[]]],true]],
[[[[[v,a],[1,2]]],[[[v,b],[]]],true]],
[[[[[v,a],[1,2]]],[[[v,b],[2]]],true]],
[[[[[v,a],[2]]],[[[v,b],[]]],true]],
[[[[[v,a],[]]],[[[v,b],[]]],true]]
],
[ %% Algorithm dictionary
[[[n,add2],1,1],[[v,a],[v,b]],":-",
	[[[n,=],[[v,a],[]]],
	[[n,=],[[v,b],[]]]]],
[[[n,add3],1,1],[[v,a],[v,b]],":-",
	[[[n,tail],[[v,a],[v,b]]]]]
],
[ %% Result
[[n,add2],[[v,a],[v,b]],":-",
	[[[n,=],[[v,a],[]]],
	[[n,=],[[v,b],[]]]]],
[[n,add3],[[v,a],[v,b]],":-",
	[[[n,tail],[[v,a],[v,b]]]]],
[[n,add0],[[v,a],[v,b]],":-",
	[[[n,1],[[v,a],[v,c]]],
	[[n,=],[[v,c],[v,b]]]]],
[[n,1],[[v,a],[v,b]],":-",
	[[[n,add2],[[v,a],[v,c]]],
	[[n,=],[[v,c],[v,b]]]]],
[[n,1],[[v,a],[v,b]],":-",
	[[[n,add3],[[v,a],[v,c]]],
	[[n,1],[[v,c],[v,d]]],
	[[n,=],[[v,d],[v,b]]]]]]).

cawptest2(7.1,add0,[],2,1,%% it could be 5
3,
[1],[1],
[
%%[[[[[v,a],[1,2]]],[[[v,b],[]]],true]],
%%[[[[[v,a],[1,2]]],[[[v,b],[]]],true]],
%%[[[[[v,a],[1,2]]],[[[v,b],[2]]],true]],
%%[[[[[v,a],[2]]],[[[v,b],[]]],true]],
[[[[[v,a],[]]],[[[v,b],[]]],true]]
],
[ %% Algorithm dictionary
[[[n,add2],1,1],[[v,a],[v,b]],":-",
	[[[n,=],[[v,a],[]]],
	[[n,=],[[v,b],[]]]]]
%%[[[n,add3],1,1],[[v,a],[v,b]],":-",
%%	[[[n,tail],[[v,a],[v,b]]]]]
],
[ %% Result
[[n,add2],[[v,a],[v,b]],":-",
	[[[n,=],[[v,a],[]]],
	[[n,=],[[v,b],[]]]]],
[[n,add0],[[v,a],[v,b]],":-",
	[[[n,add2],[[v,a],[v,c]]],
	[[n,=],[[v,c],[v,b]]]]]]
).

cawptest2(7.2,add0,[],3,2,%% it could be 5
3,
[1],[1],
[
[[[[[v,a],[1,2]]],[[[v,b],[]]],true]],
[[[[[v,a],[1,2]]],[[[v,b],[2]]],true]],
[[[[[v,a],[2]]],[[[v,b],[]]],true]],
[[[[[v,a],[]]],[[[v,b],[]]],true]]
],
[ %% Algorithm dictionary
[[[n,add2],1,1],[[v,a],[v,b]],":-",
	[[[n,=],[[v,a],[]]],
	[[n,=],[[v,b],[]]]]],
[[[n,add3],1,1],[[v,a],[v,b]],":-",
	[[[n,tail],[[v,a],[v,b]]]]]
],
[ %% Result
[[n,add2],[[v,a],[v,b]],":-",
	[[[n,=],[[v,a],[]]],
	[[n,=],[[v,b],[]]]]],
[[n,add3],[[v,a],[v,b]],":-",
	[[[n,tail],[[v,a],[v,b]]]]],
[[n,add0],[[v,a],[v,b]],":-",
	[[[n,add2],[[v,a],[v,c]]],
	[[n,=],[[v,c],[v,b]]]]],
[[n,add0],[[v,a],[v,b]],":-",
	[[[n,add3],[[v,a],[v,c]]],
	[[n,add0],[[v,c],[v,d]]],
	[[n,=],[[v,d],[v,b]]]]]]).
	**/

cawptest2(7,add0,[[[n,+],2,1%% Modes=2 inputs, 1 output
],[[n,-],2,1]],4,1,%% MaxPredicates is not the number of predicates in the result, it is the number of non-dictionary predicates in the result.
6,
[2],[1],%% Numinputs, Numoutputs tested for
[
    [[[[[v,a],1],[[v,b],1]],[[[v,c],0],[[v,d],2]],true],
    [[[[v,a],1],[[v,b],2]],[[[v,c],1],[[v,d],3]],true]]
]
,
[], %% Algorithm dictionary
/**[ %% Result
        [[n,add0],[[v,a],[v,b],[v,c],[v,d]],":-",
        [       [[n,+],[[v,a],[v,b],[v,e]]],
                [[n,+],[[v,a],[v,b],[v,f]]],
                [[n,=],[[v,c],[v,e]]],
                [[n,=],[[v,f],[v,d]]]]]**/
                
       /**
        [[[n,add0],[[v,a],[v,b],[v,c],[v,d]],":-",
                [[[n,+],[[v,a],[v,a],[v,e]]],
                [[n,+],[[v,a],[v,b],[v,f]]],
                [[n,=],[[v,e],[v,c]]],
                [[n,=],[[v,e],[v,d]]]]]
                **/
                
               /**
                [[[n,add0],[[v,a],[v,b],[v,c],[v,d]],":-",
                	[[[n,+],[[v,a],[v,b],[v,e]]],
                	[[n,-],[[v,b],[v,a],[v,f]]],
                	[[n,=],[[v,e],[v,d]]],
                	[[n,=],[[v,f],[v,c]]]]]]
**/

[[[n,add0],[[v,a],[v,b],[v,c],[v,d]],":-",
	[[[n,+],[[v,a],[v,b],[v,e]]],
	[[n,-],[[v,b],[v,a],[v,f]]],
	[[n,=],[[v,e],[v,d]]],
	[[n,=],[[v,f],[v,c]]]]]]

).

/**
 Not tested nonrecursive multiclauses
cawptest2(8,add0,[[[n,+],2,1],[[n,-],2,1]],2,3,4,[2],[1],
[[[[[[v,a],1],[[v,b],1]],[[[v,b],2]],true],[[[[v,a],2],[[v,b],1]],[[[v,b],1]],true]],
[[[[[v,a],1],[[v,b],1]],[[[v,b],2]],true]],
[[[[[v,a],2],[[v,b],1]],[[[v,b],1]],true]]],

[ %% Algorithm dictionary
],

 %% Result

[[[n,1],[[v,a],[v,b],[v,c]],":-",
	[[[n,+],[[v,a],[v,b],[v,d]]],
	[[n,=],[[v,c],[v,d]]]]],
[[n,1],[[v,a],[v,b],[v,c]],":-",
	[[[n,-],[[v,a],[v,b],[v,d]]],
	[[n,=],[[v,d],[v,c]]]]],
[[n,add0],[[v,a],[v,b],[v,c]],":-",
	[[[n,1],[[v,a],[v,b],[v,d]]],
	[[n,=],[[v,c],[v,d]]]]]]
).
**/
 
/**

cawptest2(8,add0,[[[n,+],2,1%% Modes=2 inputs, 1 output
]],4,1,%% MaxPredicates is not the number of predicates in the result, it is the number of non-dictionary predicates in the result.
8,
[2],[1],%% Numinputs, Numoutputs tested for
[
    [[[[[v,a],1],[[v,b],1],[[v,c],2],[[v,d],1]],[[[v,e],5]],true],
    [[[[v,a],2],[[v,b],2],[[v,c],2],[[v,d],1]],[[[v,e],7]],true]]
]
,
[],
%% add options to remove extra choice points
%% test on lpiv
%% caw00(off,add0,[[[n,+],2,1]],4,1,8,[2,4],[1],[[[[[[v,a],1],[[v,b],1],[[v,c],2],[[v,d],1]],[[[v,e],5]],true],[[[[v,a],2],[[v,b],2],[[v,c],2],[[v,d],1]],[[[v,e],7]],true]]],[],[],P).
%% x:

[[[n,add0],[[v,a],[v,b],[v,c],[v,d],[v,e]],":-",[[[n,+],[[v,a],[v,b],[v,f]]],[[n,+],[[v,c],[v,f],[v,g]]],[[n,+],[[v,d],[v,g],[v,h]]],[[n,=],[[v,h],[v,e]]]]]]

).

**/

% ["Computational English","COMPUTATIONAL ENGLISH by Lucian Green Conglish Reflection 2 of 4.txt",0,algorithms,"14.   *I prepared to serve the vegetable burger.  I did this by cooking the vegetable patty.  First, I made the patty from semolina, soy and carrot.  Second, I minced it up.  Third, I cooked it.  In this way, I prepared to serve the vegetable burger by cooking the vegetable patty."]


cawptest2(8,append1,[[[n,append],2,1%% Modes=2 inputs, 1 output
]],3,1,%% MaxPredicates is not the number of predicates in the result, it is the number of non-dictionary predicates in the result.
7,
[3],[1],%% Numinputs, Numoutputs tested for
[
	[[[[[v,a],["top"]],[[v,b],["middle"]],[[v,c],["bottom"]]],
		[[[v,d],["top","middle","bottom"]]],true]]
]
,
[],
[[[n,append1],[[v,a],[v,b],[v,c],[v,d]],":-"	,[[[n,append],[[v,a],[v,b],[v,e]]],[[n,append],[[v,e],[v,c],[v,f]]],[[n,=],[[v,f],[v,d]]]]]]

).
    
% ["Fundamentals of Pedagogy and Pedagogy Indicators","FUNDAMENTALS OF PEDAGOGY by Lucian Green Two Uses 23 of 30.txt",0,algorithms,"225.	ALEXIS: *The subject should write logically connected breasonings."]

/**

cawptest2(9,mp*,[[[n,append],2,1%% Modes=2 inputs, 1 output
]],3,1,%% MaxPredicates is not the number of predicates in the result, it is the number of non-dictionary predicates in the result.
7,
[3],[1],%% Numinputs, Numoutputs tested for
[
	[[[[[v,a],["p1"]],[[v,b],["p1","p2"]]],
		[[[v,c],["p2"]]],true]]
]
,
[],
[[[n,mp],[[v,a],[v,b],[v,c],[v,d]],":-"	,[[[n,append],[[v,a],[v,b],[v,e]]],[[n,append],[[v,e],[v,c],[v,f]]],[[n,=],[[v,f],[v,d]]]]]]

).

**/
