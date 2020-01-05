%% Test individual cases, Debug=trace=on or off, N=case number, Passed=output=result

cawptest1a(Debug,N,Passed) :-
	cawptest2a(N,Function,Rules,MaxLength,MaxPredicates,TotalVars,Numinputs, Numoutputs,Specifications,AlgDict,Program1),
	%%writeln1([cawptest2(N,Specifications,Program1)]),
	%%writeln1(caw00(Debug,Function,Rules,MaxLength,MaxPredicates,TotalVars,Numinputs, Numoutputs,Specifications,AlgDict,[],Program2)),

	(((caw000(Debug,Function,Rules,MaxLength,MaxPredicates,TotalVars,Numinputs, Numoutputs,Specifications,AlgDict,[],Program1)
	%%sort(Program1,ProgramA),
	%%sort(Program2,ProgramA)

	%%writeln(Program1),
	%%writeln(Program2),
	%%Program1=Program2
	))->(Passed=passed,writeln([cawptest,N,passed]));(Passed=failed,writeln([cawptest,N,failed]))),!.
	
cawptest2a(7,[add0,add0],[],[2,3],[1,1],%% it could be 5
[3,4], %% 4 not 3 because of bug
[1],[1],
[
[[[[[[v,a],[]]],[[[v,b],[]]],true]]],

[[[[[[v,a],[1,2,3]]],[[[v,b],[]]],true]]]
%%[[[[[v,a],[2]]],[[[v,b],[]]],true]], %% Needs to progress bottom up by writing base case first
%%[[[[[v,a],[1,2]]],[[[v,b],[]]],true]], %% add0 second clause has this not spec below
%%[[[[[v,a],[1,2]]],[[[v,b],[2]]],true]],
%%[[[[[v,a],[2]]],[[[v,b],[]]],true]]
%%[[[[[v,a],[1,2]]],[[[v,b],[]]],true]],
],
[ %% Algorithm dictionary
[[[n,add2],1,1],[[v,a],[v,b]],":-",
	[[[n,=],[[v,a],[]]],
	[[n,=],[[v,b],[]]]]],
[[[n,add3],1,1],[[v,a],[v,b]],":-",
	[[[n,tail],[[v,a],[v,b]]]]]
	
	 %% Result
/**[[[n,add1],1,1],[[v,a],[v,b]],":-",
	[[[n,add2],[[v,a],[v,c]]],
	[[n,=],[[v,c],[v,b]]]]]
**/
],
[ %% Result
%%[[n,add3],[[v,a],[v,b]],":-", %% swapped a3,a2
	%%[[[n,tail],[[v,a],[v,b]]]]],

/**[[n,add2],[[v,a],[v,b]],":-",
	[[[n,=],[[v,a],[]]],
	[[n,=],[[v,b],[]]]]],
[[n,add0],[[v,a],[v,b]],":-", 
	[[[n,add2],[[v,a],[v,c]]],
	[[n,=],[[v,c],[v,b]]]]]],
**/	
	 %% Resulting base case

	[[[n,add2],[[v,a],[v,b]],":-",
		[[[n,=],[[v,a],[]]],
		[[n,=],[[v,b],[]]]]],
	[[n,add0],[[v,a],[v,b]],":-",
		[[[n,add2],[[v,a],[v,c]]],
		[[n,=],[[v,c],[v,b]]]]]],
	
	[ %% Resulting recursive algorithm
	
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
	[[n,=],[[v,d],[v,b]]]]]]]
	
	).


%%[[[n,add],[[v,a],[v,c],[v,d]],":-",[[[n,[]],[[v,a]]],[[n,=],[[v,c],[v,d]]]]]]

%% Add cover all vars before returning
%%**/


/** Doesn't work
%% before now, io=21
cawptest2(7,add0,[],2,3,5,% 3 x 5
[1,2],[0,1],
[[[[[[v,a],1],[[v,b],2]],[],true],[[[[v,a],2],[[v,b],1]],[],true]],
%%[[[[[v,a],1],[[v,b],2]],[],true],[[[[v,a],2],[[v,b],1]],[],true]],
[[[[[v,a],1]],[[[v,b],2]],true]],
[[[[[v,a],2]],[[[v,b],1]],true]]
],

[ %% Algorithm dictionary

        [[[n,a2],1,1],[[v,a],[v,b]],":-",
        [       [[n,+],[[v,a],1,[v,c]]],
                [[n,=],[[v,c],[v,b]]]]],

        [[[n,a3],1,1],[[v,a],[v,b]],":-",
        [       [[n,-],[[v,a],1,[v,c]]],
                [[n,=],[[v,c],[v,b]]]]]
],

[ %% Result
        [[n,a2],[[v,a],[v,b]],":-",
        [       [[n,+],[[v,a],1,[v,c]]],
                [[n,=],[[v,c],[v,b]]]]],

        [[n,a3],[[v,a],[v,b]],":-",
        [       [[n,-],[[v,a],1,[v,c]]],
                [[n,=],[[v,c],[v,b]]]]],

        [[n,add0],[[v,a],[v,b]],":-",
        [       [[n,1],[[v,a],[v,b]]]]],
        
        [[n,1],[[v,a],[v,b]],":-",
        [       [[n,a2],[[v,a],[v,c]]],
                [[n,=],[[v,c],[v,b]]]]],

        [[n,1],[[v,a],[v,b]],":-",
        [       [[n,a3],[[v,a],[v,c]]],
                [[n,=],[[v,c],[v,b]]]]]
]).

**/
