
%% cawptest(Debug[on/off],Total,Score).

cawptest(Debug,NTotal,Score) :- cawptest(Debug,0,NTotal,0,Score),!.
cawptest(_Debug,NTotal,NTotal,Score,Score) :- NTotal=1, !.
cawptest(Debug,NTotal1,NTotal2,Score1,Score2) :-
	NTotal3 is NTotal1+1,
	cawptest2(NTotal3,Function,Rules,MaxLength,MaxPredicates,TotalVars,Specifications,Program1),
	
	%%writeln([cawptest2(NTotal3,Specifications,Program1)]),
	(((%%writeln(caw00(Debug,function0,[],5,TotalVars,Specifications,[],Program1)),
	caw00(Debug,Function,Rules,MaxLength,MaxPredicates,TotalVars,Specifications,[],Program1)
	
	%%sort(Program1,ProgramA),
	%%sort(Program2,ProgramA)
	%%writeln1(Program1),writeln1(Program2)
	%%Program1=Program2
	))->(Score3 is Score1+1,writeln([cawptest,NTotal3,passed]));(Score3=Score1,writeln([cawptest,NTotal3,failed]))),
	writeln(""),
	cawptest(Debug,NTotal3,NTotal2,Score3,Score2),!.

%% Test individual cases, Debug=trace=on or off, N=case number, Passed=output=result

cawptest1(Debug,N,Passed) :-
	cawptest2(N,Function,Rules,MaxLength,MaxPredicates,TotalVars,Specifications,Program1),
	%%writeln([cawptest2(N,Specifications,Program1)]),

	(((caw00(Debug,Function,Rules,MaxLength,MaxPredicates,TotalVars,Specifications,[],Program1)
	%%sort(Program1,ProgramA),
	%%sort(Program2,ProgramA)

	%%writeln(Program1),writeln(Program2)
	%%Program1=Program2
	))->(Passed=passed,writeln([cawptest,N,passed]));(Passed=failed,writeln([cawptest,N,failed]))),!.



cawptest2(1,add0,[[[n,+],2,1]],2,2,5,% 3 x 5 %% 2,2,3
[
    [[[[[v,a],1],[[v,b],1]],[[[v,c],2]],true]],
    [[[[[v,a],1],[[v,b],1]],[[[v,c],2]],true]]

%  [[[[[v,a],1]],[[[v,b],2]],true]],
%%  [[[[[v,a],1],[[v,b],1]],[[v,c],2],true]]
]
,
[       
/**
        [[n,1],[[v,a],[v,b]],":-",
        [       [[n,+],[[v,a],1,[v,c]]],
                [[n,=],[[v,c],[v,b]]]]],
        [[n,add0],[[v,a],[v,b]],":-",
        [       [[n,1],[[v,a],[v,b]]]]]        
**/
        [[n,1],[[v,a],[v,b],[v,c]],":-",
        [       [[n,+],[[v,a],[v,b],[v,d]]],
                [[n,=],[[v,d],[v,c]]]]],
        [[n,add0],[[v,a],[v,b],[v,c]],":-",
        [       [[n,1],[[v,a],[v,b],[v,d]]],
                [[n,=],[[v,d],[v,c]]]]]

/**
        [[n,add0],[[v,a],[v,b],[v,c]],":-",
        [       [[n,+],[[v,a],[v,b],[v,d]]],
                [[n,=],[[v,d],[v,c]]]]]
**/
]).
