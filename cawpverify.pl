
%% cawptest(Debug[on/off],Total,Score).

cawptest(Debug,NTotal,Score) :- cawptest(Debug,0,NTotal,0,Score),!.
cawptest(_Debug,NTotal,NTotal,Score,Score) :- NTotal=3, !.
cawptest(Debug,NTotal1,NTotal2,Score1,Score2) :-
	NTotal3 is NTotal1+1,
	cawptest2(NTotal3,Function,Rules,MaxLength,MaxPredicates,TotalVars,Specifications,Program1),
	
	%%writeln([cawptest2(NTotal3,Specifications,Program1)]),
	(((%%writeln(caw00(Debug,function0,[],5,TotalVars,Specifications,[],Program1)),
	caw00(Debug,Function,Rules,MaxLength,MaxPredicates,TotalVars,Specifications,[],Program1)
	
	%%writeln(Program1),writeln(Program2),
	%%Program1=Program2
	))->(Score3 is Score1+1,writeln([cawptest,NTotal3,passed]));(Score3=Score1,writeln([cawptest,NTotal3,failed]))),
	writeln(""),
	cawptest(Debug,NTotal3,NTotal2,Score3,Score2),!.

%% Test individual cases, Debug=trace=on or off, N=case number, Passed=output=result

cawptest1(Debug,N,Passed) :-
	cawptest2(N,Function,Rules,MaxLength,MaxPredicates,TotalVars,Specifications,Program1),
	%%writeln([cawptest2(N,Specifications,Program1)]),

	(((caw00(Debug,Function,Rules,MaxLength,MaxPredicates,TotalVars,Specifications,[],Program1)
	%%writeln(Program1),writeln(Program2),
	%%Program1=Program2
	))->(Passed=passed,writeln([cawptest,N,passed]));(Passed=failed,writeln([cawptest,N,failed]))),!.


cawptest2(1,function3,[[[n,+],2,1]],4,1,5,[[[[[[v,a],1],[[v,b],1]],[[[v,c],3]],true]]],
[[[n,function3],[[v,a],[v,b],[v,c]],":-",[[[n,+],[[v,a],[v,a],[v,d]]],[[n,+],[[v,b],[v,d],[v,e]]],[[n,=],[[v,e],[v,c]]]]]]).

cawptest2(2,add,[[[n,[]],1,0],[[n,"_"],1,0]],2,1,3,[[[[[[v,a],[]],[[v,c],[1,2]]],[[[v,d],[1,2]]],true]]],

[[[n,add],[[v,a],[v,c],[v,d]],":-",[[[n,[]],[[v,a]]],[[n,=],[[v,c],[v,d]]]]]]
).

cawptest2(3,add,[[[n,[]],1,0],[[n,"_"],1,0]],3,1,4,[[[[[[v,a],[]],[[v,b],1],[[v,c],[1,2]]],[[[v,d],[1,2]]],true]]],

[[[n,add],[[v,a],[v,b],[v,c],[v,d]],":-",[[[n,[]],[[v,a]]],[[n,"_"],[[v,a]]],[[n,"_"],[[v,b]]],[[n,=],[[v,c],[v,d]]]]]]).
