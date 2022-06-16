/**cawptest2(3,add,[[[n,[]],1,0],[[n,"_"],1,0]],4,1,4,[1,2,3],[0,1],
[
[[[[[v,a],[]],[[v,b],3],[[v,c],[4,5,6]]],
	[[[v,d],[4,5,6]]],true]]
],
[ %% Algorithm dictionary
],
%% Result

	[[[n,add],[[v,a],[v,b],[v,c],[v,d]],":-",[[[n,[]],[[v,a]]],[[n,"_"],[[v,a]]],[[n,"_"],[[v,b]]],[[n,=],[[v,c],[v,d]]]]]]

).
**/

:- dynamic debug/1.
:- dynamic totalvars/1.
:- dynamic outputvars/1.
:- dynamic newrulenumber/1.
:- dynamic maxlength/1.
:- dynamic lastrule/1.
:- dynamic furthest_rule/1.
%%:- dynamic a/1.

/**

caw00(off,function3,[],5,7,[[[[a,1],[b,1]],[[c,2]],true],[[[a,1],[b,1]],[[c,2]],true],[[[a,1],[b,1]],[[c,1]],fail],[[[a,1],[b,1]],[[c,1]],fail]],[],Program),writeln(Program).

VarLists is in format list of [InputVarList,OutputVarList,Positivity], where these are specification lines that are either Positivity=true or fail

Notes:
- true specification line in first position of varlists, otherwise will try all except the specification line
- manually write a,a in algdict in a,b :- a,a,b because cawp would take too long finding all combinations including a,a
- give argument values in order 3 2 1 not 1 2 3 when adding, or will try 1+1+1 instead of 3 at first

**/

%%:- include('algdict.pl').
%%:- include('remove_duplicate_predicates.pl').

/**caw00a(Debug,PredicateName,Rules1,MaxLength,MaxPredicates,TotalVars,VarLists,Program1,Program2B) :-
	(caw00(Debug,PredicateName,Rules1,MaxLength,MaxPredicates,TotalVars,VarLists,Program1,Program2B),%%writeln("If error, returned true");(furthest_rule([Number,Rules]),
	writeln(["Error: No specification for predicate.  Programs so far:",Rules,"\n\nPredicate number:",Number]).
**/

caw000(Debug,PredicateName,Rules,MaxLength,MaxPredicates,TotalVars,NumInputs,NumOutputs,VarLists,AlgDict,Program1,Program2A) :-
	PredicateName=[PredicateName1],
	MaxLength=[MaxLength1],
	MaxPredicates=[MaxPredicates1],
	TotalVars=[TotalVars1],
	VarLists=[VarLists1],
	%%AlgDict=[AlgDict1],
	Program2A=[Program2A1],
	
caw00(Debug,PredicateName1,Rules,MaxLength1,MaxPredicates1,TotalVars1,NumInputs,NumOutputs,VarLists1,AlgDict,Program1,Program2A1),!.

caw000(Debug,PredicateName,Rules,MaxLength,MaxPredicates,TotalVars,NumInputs,NumOutputs,VarLists,AlgDict,Program1,Program2A) :-
	PredicateName=[PredicateName1|PredicateName2],
	MaxLength=[MaxLength1|MaxLength2],
	MaxPredicates=[MaxPredicates1|MaxPredicates2],
	TotalVars=[TotalVars1|TotalVars2],
	VarLists=[VarLists1|VarLists2],
	%%AlgDict=[AlgDict1|AlgDict2],
	Program2A=[Program2A1|Program2A2],
	
caw00(Debug,PredicateName1,Rules,MaxLength1,MaxPredicates1,TotalVars1,NumInputs,NumOutputs,VarLists1,AlgDict,Program1,Program2A1),
%%trace,

findall(A,(member(B,Program2A1),findall(K,(member(F,NumInputs),member(G,NumOutputs),B=[[n,C],Q,D,E],K=[[[n,C],F,G],Q,D,E]),L),member(A,L)),J),
append(AlgDict,J,Program2AA4),
%%Program2AA3=[Program2AA4],
remvdup(Program2AA4,[],Program2A3),
%%trace,
caw000(Debug,PredicateName2,Rules,MaxLength2,MaxPredicates2,TotalVars2,NumInputs,NumOutputs,VarLists2,Program2A3,[],Program2A2).

caw00(Debug,PredicateName,Rules1,MaxLength,MaxPredicates,TotalVars,NumInputs,NumOutputs,VarLists,AlgDict,Program1,Program2B) :-
	PredicatesA=AlgDict,
	%% remove duplicate predicates
	%%remvdup(PredicatesA0,[],PredicatesA),
	split3(PredicatesA,[],Rules2),
	split2(PredicatesA,[],Predicates),
	%%writeln([Rules2,Predicates]),
	append(Rules1,Rules2,Rules3),

	%%retractall(a(_)),

	retractall(debug(_)),
	assertz(debug(Debug)),
	retractall(totalvars(_)),
   assertz(totalvars(TotalVars)),
	retractall(maxlength(_)),
   assertz(maxlength(MaxLength)),
	retractall(lastrule(_)),
   assertz(lastrule([])),
	retractall(furthest_rule(_)),
   assertz(furthest_rule([0,[]])),
	%%retractall(newrulenumber(_)),
	%%assertz(newrulenumber(0)),
	retractall(numInputs(_)),
   assertz(numInputs(NumInputs)),
	retractall(numOutputs(_)),
   assertz(numOutputs(NumOutputs)),

	/**catch(call_with_time_limit(10, 
		**/caw01(VarLists,_,Predicates,PredicateName,Rules3,MaxLength,MaxPredicates,0,_,Program1,Program2B,_)
		
		%%writeln1(Program2B)
		/**),
      time_limit_exceeded,
      fail)
      **/
      .%%,not(length(Program2B,1)).

%%caw01([],_,_Predicates,_PredicateName,_Rules3,_MaxLength,_MaxPredicates,_New_rule_number1,_New_rule_number2,Program,Program) :- !. %% Recently added *****	

caw01([],[],_Predicates,_PredicateName,_Rules3,_MaxLength,_MaxPredicates,_New_rule_number1,_New_rule_number2,_Program1,_Program2,_) :- 
%%writeln(here1),
%%writeln1(["Error: No specification for predicate.  Program so far:",Program1,"\n\nPredicate number:",New_rule_number]),abort,!.
	fail,!.
caw01([[]],[[]],_Predicates,_PredicateName,_Rules3,_MaxLength,_MaxPredicates,_New_rule_number1,_New_rule_number2,Program,Program,_V3) :- %%writeln(here2),
fail.	
%%caw01(_VarLists,_Predicates,_PredicateName,_Rules3,_MaxLength,MaxPredicates,New_rule_number,Program,Program) :- New_rule_number=<MaxPredicates,!.	
caw01(VarLists,VarLists03,Predicates,PredicateName,Rules3,MaxLength,MaxPredicates,New_rule_number1,New_rule_number2,Program1,Program2B,_V) :-
%%writeln1(caw01(VarLists,_,Predicates,PredicateName,Rules3,MaxLength,MaxPredicates,New_rule_number1,New_rule_number2,Program1,Program2B)),
%%(Predicates=[[[n,add2],[[v,a],[v,b]],":-",[[[n,=],[[v,a],[]]],[[n,=],[[v,b],[]]]]],[[n,add3],[[v,a],[v,b]],":-",[[[n,tail],[[v,a],[v,b]]]]]]->trace;true),
	VarLists=[VarLists0|VarLists02],
	VarLists0=[VarLists1|VarLists2],
	
	%%retractall(varlists(_)),
	%%assertz(varlists(VarLists02)),

%%trace,
	%%writeln1(	findall(Program2A,caw0(Predicates,PredicateName,
	%%Rules3,MaxLength,MaxPredicates,
	%%VarLists1,VarLists02,New_rule_number1,New_rule_number2,Program1,Program2A),Program2)),
%%notrace,

	findall([Program2A,VarLists041,New_rule_number2A],caw0(Predicates,PredicateName,
	Rules3,MaxLength,MaxPredicates,
	VarLists1,VarLists02,VarLists041,New_rule_number1,New_rule_number2A,Program1,Program2A,_V),Program2VarLists04),

findall(Program21,(member(MemberProgram2VarLists04,Program2VarLists04),MemberProgram2VarLists04=[Program21,_,_]),Program2),

findall(VarLists041,(member(MemberProgram2VarLists041,Program2VarLists04),MemberProgram2VarLists041=[_,VarLists041,_]),VarLists04),

findall(New_rule_number2A1,(member(MemberProgram2VarLists041New_rule_number2A1,Program2VarLists04),MemberProgram2VarLists041New_rule_number2A1=[_,_,New_rule_number2A1]),New_rule_number2A2),

%%trace,

	%%writeln1(	findall(Program2A,caw0(Predicates,PredicateName,
	%%Rules3,MaxLength,MaxPredicates,
	%%VarLists1,VarLists02,New_rule_number1,New_rule_number2,Program1,Program2A),Program2)),
	%%notrace,
	length(Program2,Program2L),
	length(Program2LList,Program2L),
	append(Program2LList,_,[1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,41,42,43,44,45,46,47,48,49,50,51,52,53,54,55,56,57,58,59,60,61,62,63,64,65,66,67,68,69,70,71,72,73,74,75,76,77,78,79,80,81,82,83,84,85,86,87,88,89,90,91,92,93,94,95,96,97,98,99,100]),
	
	member(Program2LListItem,Program2LList),
	get_item_n(Program2,Program2LListItem,Program2B),
	get_item_n(VarLists04,Program2LListItem,VarLists03),
	get_item_n(New_rule_number2A2,Program2LListItem,New_rule_number2),
	%%member(Program2B,Program2), %e->1
	%%member(VarLists03,VarLists04), %e->1
%%writeln1(Program2B1),
%%writeln1(Program2B),
	%%Program2B1=[Program2B],
	
	%%trace,

	%%writeln1(member(Program2B,Program2)),
	%%notrace,

	
	aggregate_all(count,(member(Item,VarLists2),
	caw0(Predicates,PredicateName,Rules3,MaxLength,MaxPredicates,
	Item,VarLists03,_VarLists031,New_rule_number1,_New_rule_number2A3,Program1,Program2B,_V2)),Count1),
	
	%%trace,

	%%writeln1(	aggregate_all(count,(member(Item,VarLists2),
	%%caw0(Predicates,PredicateName,Rules3,MaxLength,MaxPredicates,
	%%Item,VarLists02,New_rule_number,Program1,Program2B)),Count)),
%%notrace,
%%writeln1(Program2),
%%trace,

%%writeln1(length(VarLists2,Count)),
%%notrace,

   %%(findall(EVM1,(everyvarmentioned(Vars2,Program5),

length(VarLists2,Count2),
Count1>=Count2,

%%(Program2B=[[[n,1],[[v,a],[v,b],[v,c]],":-",[[[n,+],[[v,a],[v,b],[v,d]]],[[n,=],[[v,d],[v,c]]]]]]->true%%trace
%%;true),
!.%%!.%%!. %%Predicates->PredicatesA x

get_item_n(List1,N1,Item) :-
	N2 is N1-1,
	length(List3,N2),
	append(List3,List4,List1),
	List4=[Item|_].
 caw0(Algorithms,PredicateName,Rules,MaxLength,MaxPredicates,VarLists,VarLists02,VarLists03,New_rule_number1,New_rule_number2,Program1,Program2,_V3) :-
	VarLists=[InputVarList,OutputVarList,Positivity],
	varnames(InputVarList,[],InputVars,[],InputValues),
	varnames(OutputVarList,[],OutputVars,[],_OutputValues),
	%%retractall(outputvars(_)),
   %%assertz(outputvars(OutputVars)),
	append(InputVars,OutputVars,Vars11),
%%Vars11=InputVars,
%%Vars12=InputVars,
	append(InputValues,OutputVars,Vars2),
	%%append(InputValues,OutputValues,Values),
	Query=[[n,PredicateName],Vars2],
	%%writeln(	caw(Algorithms,Query,PredicateName,Rules,MaxLength,MaxPredicates,Vars11,InputVars,InputVars,_,OutputVarList,OutputVars,Positivity,VarLists02,New_rule_number1,New_rule_number2,Program1,Program2)),
	caw(Algorithms,Query,PredicateName,Rules,MaxLength,MaxPredicates,Vars11,InputVars,InputVars,_,OutputVarList,OutputVars,Positivity,VarLists02,VarLists03,New_rule_number1,New_rule_number2,Program1,Program2,_V).
%%caw(_,_,_,_,_,N,_,_,_,_,_,_,_,_,_,_,N,P,P) :- fail,!. %% Turn off fail,! to have all solutions
	/**caw(_Algorithms,_Query,_PredicateName,_Rules,_MaxLength,MaxPredicates,_Vars11,_InputVars,_InputVarsa,_InputVars2,_OutputVarList,_OutputVars,_Positivity,_VarLists02,New_rule_number1,New_rule_number2,_Program1,_Program11):-
	%%writeln(caw_here(Algorithms,Query,PredicateName,Rules,MaxLength,MaxPredicates,Vars11,InputVars,InputVars,InputVars2,OutputVarList,OutputVars,Positivity,VarLists02,New_rule_number1,New_rule_number2,Program1,Program1)),
	%%MaxPredicates1 is MaxPredicates-1,
	writeln(["New_rule_number>MaxPredicates1",New_rule_number,">",MaxPredicates]),
	New_rule_number>MaxPredicates,
	fail,!.
**/
%%caw(_,_,_,_,_,_N,_,_,_,_,_,_,_,_,[],_,_N2?,P,P) :- !.
caw(_,_,_,_,0,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_F) :- fail, !. %% Turn off fail,! to have all solutions
caw(Algorithms1,Query,PredicateName,_Rules,_MaxLength,MaxPredicates,_VarList,InputVars1,InputVars2,_InputVarsa,VarLists,OutputVars,Positivity,VarLists02,VarLists02,New_rule_number1,New_rule_number1,Program1,Program2,_V) :-
%%writeln1(V),

	%%MaxLength>0, ***
	
%%MaxPredicates1 is MaxPredicates,
New_rule_number1=<MaxPredicates,
	
	%%trace,
	addrules(InputVars2,OutputVars,OutputVars,[],_PenultimateVars,[],Program3),%%notrace,
	
%%writeln([addrules(InputVars2,OutputVars,OutputVars,[],PenultimateVars,[],Program3)]),	
	%%optimise(Program1,InputVars1,_InputVars3,PenultimateVars,Program4), %% IV2->3
%%writeln([optimise(Program1,InputVars1,InputVars3,PenultimateVars,Program4)]),
	append(Program1,Program3,Program5),
	not(Program5=[]),
	
	append(InputVars1,OutputVars,Vars2),
	Program22=[
        [[n,PredicateName],Vars2,":-",
                Program5
        ]
        ],
	%%writeln1(interpret-short(Program22)),
   
   %%everyvarmentioned(Vars2,Program5),
   %%((PredicateName=1;PredicateName=2)->trace;true),
   /**
   not(Program22=[
        [[n,add],_,_,
                [[[n,+],_]|_]
        ]
        ]),
	**/
	%%(Program22=[[[n,1],[[v,a],[v,b],[v,c]],":-",[[[n,+],[[v,a],[v,b],[v,d]]],[[n,=],[[v,d],[v,c]]]]]],trace),
        
	eliminate_unused_predicates(Program22,Algorithms1,Algorithms2),
	%%writeln(eliminate_unused_predicates(Program22,Algorithms1,Algorithms2)),
	
	%%Algorithms2=[[[n,_],_,_,Body]|_],length(Body,1),
	%%(Program22=[[[n,function0],[[v,a],[v,b],[v,c]],":-",[[[n,function2],[[v,a],[v,b],[v,d]]],[[n,=],[[v,c],[v,d]]]]]]->writeln(eliminate_unused_predicates(Program22,Algorithms1,Algorithms2));true),
	
	%%trace,
	%%writeln(["1*",append(Algorithms2,Program22,Program2)]), %% ***** swapped a2,p22
	%%[Program23]=Program22,
	%%not(member(Program23,Algorithms2)),
	append(Algorithms2,Program22,Program2), %% ***** swapped a2,p22
	%%remvdup(Program2a,[],Program2),
	%%Algorithms2=Program2,
	
	
	%%not(Program2=[[[n,add0],[[v,a],[v,b],[v,c]],":-",[[[n,+],[[v,a],[v,b],[v,d]]],[[n,=],[[v,d],[v,c]]]]]]),
%%(Program2=[[[n,1],[[v,a],[v,b],[v,c]],":-",[[[n,+],[[v,a],[v,b],[v,d]]],[[n,=],[[v,d],[v,c]]]]]]->trace;true),

	%%length(Program2,Program2L),not(Program2L=1),
	%%not(New_rule_number=1),
	%%=<MaxPredicates,
%%([off,[[n,add0],[[1,2,3],[v,b]]],[[[n,add2],[[v,a],[v,b]],":-",[[[n,=],[[v,a],[]]],[[n,=],[[v,b],[]]]]],[[n,add0],[[v,a],[v,b]],":-",[[[n,add2],[[v,a],[v,c]]],[[n,add2],[[v,a],[v,d]]],[[n,=],[[v,d],[v,b]]]]]],[[[[v,b],[]]]]]=[Debug,Query,Program2,[VarLists]]->trace;true),

	%%append_last_rule(Program2),

	/**
	length(Program2,Program2L),(Program2L>=2->(%%writeln(here2),trace
	true);true),
	**/
	debug(Debug),
	
	%%([off,[[n,add0],[[1,2,3],[v,b]]],[[[n,add2],[[v,a],[v,b]],":-",[[[n,=],[[v,a],[]]],[[n,=],[[v,b],[]]]]],[[n,add0],[[v,a],[v,b]],":-",[[[n,add2],[[v,a],[v,c]]],[[n,add0],[[v,a],[v,d]]],[[n,=],[[v,d],[v,b]]]]]],[[[[v,b],[]]]]]=[Debug,Query,Program2,[VarLists]]->%%true
	%%trace
	%%;true),

%%([off,[[n,function3],[1,1,[v,c]]],[[[n,function1],[[v,a],[v,b],[v,c]],":-",[[[n,+],[[v,a],[v,b],[v,c]]]]],[[n,function3],[[v,a],[v,b],[v,c]],":-",[[[n,function1],[[v,a],[v,b],[v,d]]],[[n,=],[[v,d],[v,c]]]]]],[[[[v,c],2]]]]=[Debug,Query,Program2,[VarLists]]->true
	%%trace
	%%;true),

%%writeln1([V,"\n",Program2]),
%%writeln1(V),
%%writeln1([program2,Program2]),
%%writeln1([v,V]),

%% add 0s
%%(var(V)->true;(member([[n,add0],[[v,a],[v,b]],":-",[[[n,add3],[[v,a],[v,c]]],[[n,add0],[[v,c],[v,d]]],[[n,=],[[v,d],[v,b]]]]],V)->writeln1("v yes");true)),
%%**()(V=[[[n,add3],[[v,a],[v,b]],":-",[[[n,tail],[[v,a],[v,b]]]]],[[n,add0],[[v,a],[v,b]],":-",[[[n,add3],[[v,a],[v,c]]],[[n,add0],[[v,c],[v,d]]],[[n,=],[[v,d],[v,b]]]]]]->writeln1("v yes");true),

%%(var(Program2)->true;(member([[n,add0],[[v,a],[v,b]],":-",[[[n,add2],[[v,a],[v,c]]],[[n,=],[[v,c],[v,b]]]]],Program2)->writeln1("p yes");true)),
%%**()(Program2=[[[n,add2],[[v,a],[v,b]],":-",[[[n,=],[[v,a],[]]],[[n,=],[[v,b],[]]]]],[[n,add0],[[v,a],[v,b]],":-",[[[n,add2],[[v,a],[v,c]]],[[n,=],[[v,c],[v,b]]]]]]->writeln1("p yes");true),

%%vyes(Program2),

%%(((var(V)->true;(member([[n,add0],[[v,a],[v,b]],":-",[[[n,add3],[[v,a],[v,c]]],[[n,add0],[[v,c],[v,d]]],[[n,=],[[v,d],[v,b]]]]],V))),(var(Program2)->true;(member([[n,add0],[[v,a],[v,b]],":-",[[[n,add2],[[v,a],[v,c]]],[[n,=],[[v,c],[v,b]]]]],Program2))))->trace;true),

%%**()((not(var(V)),V=[[[n,add3],[[v,a],[v,b]],":-",[[[n,tail],[[v,a],[v,b]]]]],[[n,add0],[[v,a],[v,b]],":-",[[[n,add3],[[v,a],[v,c]]],[[n,add0],[[v,c],[v,d]]],[[n,=],[[v,d],[v,b]]]]]],
%%Program2=[[[n,add2],[[v,a],[v,b]],":-",[[[n,=],[[v,a],[]]],[[n,=],[[v,b],[]]]]],[[n,add0],[[v,a],[v,b]],":-",[[[n,add2],[[v,a],[v,c]]],[[n,=],[[v,c],[v,b]]]]]])->trace;true),
	%%trace;
	%%true),

	%%writeln([program2,Program2]),
	%%(length(Program5,4)->writeln([program2,Program2])),
	%%(Program2=[[[n,add0],[[v,a],[v,b],[v,c],[v,d],[v,e]],":-",[[[n,+],[[v,a],[v,b],[v,f]]],[[n,+],[[v,c],[v,f],[v,g]]],[[n,+],[[v,d],[v,g],[v,h]]],[[n,=],[[v,h],[v,e]]]]]]->trace;true),
	%%writeln(["Press c."]),(get_single_char(97)->true;true),
	
	 %%writeln1([interpret(Debug,Query,Program2,OutputVarList)]),
	
 %writeln1(interpret(Debug,Query,Program2,VarLists)),
	
	
%%([Debug,Query,Program2,[VarLists]]=[off,[[n,add],[[],[1,2],[v,d]]],[[[n,add],[[v,a],[v,c],[v,d]],":-",[[[n,[]],[[v,a]]],[[n,=],[[v,c],[v,d]]]]]],[[[[v,d],[1,2]]]]]->trace;true),

%%([Debug,Query,Program2,[VarLists]]=[off,[[n,add0],[[1,2,3],[v,b]]],[[[n,add2],[[v,a],[v,b]],":-",[[[n,=],[[v,a],[]]],[[n,=],[[v,b],[]]]]],[[n,add0],[[v,a],[v,b]],":-",[[[n,add2],[[v,a],[v,c]]],[[n,add2],[[v,a],[v,d]]],[[n,=],[[v,d],[v,b]]]]]],[[[[v,b],[]]]]]->trace;true),

%%(Program2=[[[n,add2],[[v,a],[v,b]],":-",[[[n,=],[[v,a],[]]],[[n,=],[[v,b],[]]]]],[[n,add0],[[v,a],[v,b]],":-",[[[n,add2],[[v,a],[v,c]]],[[n,add2],[[v,a],[v,d]]],[[n,=],[[v,d],[v,b]]]]]]->trace;true),
	%%interpret(Debug,Query,Program2,OutputVarList).
	%%aggregate_all(count,(member(Item,VarLists),
	%%varlists(VarLists02),
	%%(
	
	%%notrace,
	try_interpret(Positivity,Debug,Query,Program2,VarLists),%%trace,%%writeln(here),
	%%trace,
		%%trace,
	add_furthest_rule1(New_rule_number1,Program2),
	%%(Program2=[[[n,add],[[v,a],[v,b],[v,c],[v,d]],":-",[[[n,=],[[v,c],[v,d]]]]]]->true%%trace
	%%;true),
	%%trace,
	(no_singletons(Vars2,Program5)),
	
	%%writeln1(Program2),	
	%%writeln1([cawptest,passed,Program2]),abort,
	%%->true;(%%notrace,fail)),
 %%writeln1(interpret(Debug,Query,Program2,[VarLists])),

%%(furthest_rule(A)->writeln(furthest_rule(A));true),%%notrace,
	%%!.
	!.%%!.
	%%-> %% *** [VarLists] ?
	/**(VarLists03=VarLists02,
	%%retractall(varlists(_)),
	%%assertz(varlists(VarLists03)),
	Program2c=Program2);%%fail%%
	(Program2c=[],VarLists03=VarLists02,%%append(VarLists,VarLists02,VarLists03),
	retractall(varlists(_)),
	assertz(varlists(VarLists03)),fail
	)
	),!.%%),Count), **** [VarLists02] to VarLists02
	%%length(OutputVarList,Count),!.
	**/
	/**
	
	vyes(P):-%%((not(var(V)),
	(member([[n,add0],[[v,a],[v,b]],":-",[[[n,add3],[[v,a],[v,c]]],[[n,add0],[[v,c],[v,d]]],[[n,=],[[v,d],[v,b]]]]],P),writeln("pyes*****")).%%,(var(Program2)->true;(member([[n,add0],[[v,a],[v,b]],":-",[[[n,add2],[[v,a],[v,c]]],[[n,=],[[v,c],[v,b]]]]],Program2)))).
	vyes(V):-%%((not(var(V)),
	(member([[n,add0],[[v,a],[v,b]],":-",[[[n,add2],[[v,a],[v,c]]],[[n,=],[[v,c],[v,b]]]]],V)),writeln("vyes*****")%%,(var(Program2)->true;(member([],Program2)))).
.
	vyes(V):-
true.
**/
caw(Algorithms,Query,PredicateName,Rules,MaxLength,MaxPredicates,VarList,InputVars1,InputVars2,InputVars3,VarLists,OutputVars,Positivity,VarLists02,VarLists03,New_rule_number1,New_rule_number2,Program1,Program4,_V) :-
%%writeln(here4),
%%trace,

%%writeln1(caw(Algorithms,Query,PredicateName,Rules,MaxLength,MaxPredicates,VarList,InputVars1,InputVars2,InputVars3,VarLists,OutputVars,Positivity,VarLists02,New_rule_number1,New_rule_number2,Program1,Program4)),
%%trace,
%%writeln([caw(Query,PredicateName,Rules,MaxLength,MaxPredicates,VarList,InputVars1,InputVars2,OutputVarList,OutputVars,Program1,Program4)]),?
	MaxLength>=0, %%***
	MaxLength2 is MaxLength - 1,
	%%reverse(InputVars2,InputVars5),

%%writeln([new_rule_number,New_rule_number,maxPredicates,MaxPredicates]),
%%writeln(limit_reached(New_rule_number,MaxPredicates,Rules,PredicateName,InputVars1,OutputVars,Rules1)),
%%trace,
limit_reached(New_rule_number1,MaxPredicates,Rules,PredicateName,InputVars1,OutputVars,Rules1), %% *** Check these
%%writeln([rules1,Rules1]),
	%%repeat,
	%%writeln(limit_reached(New_rule_number,MaxPredicates,Rules,PredicateName,InputVars1,OutputVars,Rules1)),
%%get_char(_),
	%%trace,
	
	
	member([RuleName0,NumInputs0,NumOutputs0],Rules1),
	
	%%[RuleName0,NumInputs0,NumOutputs0]=[other_new_branch,_,_],
	
%%	**/
	%%RuleName0=newrule123,
%%writeln([member([RuleName,NumInputs,NumOutputs],Rules)]),
%%writeln([rule(RuleName,NumInputs,NumOutputs,VarList,VarList2,Rule)]),
	
	
	%%retractall(newrulenumber(_)),
	%%assertz(newrulenumber(Newrulenumber1)),

	%%[InputVars2,VarLists,Positivity]=[VarLists0311,VarLists0312,_VarLists0313], ***
	%% **** InputVars1 or InputVars2?
	%%length(VarLists0311,VarLists0311L),
	%%length(VarLists0312,VarLists0312L),
	
	%%***newbranchifcall(RuleName0,PredicateName,Itema),
	numInputs(NumInputs1a),numOutputs(NumOutputs1a),
	member(NumInputs1,NumInputs1a),%%[1,2,3]),%%,0,2,3]),	 %%*** 4	
	member(NumOutputs1,NumOutputs1a),%%[0,1]),%%,0,2,3]), %%***  4
	
	%%writeln([i,o,NumInputs1,NumOutputs1]),
	%%*** caw x when creates a new pred, num extra clauses 0-2, doesn't add rule to pred, returns vl03 for (rules in) this pred
	caw3(RuleName0,Algorithms,Algorithms2,NumInputs0,NumOutputs0,New_rule_number1,New_rule_number3,Rules,PredicateName,Program1,VarList,VarList2,InputVars2,InputVars4,OutputVars,Rule,NumInputs1,NumOutputs1,VarLists02,VarLists04,MaxPredicates,Rules2),
	
	
%%->true
%%;
 %% InputVars5->InputVars2
%%writeln([rule(RuleName,NumInputs,NumOutputs,InputVars1,InputVars3,VarList,VarList2,Rule)]),
	%%writeln(not(member(Rule,Program1))),
	not(member(Rule,Program1)), %% Need to stop repeats of arity 1 calls
	append(Program1,[Rule],Program3),
%%writeln([inputVars3,InputVars3]),
%%InputVars2=InputVars3,
%%writeln([program4,Program4]),
	
	%%retractall(varlists(_)),
	%%assertz(varlists(VarLists02)),
	%%writeln1(	caw(Algorithms2,Query,PredicateName,Rules2,MaxLength2,MaxPredicates,VarList2,InputVars1,InputVars4,InputVars3,VarLists,OutputVars,Positivity,VarLists02,New_rule_number3,New_rule_number2,Program3,Program4)),
	caw(Algorithms2,Query,PredicateName,Rules2,MaxLength2,MaxPredicates,VarList2,InputVars1,InputVars4,InputVars3,VarLists,OutputVars,Positivity,VarLists04,VarLists03,New_rule_number3,New_rule_number2,Program3,Program4,_Algorithms2).

caw3(RuleName0,Algorithms,Algorithms2,NumInputs0,NumOutputs0,New_rule_number,New_rule_number1,Rules,PredicateName,Program1,VarList,VarList2,InputVars2,InputVars4,OutputVars,Rule,_NumInputs1,_NumOutputs1,VarLists02,VarLists02,_MaxPredicates,Rules2) :-
	
	%% rules_existing
	not(RuleName0=predicatename_new_branch),not(RuleName0=other_new_branch),not(RuleName0=[rules_new_branch,_]),
	not(RuleName0=predicatename_existing),
		%%trace,
		find_rule_name(RuleName0,RuleName2),
	Algorithms2=Algorithms,%%RuleName=RuleName0,
NumInputs=NumInputs0,NumOutputs=NumOutputs0,New_rule_number1 = New_rule_number,Rules2=Rules,

rulename_if_limit(RuleName2,PredicateName,RuleName),
	rule(Program1,RuleName,NumInputs,NumOutputs,InputVars2,InputVars4,VarList,VarList2,OutputVars,Rule).
	
	
caw3(RuleName0,Algorithms,Algorithms2,NumInputs0,NumOutputs0,New_rule_number,New_rule_number1,Rules,PredicateName,Program1,VarList,VarList2,InputVars2,InputVars4,OutputVars,Rule,NumInputs1,NumOutputs1,VarLists02,VarLists03,MaxPredicates,Rules2) :-

%%Number_of_clauses2=1, %% Not tested nonrecursive multiclauses - use bottom up
member(Number_of_clauses1,[1]),%%,3,4]),
(Number_of_clauses1>=MaxPredicates->Number_of_clauses2=MaxPredicates;Number_of_clauses2=Number_of_clauses1),

create_multiple_nonrecursive_clauses(Number_of_clauses2,RuleName0,Algorithms,Algorithms2,NumInputs0,NumOutputs0,New_rule_number,New_rule_number1,Rules,PredicateName,Program1,VarList,VarList2,InputVars2,InputVars4,OutputVars,Rule,NumInputs1,NumOutputs1,VarLists02,VarLists03,MaxPredicates,Rules2).



create_multiple_nonrecursive_clauses(0,_RuleName0,Algorithms,Algorithms,_NumInputs0,_NumOutputs0,New_rule_number,New_rule_number,Rules,_PredicateName,_Program1,VarList,VarList,InputVars,InputVars,_OutputVars,_Rule,	_NumInputs1,_NumOutputs1,VarLists02,VarLists02,_MaxPredicates,Rules) :- 
!.

create_multiple_nonrecursive_clauses(Number_of_clauses1,RuleName0,Algorithms,Algorithms2,NumInputs0,NumOutputs0,New_rule_number1,New_rule_number3,Rules,PredicateName,Program1,VarList,VarList2,InputVars2,InputVars4,OutputVars,Rule,NumInputs1,NumOutputs1,VarLists02,VarLists03,MaxPredicates,Rules2) :-


caw4a(RuleName0,New_rule_number1,Rules,PredicateName,NumInputs0,NumOutputs0,NumInputs1,NumOutputs1,Rules3,New_rule_number4,RuleName),
%%writeln(before4b),
%%trace,
caw4b(VarLists02,VarLists04,Algorithms,New_rule_number5,MaxPredicates,New_rule_number4,Program1,RuleName,NumInputs1,NumOutputs1,InputVars2,InputVars5,VarList,VarList3,OutputVars,Rule,Algorithms3,Rules3),
%%trace,

Number_of_clauses2 is Number_of_clauses1-1,
create_multiple_nonrecursive_clauses(Number_of_clauses2,RuleName0,Algorithms3,Algorithms2,NumInputs0,NumOutputs0,New_rule_number5,New_rule_number3,Rules3,PredicateName,Program1,VarList3,VarList2,InputVars5,InputVars4,OutputVars,Rule,NumInputs1,NumOutputs1,VarLists04,VarLists03,MaxPredicates,Rules2).



caw4a(RuleName0,New_rule_number,Rules,_PredicateName,_NumInputs0,_NumOutputs0,NumInputs1,NumOutputs1,Rules2,New_rule_number1,RuleName) :-

	RuleName0=other_new_branch, %% Command from Rules with new branch

%%trace,
	New_rule_number1 is New_rule_number+1,
%%not(New_rule_number1=2), 

RuleName=[n,New_rule_number1],

	append(Rules,[[RuleName,NumInputs1,NumOutputs1]],Rules2)
.

caw4a(RuleName0,New_rule_number,Rules,PredicateName,NumInputs0,NumOutputs0,_NumInputs1,_NumOutputs1,Rules2,New_rule_number1,RuleName) :-
	RuleName0=predicatename_new_branch, %% New branch with same name as current predicate
RuleName=[n,PredicateName],
	New_rule_number1 is New_rule_number+1,
		append(Rules,[[RuleName,NumInputs0,NumOutputs0]],Rules2)

.

caw4a(RuleName0,New_rule_number,Rules,_PredicateName,NumInputs0,NumOutputs0,_NumInputs1,_NumOutputs1,Rules2,New_rule_number1,RuleName) :-
	RuleName0=[rules_new_branch,RuleName], %% New branch with same name as current predicate
%%trace,
%%RuleName=[n,RuleName1],
	New_rule_number1 is New_rule_number+1,
		append(Rules,[[RuleName,NumInputs0,NumOutputs0]],Rules2)
.

caw4a(RuleName0,New_rule_number,Rules,PredicateName,NumInputs0,NumOutputs0,_NumInputs1,_NumOutputs1,Rules2,New_rule_number1,RuleName) :-
	RuleName0=predicatename_existing,
		%%trace,
		find_rule_name(RuleName0,RuleName2),
	%%Algorithms2=Algorithms,
	%%RuleName=RuleName0, *** not this
%%NumInputs1=NumInputs0,NumOutputs1=NumOutputs0,%% n i,o epsilon to 1
New_rule_number1 = New_rule_number,%%Rules2=Rules,

rulename_if_limit(RuleName2,PredicateName,RuleName),
		append(Rules,[[RuleName,NumInputs0,NumOutputs0]],Rules2)
.
	%%rule(Program1,RuleName,NumInputs,NumOutputs,InputVars2,InputVars4,VarList,VarList2,OutputVars,Rule).


caw4b(VarLists02,VarLists03,Algorithms,New_rule_number2,MaxPredicates,New_rule_number1,Program1,RuleName,NumInputs1,NumOutputs1,InputVars2,InputVars4,VarList,VarList2,OutputVars,Rule,Algorithms2,Rules2) :-
	%%writeln1(append(Rules,[[RuleName,NumInputs0,NumOutputs0]],Rules2)),
	%%MaxPredicates2 is MaxPredicates-1,
	
	maxlength(MaxLength3),
	
%%writeln(    caw01(VarLists02,Algorithms,New_rule_number1,New_rule_number2,Rules2,MaxLength3,MaxPredicates,New_rule_number1,[],Program2)),
    %%writeln(here3),
    %%trace,
    %%writeln(    RuleName=[_,RuleName1]),
    RuleName=[_,RuleName1],
caw01(VarLists02,VarLists03,Algorithms,%%New_rule_number1
    RuleName1
,Rules2,MaxLength3,MaxPredicates,New_rule_number1,New_rule_number2,[],Program2,_Program12), %% *** VarLists02     
    %%writeln(here4),
%%to [VarLists02]
%%Program2=[[[[n,1],[[v,a],[v,b],[v,c]],":-",[[[n,+],[[v,a],[v,b],[v,d]]],[[n,=],[[v,d],[v,c]]]]]]],
%%writeln([caw01,Program2]),
%%trace,
rule(Program1,RuleName,NumInputs1,NumOutputs1,InputVars2,InputVars4,VarList,VarList2,OutputVars,Rule),
%%trace,
	%%writeln(["2*",append(Program2,Algorithms,Algorithms2)]), %% *** swapped a,p2
	%%[Program2a]=Program2,
	%%not(member(Program2a,Algorithms)),
	%append(Program2,Algorithms,Algorithms2) %% *** swapped a,p2
	Program2=Algorithms2
	.
	
find_rule_name(RuleName0,RuleName2) :-
	RuleName0=[_,RuleName1],RuleName2=RuleName1.
find_rule_name(RuleName0,RuleName2) :-
	not(RuleName0=[_,_RuleName1]),RuleName2=RuleName0.

try_interpret(Positivity,Debug,Query,Program2,VarLists) :-
	Positivity=true,catch(call_with_time_limit(0.05, 
		international_interpret([lang,"en"],Debug,Query,Program2,[VarLists])),
      time_limit_exceeded,
      fail),!.
      
try_interpret(Positivity,Debug,Query,Program2,VarLists) :-
	not(Positivity=true),catch(call_with_time_limit(0.05, 
		not(international_interpret([lang,"en"],Debug,Query,Program2,[VarLists]))),
      time_limit_exceeded,
      fail),!.

append_last_rule(Program2):-
	lastrule(LastRule1),
	append_last_rule1(Program2,LastRule1).
	
%%append_last_rule1(_Program2,LastRule1) :-
%%	LastRule1=[],!.
append_last_rule1(Program2,LastRule1) :-
%%	not(LastRule1=[]),
	not(member(Program2,LastRule1)),
	append(LastRule1,[Program2],LastRule2),
	retractall(lastrule(_)),
   assertz(lastrule(LastRule2)).

add_furthest_rule1(New_rule_number,Program1) :-
	furthest_rule(Rule),Rule=[Number,Rules],
	add_furthest_rule2(New_rule_number,Number,Rules,Program1).
/**add_furthest_rule2(New_rule_number,Number,_Rules,Program1) :-
	New_rule_number>Number,
	retractall(furthest_rule(_)),
   assertz(furthest_rule([New_rule_number,[Program1]])),!.
   **/
add_furthest_rule2(New_rule_number,_Number,Rules,Program1) :-
%%trace,
	%%New_rule_number=Number,
	retractall(furthest_rule(_)),
	delete(Rules,Program1,Rules1),
	append(Rules1,[Program1],Rules2),
   assertz(furthest_rule([New_rule_number,Rules2])).
/**add_furthest_rule2(New_rule_number,Number,_Rules,_Program1) :-
	New_rule_number<Number,!.
**/
rulename_if_limit(RuleName0,PredicateName,RuleName) :-
	RuleName0=predicatename_existing,RuleName=[n,PredicateName],!.
rulename_if_limit(RuleName0,_PredicateName,RuleName) :-
	not(RuleName0=predicatename_existing),RuleName=RuleName0.

/**
everyvarmentioned(Vars1,Program) :-
	everyvarmentioned1(Vars1,Program).
	%%underscore_occurs_once_per_var(Vars1,Program).

everyvarmentioned1([],_Program) :- !.
everyvarmentioned1(Vars1,Program) :-
	Vars1=[Vars2|Vars3],
	
	findall(Vars2,(member(C,Program),(
	%%(C=[[n,"_"],[[v,b]]]->(true);true),
	C=[_E,D],
	member(Vars2,D)
	

	%%Vars2=true
	)),B),not(B=[]),
	everyvarmentioned1(Vars3,Program).
**/

no_singletons(Vars1,Program):-
	findall(DA,(member(C,Program),C=[_E,D],member(DA,D)),Vars2),
	%%append_list(Vars2,Vars2A),
	append(Vars1,Vars2,Vars3),
	findall(Count1,(member(Item,Vars3),aggregate_all(count,(member(Item,Vars3)),Count1),
	Count1=1),G),G=[].



/**
underscore_occurs_once_per_var([],_,_Program) :- !.
underscore_occurs_once_per_var(Vars1,Program) :-
	
	findall(Var,(aggregate_all(count,(member(Var,Vars1),
	member(C,Program),C=[[n,"_"],[Var]]),Count),Count=<1),List),
	not(List=[]).
**/

limit_reached(New_rule_number,MaxPredicates,Rules0,PredicateName,InputVars1,OutputVars,Rules1) :-
	New_rule_number<MaxPredicates,
	length(InputVars1,InputVars1L),
	length(OutputVars,OutputVarsL),
	pred_already_in_list1(PredicateName,InputVars1L,OutputVarsL,Rules0,Rules),
	
append(Rules,[[predicatename_new_branch,InputVars1L,OutputVarsL], %% Uncommented for test 7, commented for test 7
[other_new_branch,_,_] %% commented for test 7
],Rules1),!.
limit_reached(New_rule_number,MaxPredicates,Rules0,PredicateName,InputVars1,OutputVars,Rules1) :-
	New_rule_number>=MaxPredicates,
	length(InputVars1,InputVars1L),
	length(OutputVars,OutputVarsL),
	pred_already_in_list1(PredicateName,InputVars1L,OutputVarsL,Rules0,Rules1).

	pred_already_in_list1(PredicateName,InputVars1L,OutputVarsL,Rules0,Rules) :-
	pred_already_in_list2(PredicateName,InputVars1L,OutputVarsL,Rules0,Rules).
	
		pred_already_in_list2(PredicateName,InputVars1L,OutputVarsL,Rules0,Rules) :-
	member([[n,PredicateName],InputVars1L,OutputVarsL],Rules0),
		apply_rules_existing_and_new_branch(Rules0,Rules),!.
			pred_already_in_list2(PredicateName,InputVars1L,OutputVarsL,Rules0,Rules) :-
	not(member([[n,PredicateName],InputVars1L,OutputVarsL],Rules0)),
	apply_rules_existing_and_new_branch(Rules0,Rules01),
	append(Rules01,[[predicatename_existing,InputVars1L,OutputVarsL]  %% Uncommented for test 7
	],Rules).

apply_rules_existing_and_new_branch(Rules1,Rules2) :-
apply_rules_existing_and_new_branch2(Rules1,Rules3),
apply_rules_existing_and_new_branch3(Rules1,Rules4),
append(Rules3,Rules4,Rules2).

apply_rules_existing_and_new_branch2(Rules1,Rules2) :-
	findall([A,C1,C2],(member(B,Rules1),B=[A1,C1,C2],member(D,[rules_existing
	]),A=[D,A1]),Rules2).
	
apply_rules_existing_and_new_branch3(Rules1,Rules2) :-
	findall([A,C1,C2],(member(B,Rules1),B=[A1,C1,C2],member(D,[%%rules_new_branch
	]),A=[D,A1]),Rules2).
	
%%append(Rules,[[predicatename_existing,InputVars1L,OutputVarsL]%%%%,[other_existing,_,_]
%%],Rules1),!.

/**
newbranchifcall(RuleName0,PredicateName,Itema):-
		RuleName0=[n,PredicateName],member(Itema,[useexisting,newbranch]).
newbranchifcall(RuleName0,PredicateName,Itema):-
		not(RuleName0=[n,PredicateName]),Itema=useexisting.
**/

varnames([],Vars,Vars,Values,Values) :- !.
varnames(VarList,Vars1,Vars2,Values1,Values2) :-
	VarList=[Var|Vars3],
	Var=[VarName,Value],
	append(Vars1,[VarName],Vars4),
	append(Values1,[Value],Values3),
	varnames(Vars3,Vars4,Vars2,Values3,Values2),!.

/**
%%addrules(_,_,[],PV,PV,Program,Program) :- !.
addrules(VarList,OutputVars1,OutputVars2,PenultimateVars1,PenultimateVars2,Program1,Program3) :-
writeln1(addrules(VarList,OutputVars1,OutputVars2,PenultimateVars1,PenultimateVars2,Program1,Program3)),
%%trace,
	OutputVars2=[],%%[OutputVar|OutputVars3],
	member(Var,VarList),
	%%member(OutputVar,OutputVars1),%%()
	append(VarList,OutputVars1,OutputVars4),
	member(Var2,OutputVars4),
	append(Program1,[[[n,=],[Var,Var2%%OutputVar
	]]],Program3),
	append(PenultimateVars1,[Var],PenultimateVars2).
	
	**/
	/**
	%%addrules(VarList,OutputVars1,OutputVars3,PenultimateVars3,PenultimateVars2,Program3,Program2).
addrules(VarList,OutputVars1,OutputVars2,PenultimateVars1,PenultimateVars2,Program1,Program2) :-
%%trace,
	OutputVars2=[OutputVar|OutputVars3],
	member(Var,VarList),
	member(OutputVar,OutputVars1),%%()
	append(VarList,OutputVars1,OutputVars4),
	member(Var2,OutputVars4),
	append(Program1,[[[n,=],[Var,Var2%%OutputVar
	]]],Program3),
	append(PenultimateVars1,[Var],PenultimateVars3),
	addrules(VarList,OutputVars1,OutputVars3,PenultimateVars3,PenultimateVars2,Program3,Program2).
**/
addrules(VarList,OutputVars1,OutputVars2,PenultimateVars1,PenultimateVars2,Program1,Program2) :-
find_addrules_outputvars(OutputVars1,OutputVars3),
	addrules1(OutputVars3,VarList,OutputVars1,OutputVars2,PenultimateVars1,PenultimateVars2,Program1,Program2).%%;(
	
addrules1([],_VarList,_OutputVars1,_OutputVars2,PenultimateVars,PenultimateVars,Program,Program) :- !.
addrules1(OutputVars3,VarList,OutputVars1,OutputVars2,PenultimateVars1,PenultimateVars2,Program1,Program2) :-
OutputVars3=[_OutputVars31|OutputVars32],
addrules2(VarList,OutputVars1,OutputVars2,PenultimateVars1,PenultimateVars3,Program1,Program3),
addrules1(OutputVars32,VarList,OutputVars1,OutputVars2,PenultimateVars3,PenultimateVars2,Program3,Program2).

addrules2([],_,_,PV,PV,Program,Program) :- !.
addrules2([[v,_]],_,_,PV,PV,Program,Program) :- !.
addrules2(VarList,OutputVars1,_OutputVars2,PenultimateVars1,PenultimateVars2,Program1,Program3) :- %%*** Underscored OutputVars2
%%writeln1(addrules(VarList,OutputVars1,OutputVars2,PenultimateVars1,PenultimateVars2,Program1,Program3)),
%%trace,
	%%OutputVars2=[],%%[OutputVar|OutputVars3],
	member(Var,VarList),
	%%member(OutputVar,OutputVars1),%%()
	append(VarList,OutputVars1,OutputVars4),
	member(Var2,OutputVars4),
	not(Var=Var2),
	append(Program1,[[[n,=],[Var,Var2%%OutputVar
	]]],Program3),
	append(PenultimateVars1,[Var],PenultimateVars2).

find_addrules_outputvars(OutputVars1,OutputVars3) :-
	OutputVars1=[],OutputVars3=[_],!.
find_addrules_outputvars(OutputVars1,OutputVars3) :-
	not(OutputVars1=[]),OutputVars3=OutputVars1.
%% optimise([[append,[a,a,d]],[append,[a,a,e]],[append,[a,a,f]],[append,[a,b,g]]],[g],P).
/****
optimise(Program1,InputVars1,InputVars2,PenultimateVars,Program2) :-
	findrulesflowingtopv1(Program1,InputVars1,InputVars2,PenultimateVars,[],Rules,true),
	%%findrulesflowingtopv1a(Program1,_Program32,InputVars1,InputVars2,PenultimateVars,[],_Rules1),
	intersection(Program1,Rules,Program3),
	unique1(Program3,[],Program2).
findrulesflowingtopv1(_,_,_,[],Rules,Rules,false).
findrulesflowingtopv1(Program0,InputVars1,InputVars2,Var,Rules1,Rules2,IV1Flag1) :-
	(Var=[v,_]),%%***;length(Var,1)),
	findrulesflowingtopv20(Program0,Program0,InputVars1,InputVars2,Var,Rules1,Rules2,IV1Flag1).
findrulesflowingtopv1(Program0,InputVars1,InputVars2,Vars1,Rules1,Rules2,IV1Flag1) :-
	Vars1=[Var|Vars2],
	findrulesflowingtopv20(Program0,Program0,InputVars1,InputVars2,Var,Rules1,Rules3,IV1Flag2), 
	findrulesflowingtopv1(Program0,InputVars1,InputVars2,Vars2,Rules3,Rules2,IV1Flag3),
	iv1flagdisjunction(IV1Flag2,IV1Flag3,IV1Flag1).

%%findrulesflowingtopv2([],Program,Program,_,_,Rules,Rules).
findrulesflowingtopv20(_,[],_InputVars1,_InputVars2,_Var,Rules,Rules,false).
findrulesflowingtopv20(Program0,Rules4,InputVars1,InputVars2,Var,Rules1,Rules2,IV1Flag1) :-
	Rules4=[Rule|Rules],
	(findrulesflowingtopv2(Program0,Rule,InputVars1,InputVars2,Var,Rules1,Rules3,IV1Flag2)->true;(Rules3=Rules1,IV1Flag2=false)),
	%%delete(Program0,Rule,Program1),
	findrulesflowingtopv20(Program0,Rules,InputVars1,InputVars2,Var,Rules3,Rules2,IV1Flag3),%%p1->0
	iv1flagdisjunction(IV1Flag2,IV1Flag3,IV1Flag1).
%%findrulesflowingtopv2(_,[],[],_,_,_,Rules,Rules).
findrulesflowingtopv2(Program0,Rule,InputVars1,InputVars2,Var,Rules1,Rules2,IV1Flag1) :-
	Rule=[[n,_PredicateName],Vars],
	restlast(Vars,[],Rest,Var),
	%%delete(Program1,[PredicateName,Vars],Program2),
	%%Program2=Program1,
	%%(not(intersection(Rulesx,Rules1))-> x
	%% append, append, unique1
	%%append(Rules1,[Rule],Rules3);Rules3=Rules1),

	%%member(Var2,Rest),
	%%member(Var2,InputVars1),

	length(Rest,Length1), Length1>=1,
	subtract(Rest,InputVars1,IV3s),
	length(IV3s,Length3),
	subtract(Rest,IV3s,IV1s),
	length(IV1s,Length2), Length2>=1,
	subtract(IV3s,InputVars2,[]),

	IV1Flag2=true,

	%%delete(Program0,Rule,Program1),

	%%(delete(Program0,Rule,Program3),
	%%iv3s1(IV3s,Program3,IV3s,[]),
	(Length3>=1->
	(findrulesflowingtopv1(Program0,InputVars1,InputVars2,IV3s,[],Rules5,IV1Flag3),not(Rules5=[]));
	(Rules5=[],IV1Flag3=false)),
	iv1flagdisjunction(IV1Flag2,IV1Flag3,IV1Flag4),
	%%->true; Rules5=[],IV1Flag1=IV1Flag4),
	
	((findrulesflowingtopv1(Program0,InputVars1,InputVars2,IV1s,[],Rules6,IV1Flag5), %%iv1s->rest, etc
	iv1flagdisjunction(IV1Flag4,IV1Flag5,IV1Flag1))->true;(Rules6=[],IV1Flag1=IV1Flag4)),

	append([Rule],Rules1,Rules9),
	append(Rules9,Rules5,Rules7),
	append(Rules7,Rules6,Rules8),
	unique1(Rules8,[],Rules2).
	

**
findrulesflowingtopv2(_Program0,Rule,InputVars1,InputVars2,Var,Rules1,Rules2,IV1Flag1) :-
	Rule=[_PredicateName,Vars],
	restlast(Vars,[],Rest,Var),
	%%delete(Program1,[PredicateName,Vars],Program2),
	%%Program2=Program1,
	(not(member(Rule,Rules1))->
	append(Rules1,[Rule],Rules2);Rules2=Rules1),
	subset(Rest,InputVars2),

	intersection(Rest,InputVars1,Intersection),
	length(Intersection,0),

%%	not((member(Var2,Rest),
%%	member(Var2,InputVars1))),

	IV1Flag1=false.
**
**
findrulesflowingtopv2(Program0,Rule,InputVars1,InputVars2,Var,Rules1,Rules2,IV1Flag1) :-
	Rule=[_PredicateName,Vars],
	restlast(Vars,[],Rest,Var),	
	%%delete(Program1,[PredicateName,Vars],Program3),
	%%Program3=Program1,
	%%append(Rules1,[Rule],Rules3),
	subset(Rest,InputVars2),
	
	intersection(Rest,InputVars1,Intersection),
	length(Intersection,0),

%%	not((member(Var2,Rest),
%%	member(Var2,InputVars1))),

%%	delete(Program0,Rule,Program1),

	IV1Flag2=false,
	findrulesflowingtopv1(Program0,InputVars1,InputVars2,Rest,[],Rules4,IV1Flag3),
	%%not(Rules4=[]),
	iv1flagdisjunction(IV1Flag2,IV1Flag3,IV1Flag1),

	append(Rules1,[Rule],Rules7),
	append(Rules7,Rules4,Rules8),
	unique1(Rules8,[],Rules2).
**
**
%%->true;(Program2=Program1,Rules2=Rules1)).
findrulesflowingtopv2(Rule,Program0,Program1,_Program2,InputVars1,InputVars,Var,Rules1,Rules2,IV1Flag1) :-
	Rule=[PredicateName,Vars],
	restlast(Vars,[],Rest,Var),
	%%delete(Program1,[PredicateName,Vars],Program4),
	%%Program4=Program1,
	append(Rules1,[[PredicateName,Vars]],Rules3),
	findrulesflowingtopv1(Program0,Program1,_Program2,InputVars1,InputVars,Rest,Rules3,Rules2,IV1Flag3),
	iv1flagdisjunction(IV1Flag2,IV1Flag3,IV1Flag1).

	%%findrulesflowingtopv2(Program5,Program2,Rest,Rules3,Rules2).

**
iv1flagdisjunction(A,B,true) :-
	(A=true); (B=true).
iv1flagdisjunction(_,_,false).
**
iv3s0([],_,IV3s1,IV3s2).
iv3s0(IV3s,Program0,IV3s1,IV3s2).
	IV3s=[IV3|IV3s3],
	iv3s1(IV3,Program0,IV3s1,IV3s4),
	iv3s0(IV3s3,Program0,IV3s4,IV3s2).
iv3s1(_,[],IV3s,IV3s).	
iv3s1(IV3,Program0,IV3s1,IV3s2) :-
	Program0=[Rule|Rules],
	iv3s2(IV3,Rule,IV3s1,IV3s3),
	iv3s1(IV3,Rules,IV3s3,IV3s2).
iv3s2(IV3,Rule,IV3s,IV3s1,IV3s2).
	Rule=[_PredicateName,Vars],
	restlast(Vars,[],_Rest,IV3),	
	delete(IV3s1,IV3,IV3s2).


findrulesflowingtopv1a(_,_,_,_,[],Rules,Rules).
findrulesflowingtopv1a(Program1,Program2,InputVars1,InputVars2,Var,Rules1,Rules2) :-
	atom(Var),
	findrulesflowingtopv2a(Program1,Program2,InputVars1,InputVars2,Var,Rules1,Rules2).
findrulesflowingtopv1a(Program1,Program2,InputVars1,InputVars2,Vars1,Rules1,Rules2) :-
	Vars1=[Var|Vars2],
	findrulesflowingtopv2(Program1,Program3,InputVars1,InputVars2,Var,Rules1,Rules3),
	findrulesflowingtopv1a(Program3,Program2,InputVars1,InputVars2,Vars2,Rules3,Rules2).
%%findrulesflowingtopv2([],Program,Program,_,_,Rules,Rules).
findrulesflowingtopv2a([],[],_,_,_,Rules,Rules).
findrulesflowingtopv2a(Program1,Program2,_InputVars1,InputVars2,Var,Rules1,Rules2) :-
	member([PredicateName,Vars],Program1),
	restlast(Vars,[],Rest,Var),
	(
%%delete(Program1,[PredicateName,Vars],Program2),
Program2=Program1,
	append(Rules1,[[PredicateName,Vars]],Rules2),
	subset(Rest,InputVars2)).
findrulesflowingtopv2a(Program1,Program2,InputVars1,InputVars2,Var,Rules1,Rules2) :-
	member([PredicateName,Vars],Program1),
	restlast(Vars,[],Rest,Var),
	(
%%delete(Program1,[PredicateName,Vars],Program3),
Program3=Program1,
	append(Rules1,[[PredicateName,Vars]],Rules3),
	subset(Rest,InputVars2)),
	findrulesflowingtopv1a(Program3,Program2,InputVars1,InputVars2,Rest,Rules3,Rules2).

%%->true;(Program2=Program1,Rules2=Rules1)).
findrulesflowingtopv2a(Program1,Program2,InputVars1,InputVars,Var,Rules1,Rules2) :-
	member([PredicateName,Vars],Program1),
	restlast(Vars,[],Rest,Var),
	%%delete(Program1,[PredicateName,Vars],Program4),
	Program4=Program1,
	append(Rules1,[[PredicateName,Vars]],Rules3),
	findrulesflowingtopv1a(Program4,Program2,InputVars1,InputVars,Rest,Rules3,Rules2).
	%%findrulesflowingtopv2(Program5,Program2,Rest,Rules3,Rules2).
	**
**/
restlast([],_,_,_) :- fail, !.	
restlast([Last],Rest,Rest,Last) :-
	Last=[v,_],!.
restlast(Last,Rest,Rest,Last) :-
	length(Last,1),!.
restlast(Vars1,Rest1,Rest2,Last) :-
	Vars1=[Var|Vars2],
	append(Rest1,[Var],Rest3),
	restlast(Vars2,Rest3,Rest2,Last),!.


rule(Program1,RuleName,InModes,OutModes,InputVars1,InputVars2,VarList,VarList2,OutputVars,Rule) :-

/*
findall(Rule_vars1,member([_Rule_name,Rule_vars1],Program1),Rule_vars2),foldr(append,Rule_vars2,Rule_vars3),

% count vars
	sort(Rule_vars3,K),
	findall(G,(member(G,K),findall(G,member(G,Rule_vars3),H),length(H,J),J>2),L),
% remove vars occuring more than twice
%(not(InputVars10=[])->trace;true),
(var(InputVars10)->InputVars10=InputVars1;subtract(InputVars10,L,InputVars1)),
(var(VarList0)->VarList0=VarList;subtract(VarList0,L,VarList)),
%notrace,
*/
%trace,
%writeln1(rule(Program1,RuleName,InModes,OutModes,InputVars1,InputVars2,VarList,VarList2,OutputVars,Rule)),
%notrace,

rule1(RuleName,InModes,OutModes,InputVars1,InputVars2a,VarList,VarList2a,OutputVars,Rule1),
((1 is InModes+OutModes,member(Rule1,Program1))->(InputVars1=[_|InputVars3],rule(Program1,RuleName,InModes,OutModes,InputVars3,InputVars2,VarList,VarList2,OutputVars,Rule2),Rule=Rule2);(%%InputVars1=InputVars3, *** Commented out
Rule=Rule1,InputVars2=InputVars2a,VarList2a=VarList2))
%%writeln1(Rule),(Rule=[[n,-],[[v,c],[v,c],[v,d]]]->true;true)
.%%->writeln(rule(RuleName,InModes,OutModes,InputVars1,InputVars2,VarList,VarList2,OutputVars,Rule));true).%%(writeln(stop),abort)).
%%.
rule1(RuleName,InModes,OutModes,InputVars1,InputVars2,VarList,VarList2,OutputVars,Rule) :-
%%writeln(rule(RuleName,InModes,OutModes,InputVars1,InputVars2,VarList,VarList2,OutputVars,Rule)),
	get_members(InModes,InputVars1,[],Vars1),
	rulea(OutModes,RuleName,Vars1,VarList,VarList2,
		Rule,OutputVars,Vars2),
	(num_modes(OutModes,Last,InputVars1),
	(equals_or(Vars1,Last)->true;equals_or(Vars2,Last))),
	append(InputVars1,Vars2,InputVars2)
	%%retractall(lastrule(_)),
   %%assertz(lastrule(Rule)),
	.

num_modes(OutModes,Last,_InputVars1) :-
	OutModes=0,Last=[],!.
num_modes(OutModes,Last,InputVars1) :-
	not(OutModes=0),restlast(InputVars1,[],_,Last),!.
	
member_functions(Rule,Algorithms,Item) :-
	member(Item,Algorithms),
	Item=[Rule|_Rest].

%%get_members(0,_,Vars,Vars):-true.
get_members(_,[],Vars,Vars).
get_members(InModes1,InputVars,Vars1,Vars2) :-
	(not(InModes1=0)->(
	%%findnsols(InModes1,A,(member(A,InputVars)),Vars2).
	%%length(Vars2,InModes1),append(Vars2,_,InputVars).
	member(Var,InputVars),
	%%InputVars=[Var|InputVars2],
	%%delete(InputVars,Var,InputVars2),
	append(Vars1,[Var],Vars3),
	InModes2 is InModes1-1,
	get_members(InModes2,InputVars,Vars3,Vars2));
	Vars1=Vars2). %%InModes2->1

equals_or([],_) :- true,!.%%fail.***
equals_or(List,Item) :-
	List=[Item|_Rest],!.
equals_or(List,Item1) :-
	List=[Item2|Rest],
	not(Item1=Item2),
	equals_or(Rest,Item1),!.

rulea(OutModes,RuleName,Vars1,VarList,VarList3,Rule,OutputVars,Vars2) :-
    get_vars(OutModes,VarList,VarList3,OutputVars,[],Vars2),
    append(Vars1,Vars2,Vars3),
    Rule=[RuleName,Vars3],!.

get_vars(0,VarList,VarList,_,Vars,Vars) :- !.
get_vars(OutModes1,VarList1,VarList2,OutputVars,Vars1,Vars2) :-
    var(VarList1,Var,VarList3,OutputVars),
    append(Vars1,[Var],Vars3),
    OutModes2 is OutModes1 - 1,
	 get_vars(OutModes2,VarList3,VarList2,OutputVars,Vars3,Vars2),!.
	
/**

rule(RuleName,1,1,InputVars1,InputVars2,VarList,VarList2,Rule) :-
	member(Var,InputVars1),
	rule2(RuleName,Var,VarList,VarList2,Rule,Var1),
	append(InputVars1,[Var1],InputVars2).
rule2(RuleName,Var,VarList,VarList2,Rule,Var1) :-
	var(VarList,Var1,VarList2),
	Rule=[RuleName,[Var,Var1]],!.

rule(RuleName,1,2,InputVars1,InputVars2,VarList,VarList2,Rule) :-
        member(Var,InputVars1),
        rule3(RuleName,Var,VarList,VarList2,Rule,Vars),
	append(InputVars1,Vars,InputVars2).
rule3(RuleName,Var,VarList,VarList3,Rule,[Var1,Var2]) :-
        var(VarList,Var1,VarList2),
        var(VarList2,Var2,VarList3),
        Rule=[RuleName,[Var,Var1,Var2]],!.

rule(RuleName,2,1,InputVars1,InputVars2,VarList,VarList2,Rule) :-
        member(Var,InputVars1),
        member(Vara,InputVars1),
        rule4(RuleName,Var,Vara,VarList,VarList2,Rule,Var1),
	append(InputVars1,[Var1],InputVars2).
rule4(RuleName,Var,Vara,VarList,VarList2,Rule,Var1) :-
        var(VarList,Var1,VarList2),
        Rule=[RuleName,[Var,Vara,Var1]],!.

rule(RuleName,2,2,InputVars1,InputVars2,VarList,VarList2,Rule) :-
        member(Var,InputVars),
        member(Vara,InputVars),
        rule5(RuleName,Var,Vara,VarList,VarList2,Rule,Vars),
	append(InputVars1,Vars,InputVars2).
rule5(RuleName,Var,Vara,VarList,VarList3,Rule,[Var1,Var2]) :-
        var(VarList,Var1,VarList2),
        var(VarList2,Var2,VarList3),
        Rule=[RuleName,[Var,Vara,Var1,Var2]],!.

**/

%%var(Item,Var,Vars,Vars) :-
%%	member([Item,Var],Vars).
var(Vars1,Var1,Vars2,OutputVars) :-
	length(Vars1,Vars1Length1),
	Vars1Length2 is Vars1Length1-1,
	length(Vars3,Vars1Length2),
	append(Vars3,[Var2A],Vars1),
	Var2A=[v,Var2],
	char_code(Var2,Var2Code1),
	Var2Code2 is Var2Code1 + 1,
	var2(Var2Code2,Var1A,OutputVars),
	Var1=[v,Var1A],
	append(Vars1,[Var1],Vars2),!.

var2(Code,Var1,OutputVars) :-
	%%outputvars(OutputVars),
	totalvars(TotalVars),
	Code2 is 96+TotalVars,
	Code =< Code2, %% 122
	char_code(Var1,Code),
	not(member(Var1,OutputVars)),!.
var2(Var2Code,Code3,OutputVars) :-
	Var2Code2 is Var2Code + 1,	
	totalvars(TotalVars),
	Code2 is 96+TotalVars,
	Var2Code2 =< Code2,
	var2(Var2Code2,Code3,OutputVars),!.
/**
algorithmstopredicates1([],Predicates1,Predicates1) :-!.
algorithmstopredicates1(Algorithms1,Predicates1,Predicates2) :-
	Algorithms1=[Algorithm1|Algorithms2],
	Algorithm1=[_TestNumber,_Queries,Algorithm3],
	algorithmstopredicates2(Algorithm3,[],Algorithm4),
	append_list(Predicates1,Algorithm4,Predicates4),
	algorithmstopredicates1(Algorithms2,Predicates4,Predicates2).
algorithmstopredicates2([],Predicates1,Predicates1) :- !.
algorithmstopredicates2(Algorithms1,Predicates1,Predicates2) :-
	Algorithms1=[Algorithm1|Algorithms2],
	Algorithm1=[Name,In,Out|Rest],
	append(Predicates1,[[Name,In,Out|Rest]],Predicates4),
	algorithmstopredicates2(Algorithms2,Predicates4,
		Predicates2).
**/
split3([],List,List) :- !.
split3(Predicates1,List1,List2) :-
	Predicates1=[Item1|List4],
	Item1=	[[[n,Name],In,Out]|_Rest],
	append(List1,[[[n,Name],In,Out]],List6),
	split3(List4,List6,List2),!.
	
split2([],List,List) :- !.
split2(Predicates1,List1,List2) :-
	Predicates1=[Item1|List4],
	Item1=	[[[n,Name],_In,_Out]|Rest],
	append(List1,[[[n,Name]|Rest]],List6),
	split2(List4,List6,List2),!.

/**
split2([],List,List) :- !.
split2(Predicates1,List1,List2) :-
	Predicates1=[Item1|List4],
	Item1=[[n,[Name,[[test,Test1],[numin,Numin],
		[numout,Numout]]]]|Rest],
	member([[n,[Name,[[test,_Test3],[numin,Numin],
		[numout,Numout]]]]|Rest],List4),
	delete(List4,[[n,[Name,[[test,_Test4],[numin,Numin],
		[numout,Numout]]]]|Rest],
		List7),
	append(List1,[[[n,[Name,[[test,Test1],[numin,Numin],
		[numout,Numout]]]]|Rest]],List6),
	split2(List7,List6,List2),!.
split2(Predicates1,List1,List2) :-
	Predicates1=[Item1|List4],
	Item1=[[n,[Name,[[test,Test1],[numin,Numin],
		[numout,Numout]]]]|Rest],
	append(List1,[[[n,[Name,[[test,Test1],[numin,Numin],
		[numout,Numout]]]]|Rest]],List6),
	split2(List4,List6,List2),!.
	
append_list(A,[],A):-!.
append_list(A,List,B) :-
	List=[Item|Items],
	append(A,[Item],C),
	append_list(C,Items,B).

**/

eliminate_unused_predicates(Program1a,Algorithms1a,Algorithms2) :-
	%% System calls and mode arities
	%%System_calls=[[is,1,1],[+,2,1],[=,2,1],[wrap,1,1],
	%%[unwrap,1,1],[head,1,1],[tail,1,1],[member,1,1],
	%%[delete,2,1],[append,2,1]], %% Ignore whether system calls are in Program and Algorithm - the interpreter will have detected whether system and user predicates clash earlier

	Program1a=[[[n, PredicateName], Arguments, ":-", _Body]],
	length(Arguments,ArgumentsLength),
	Start=[[[n,PredicateName],ArgumentsLength]],
		convert_to_grammar_part1(Program1a,[],_Program1b,Program1),
	%% Find calls in Program
	%%writeln([program1,Program1]),
	find_calls1(Start,Program1,[],Program2),
	%%writeln([program2,Program2]),
	%% Find calls in Algorithm
		convert_to_grammar_part1(Algorithms1a,[],_Algorithms1b,Algorithms1),
	%%writeln([algorithms1,Algorithms1]),
	find_calls1(Program2,Algorithms1,[],Algorithms3),
	%%writeln([algorithms3,Algorithms3]),
	append(Program2,Algorithms3,Rules),
	%% Eliminate user predicates mentioned in Program and Algorithms in Algorithms 
	eliminate_unused_predicates1(Rules,Algorithms1,[],
		Algorithms2).
	
find_calls1(_,[],Program,Program) :- !.
find_calls1(Program0,Program1,Program2,Program3) :-
	Program1=[[_Program4a,Program4]|Program5],
	%% The first predicate in Program4 only is needed to find the calls x
	(findall(Program7a,(((member([[n,PredicateName],Arguments,":-",Program6],Program4)->true;((member([[n,PredicateName],Arguments],Program4),Program6=[])->true;Program4=[[n,PredicateName],Arguments,":-",Program6])),
	length(Arguments,ArgumentsLength),
	Item=[[n,PredicateName],ArgumentsLength],
	(member(Item,Program0)->Program6=Program6a;Program6a=[])%%->true;
	%%Item=Program0
	),
	(find_calls2(Program6a,[],Program7a))),[Program7])),
	%%append(Program2,Program7,Program8),
	%%append(Program0,Program7,Program01));
	%%(Program8=Program2,Program01=Program0)),
	append(Program2,Program7,Program8),
	append(Program0,Program7,Program01),
	find_calls1(Program01,Program5,Program8,Program3).
	
find_calls2([],Program,Program) :- !.
/**
find_calls2(Program1,Program2,Program3) :-
	Program1=[Line|Program41],
	Line=[[n,code]|Program42],
	find_calls2(Program41,Program2,Program5),
	append(Program5,Program42,Program6),
	find_calls2(Program6,[],Program3).
**/

find_calls2(Program1,Program2,Program3) :-
	Program1=[Line|Program4],
	(Line=[[n,PredicateName],Arguments]->
	length(Arguments,ArgumentsLength);
	(Line=[[n,PredicateName]],ArgumentsLength=0)), %% correct syntax is [true] not true
	Item=[[[n,PredicateName],ArgumentsLength]],
	append(Program2,Item,Program5),
	find_calls2(Program4,Program5,Program3).

eliminate_unused_predicates1(_Rules,[],Algorithms,Algorithms) :- !.
eliminate_unused_predicates1(Rules,Algorithms1,Algorithms2,Algorithms3) :-
	Algorithms1=[[Algorithms4a,Algorithms4]|Algorithms5],
	%%(Algorithms4a=[]->
		%%eliminate_unused_predicates1(Rules,Algorithms5,Algorithms2,
		%%Algorithms3),%%;
((findall(Algorithms6a,(((member(Algorithms4a1,Algorithms4),Algorithms4a1=[[n,_]|_])->true;Algorithms4a1=Algorithms4),	((Algorithms4a1=[[n,PredicateName],Arguments,":-",_Program6],
%%Algorithms4a1=[[n,PredicateName],Arguments])->
	length(Arguments,ArgumentsLength))->true;
	(Algorithms4a1=[[n,PredicateName],Arguments2],
		length(Arguments2,ArgumentsLength)->true;
		(Algorithms4a1=[[n,PredicateName]],ArgumentsLength=0))),
	Item=[[n,PredicateName],ArgumentsLength],
	(member(Item,Rules)->
		(Algorithms4a=[]->Algorithms2=Algorithms6a;
		append(Algorithms2,
		[Algorithms4a],Algorithms6a));
		Algorithms6a=Algorithms2)),Algorithms6b)),
		Algorithms6b=[Algorithms6c|_],
		(var(Algorithms6c)->Algorithms6=[];Algorithms6=Algorithms6c),
		%%length(Algorithms4,Count)),
	eliminate_unused_predicates1(Rules,Algorithms5,Algorithms6,
		Algorithms3)).
