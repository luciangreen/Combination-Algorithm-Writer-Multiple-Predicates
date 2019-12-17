interpretstatement1(_F0,_Functions,[[n,[not,Operator]],[Variable1,Variable2]],Vars1,Vars2,true,nocut) :-
	isop(Operator),
	interpretpart(not_is,Variable1,Variable2,Vars1,Vars2),!.

interpretstatement1(_F0,_Functions,[[n,[not,Operator]],[Variable1,Variable2]],Vars1,Vars2,true,nocut) :-
	comparisonoperator(Operator),
%%writeln1(4),
        interpretpart(not_iscomparison,Operator,Variable1,Variable2,Vars1,Vars2).

interpretstatement1(_F0,_Functions,[[n,[not,member]],[Variable1,Variable2]],Vars1,Vars2,true,nocut) :-
%%writeln1(8),
        interpretpart(not_member,Variable1,Variable2,Vars1,Vars2).

interpretstatement1(_F0,_Functions,[[n,[not,head]],[Variable1,Variable2]],Vars1,Vars2,true,nocut) :-
%%writeln1(6),
        interpretpart(not_head,Variable1,Variable2,Vars1,Vars2).

interpretstatement1(_F0,_Functions,[[n,[not,tail]],[Variable1,Variable2]],Vars1,Vars2,true,nocut) :-
%%writeln1(61),
        interpretpart(not_tail,Variable1,Variable2,Vars1,Vars2).

interpretstatement1(_F0,_Functions,[[n,[not,append]],[Variable1,Variable2,Variable3]],Vars1,Vars2,true,nocut) :-
%%writeln1(9),
        interpretpart(not_append,Variable1,Variable2,Variable3,Vars1,Vars2).

interpretstatement1(_F0,_Functions,[[n,[not,stringconcat]],[Variable1,Variable2,Variable3]],Vars1,Vars2,true,nocut) :-
        interpretpart(not_stringconcat,Variable1,Variable2,Variable3,Vars1,Vars2).

interpretstatement1(_F0,_Functions,[[n,[]],[Variable1]],Vars1,Vars2,true,nocut) :-
%%writeln1(8),
        interpretpart([],Variable1,Vars1,Vars2).

interpretstatement1(_F0,_Functions,[[n,""],[Variable1]],Vars1,Vars2,true,nocut) :-
%%writeln1(8),
        interpretpart("",Variable1,Vars1,Vars2).

interpretstatement1(_F0,_Functions,[[n,"_"],[Variable1]],Vars1,Vars2,true,nocut) :-
%%writeln1(8),
        interpretpart("_",Variable1,Vars1,Vars2).


interpretpart(not_is,Variable1,Variable2,Vars1,Vars1) :-
        getvalues(Variable1,Variable2,Value1,Value2,Vars1),
        	not(isempty(Value1)),
        	not(isempty(Value2)),
        debug_call(Skip,[[n,[not,=]],[Value1,Value2]]),
        ((not(Value1 = Value2)
		)->
      debug_exit(Skip,[[n,[not,=]],[Value1,Value2]])
;     debug_fail(Skip,[[n,[not,=]],[Value1,Value2]])),!.                        	

interpretpart(not_iscomparison,Operator,Variable1,Variable2,Vars1,Vars1) :-        getvalues(Variable1,Variable2,Value1,Value2,Vars1),
        debug_call(Skip,[[n,[not,Operator]],[Value1,Value2]]),
	((isval(Value1),
	isval(Value2),
	Expression=..[Operator,Value1,Value2],
        not(Expression))->
      debug_exit(Skip,[[n,[not,Operator]],[Value1,Value2]])
;     debug_fail(Skip,[[n,[not,Operator]],[Value1,Value2]])),!.

interpretpart(not_member,Variable1,Variable2,Vars1,Vars1) :-
        getvalues(Variable1,Variable2,Value1,Value2,Vars1),
debug_call(Skip,[[n,[not,member]],[Value1,Value2]]),
  	(((not(Value2=empty)->%%member(Value2,Value1),
	(not(member(Value2,Value1))
	%%putvalue(Variable2,Value3,Vars1,Vars2)%%,Vars2=Vars1
	)))->
      debug_exit(Skip,[[n,[not,member]],[Value1,Value2]])
;     debug_fail(Skip,[[n,[not,member]],[Value1,Value2]])),!.

interpretpart(not_head,Variable1,Variable2,Vars1,Vars1) :-
	getvalues(Variable1,Variable2,Value1,Value2,Vars1),
debug_call(Skip,[[n,[not,head]],[Value1,Value2]]),
	((not(Value1=[Value2|_Rest])
        %%val1emptyorvalsequal(Value2,Value1A),
        %%putvalue(Variable2,Value1A,Vars1,Vars2)
        )->
      debug_exit(Skip,[[n,[not,head]],[Value1,Value2]])
;     debug_fail(Skip,[[n,[not,head]],[Value1,Value2]])),!.
        	
interpretpart(not_tail,Variable1,Variable2,Vars1,Vars1) :-
        getvalues(Variable1,Variable2,Value1,Value2,Vars1),
debug_call(Skip,[[n,[not,tail]],[Value1,Value2]]),
        ((not(Value1=[_Head|Value2])
	%%removebrackets(Value1A,Value1B), 
        %%val1emptyorvalsequal(Value2,Value1A),
        %%putvalue(Variable2,Value1A,Vars1,Vars2)
        )->
      debug_exit(Skip,[[n,[not,tail]],[Value1,Value2]])
;     debug_fail(Skip,[[n,[not,tail]],[Value1,Value2]])),!.
        	
interpretpart(not_append,Variable1,Variable2,Variable3,Vars1,Vars1) :-
        	
        getvalues(Variable1,Variable2,Variable3,Value1,Value2,Value3,Vars1),
        debug_call(Skip,[[n,[not,append]],[Value1,Value2,Value3]]),
        ((islist(Value1),islist(Value2),
        not(append1(Value1,Value2,Value3))
        %%val1emptyorvalsequal(Value3,Value3A),
       %% putvalue(Variable3,Value3A,Vars1,Vars2)
       )->
      debug_exit(Skip,[[n,[not,append]],[Value1,Value2,Value3]])
;     debug_fail(Skip,[[n,[not,append]],[Value1,Value2,Value3]])),!.                        	

interpretpart(not_stringconcat,Variable1,Variable2,Variable3,Vars1,Vars1) :-
        	
        getvalues(Variable1,Variable2,Variable3,Value1,Value2,Value3,Vars1),
        debug_call(Skip,[[n,[not_stringconcat]],[Value1,Value2,Value3]]),
        ((string(Value1),string(Value2),string(Value3),
        stringconcat(Value1,Value2,Value3)
        %%val1emptyorvalsequal(Value3,Value3A),
        %%putvalue(Variable3,Value3A,Vars1,Vars2)
        )->
      debug_exit(Skip,[[n,[not_stringconcat]],[Value1,Value2,Value3]])
;     debug_fail(Skip,[[n,[not_stringconcat]],[Value1,Value2,Value3]])),!.                        	

interpretpart([],Variable1,Vars1,Vars1) :-
        getvalue(Variable1,Value1,Vars1),
        %%getvalue(Value1,Value1A,Vars1),
	%%isvalstr(Value1),
	%%isvalstr(Value1A),
	not(isempty(Value1)),
	%%isempty(Value2),
        %%val1emptyorvalsequal(Value2,Value1),
	%%isval(Value2),
debug_call(Skip,[[n,[]],[Value1]]),
(        Value1=[]->
debug_exit(Skip,[[n,[]],[Value1]])
;     debug_fail(Skip,[[n,[]],[Value1]])),!.

interpretpart([],Variable1,Vars1,Vars2) :-
        getvalue(Variable1,Value1,Vars1),
        %%getvalue(Value1,Value1A,Vars1),
	%%isvalstr(Value1),
	%%isvalstr(Value1A),
	isempty(Value1),
        %%val1emptyorvalsequal(Value1,[]),
	%%isval(Value2),
debug_call(Skip,[[n,[]],[variable]]),
(        putvalue(Variable1,[],Vars1,Vars2)->
debug_exit(Skip,[[n,[]],[[]]])
;     debug_fail(Skip,[[n,[]],[variable]])),!.
		
interpretpart("",Variable1,Vars1,Vars1) :-
        getvalue(Variable1,Value1,Vars1),
        %%getvalue(Value1,Value1A,Vars1),
	%%isvalstr(Value1),
	%%isvalstr(Value1A),
	not(isempty(Value1)),
	%%isempty(Value2),
        %%val1emptyorvalsequal(Value2,Value1),
	%%isval(Value2),
debug_call(Skip,[[n,""],[Value1]]),
(        Value1=""->
debug_exit(Skip,[[n,""],[Value1]])
;     debug_fail(Skip,[[n,""],[Value1]])),!.

interpretpart("",Variable1,Vars1,Vars2) :-
        getvalue(Variable1,Value1,Vars1),
        %%getvalue(Value1,Value1A,Vars1),
	%%isvalstr(Value1),
	%%isvalstr(Value1A),
	isempty(Value1),
        %%val1emptyorvalsequal(Value1,""),
	%%isval(Value2),
debug_call(Skip,[[n,""],[variable]]),
(        putvalue(Variable1,"",Vars1,Vars2)->
debug_exit(Skip,[[n,""],[""]])
;     debug_fail(Skip,[[n,""],[variable]])),!.
		

interpretpart("_",_,V,V) :- 
debug_call(Skip,[[n,"_"],[variable]]),
debug_exit(Skip,[[n,"_"],[variable]]),!.
