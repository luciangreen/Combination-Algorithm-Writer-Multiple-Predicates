test( 
/**[
[[[n,add1],1,0],["a"]],
[[[n,add2],1,0],[[v,b]],":-",
[       [[n,add1],[[v,b]]]]]
**/
/**
        [[[n,add1],3,2],[[],[v,b],[v,c],[v,c],[v,d]]],
        [[[n,add2],3,2],[[v,b],[v,c],[v,d],[v,e],[v,g]],":-",
        [       [[n,head],[[v,b],[v,f]]],
                [[n,tail],[[v,b],[v,g]]],
                [[n,+],[[v,f],[v,c],[v,h]]],
                [[n,wrap],[[v,h],[v,i]]],
                [[n,append],[[v,d],[v,i],[v,e]]]
                %%[[n,add],[[v,g],[v,c],[v,j],[v,e]]]
        ]
        ]
**/
%% renumber vars, test in lipv, write sol, count vars in sol

%%[[[n,add],3,1],[[v,a],[v,b],[v,c],[v,d]],":-",[[[n,[]],[[v,a]]],[[n,"_"],[[v,a]]],[[n,"_"],[[v,b]]],[[n,=],[[v,c],[v,d]]]]]
[

	[[[n,function1],2,1],[[v,a],[v,b],[v,c]],":-", %% Modes=2 inputs, 1 output
	[
		[[n,+],[[v,a],[v,b],[v,c]]]
	]
	]
	
%%*** 2,1 in above is removed in code
	/**
	     [[[n,function2],2,1],[[v,a],[v,b],[v,c]],":-",
        [
                [[n,+],[[v,a],[v,b],[v,d]]],
                [[n,+],[[v,d],1,[v,c]]]
        ]
        ],
        
        [[[n,function3],2,1],[[v,a],[v,b],[v,c]],":-",
        [
                [[n,function4],[[v,d],[v,f]]],
                [[n,+],[[v,a],[v,b],[v,e]]],
                [[n,+],[[v,e],[v,f],[v,g]]],
                [[n,+],[[v,g],[v,d],[v,c]]]
        ]
        ],
        
        [[[n,function4],0,2],[[v,a],[v,f]],":-",
        [
                [[n,is],[[v,a],2]],
                [[n,is],[[v,f],1]]
        ]
        ],
%%***

		  [[[n,grammar1],1,0],[[v,s]],":-",
		  [
		  			 [[n,noun],[[v,s],""]] 
		  ]
		  ],
		  
		  [[[n,noun],2,0],"->",["apple"]],
		  [[[n,noun],2,0],"->",["apple"]],
%% 2 inputs and 0 outputs in internal grammar part form for grammars.  Add any inputs to 2 and any outputs to 0.
%%**
		  [[[n,grammar2],1,0],[[v,s]],":-",
		  [
		  			 [[n,noun1],[[v,s],""]] 
		  ]
		  ],
		  
		  [[[n,noun1],2,0],"->",[""]],
		  [[[n,noun1],2,0],"->",["a",[[n,noun1]]]]
		  **/
		  
]).
