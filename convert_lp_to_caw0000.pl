% 


/**

convert_lp_to_caw0000(
[
        [[n,reverse],[[],[v,l],[v,l]]],
        [[n,reverse],[[v,l],[v,m],[v,n]],":-",
        [       [[n,head],[[v,l],[v,h]]],
                [[n,tail],[[v,l],[v,t]]],
                [[n,wrap],[[v,h],[v,h1]]],
                [[n,append],[[v,h1],[v,m],[v,o]]],
                [[n,reverse],[[v,t],[v,o],[v,n]]]
        ]
        ]
],

[
	% gets used i,o (with header with vars that are only either 
	% 	in the header or used in another reverse_c4*

	% skips writeln, other predetermined commands
	% assumes wrap, unwrap used
	% doesn't use read_string (takes var input instead)
	
	[[n,reverse],[[],[v,l],[v,l]]],
	
	[[n,reverse_c41],[[v,l],[v,t],[v,h1]],":-",
	[	[[n,head],[[v,l],[v,h]]],
		[[n,tail],[[v,l],[v,t]]],
		[[n,wrap],[[v,h],[v,h1]]]
	]],
	
	% with vars it needs from another r_c4* or the header
	
	[[n,reverse_c42],[[v,h1],[v,m],[v,t],[v,n]],":-",
	[	[[n,append],[[v,h1],[v,m],[v,o]]],
		[[n,reverse],[[v,t],[v,o],[v,n]]]
	]]
],

[
	% finds algs (configuration of calls to certain 
	% 	commands with header
	% puts together pred

	[[n,reverse],[[],[v,l],[v,l]]],
	
	[[n,reverse],[[v,l],[v,m],[v,n]],":-",
	[	[n,reverse_c41],[[v,l],[v,t],[v,h1]],
		[n,reverse_c42],[[v,h1],[v,m],[v,t],[v,n]]
	]]
],

	% finds specs for r_c4*'s by using lpi with a 
	% flag switched on that returns the specs
	% - specs are i,o vars for lines, where c4 alg
	%   finds algs meeting spec for set of lines
	
	% * LPI numbers pred, pred lines, has widgets that 
	% indicate these and the var states
	% - with assertz x another var

**/

convert_lp_to_caw0000 :-

