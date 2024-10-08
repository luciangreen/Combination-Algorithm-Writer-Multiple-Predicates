# Combination Algorithm Writer with Multiple Predicates

* NB. This repository has been deprecated by <a href="https://github.com/luciangreen/Combination-Algorithm-Writer-Stable">Combination Algorithm Writer with Predicates Stable (CAWPS)</a>, which may return more commands per predicate, but this repository, CAWMP, has more features.

Combination Algorithm Writer with Multiple Predicates (CAWMP) is a SWI-Prolog algorithm that finds algorithms from given rules,  algorithm parts (from algdict.pl) and writes algorithms with multiple predicates that satisfy the given input and output.

Please contact Lucian Green at <a href="mailto:luciangreen@lucianacademy.com">luciangreen@lucianacademy.com</a> with questions, comments and feedback about CAWMP.  Please use <a href="https://github.com/luciangreen/Combination-Algorithm-Writer-Multiple-Predicates/blob/master/cawmp.bib">this citation</a> for research.

# Getting Started

Please read the following instructions on how to install the project on your computer for writing code.

# Prerequisites

Install List Prolog Interpreter Repository (https://github.com/luciangreen/listprologinterpreter) first.


# Installation from List Prolog Package Manager (LPPM)

* Optionally, you can install from LPPM by installing <a href="https://www.swi-prolog.org/build/">SWI-Prolog</a> for your machine, downloading the <a href="https://github.com/luciangreen/List-Prolog-Package-Manager">LPPM Repository</a>,
```
mkdir GitHub
cd GitHub/
git clone https://github.com/luciangreen/List-Prolog-Package-Manager.git
cd List-Prolog-Package-Manager
swipl
```
loading LPPM with `['lppm'].` then installing the package by running
```
lppm_install("luciangreen","Combination-Algorithm-Writer-Multiple-Predicates").
../
```.

# Installing

* Download this repository.
* In SWI-Prolog, enter:
```
['cawplistprolog'].
```
* Running

* Try CAWP regression tests (in <a href="https://github.com/luciangreen/Combination-Algorithm-Writer-Multiple-Predicates/blob/master/cawpverify.pl">cawpverify.pl</a>):
Example
`cawptest1(off,N,S).` where N is the test number 1-8.
Example Output
```
[cawptest,passed,[[[n,add0],[[v,a],[v,b],[v,c]],":-",[[[n,+],[[v,a],[v,b],[v,d]]],[[n,=],[[v,d],[v,c]]]]]]]
```
* Try CAWP regression tests (in <a href="https://github.com/luciangreen/Combination-Algorithm-Writer-Multiple-Predicates/blob/master/cawpverifya.pl">cawpverifya.pl</a>):

Example
`cawptest1a(off,7,S).`
Example Output
```
[cawptest,7,passed]
S = passed.
```

# Running Random Combination Algorithm Writer (RCAW)

* RCAW generates random algorithms.

* To load and install, follow the instructions above.

# Notes on CAWMP:

* Variables should be named a, b, c, and so on, without any gaps.
* In inputted data, i.e. specifications, the above applies.


CAWMP is called with the command:	`caw00(Debug,Function,Rules,MaxLength,MaxPredicates,TotalVars,NumInputs,NumOutputs,Specifications,AlgDict,Program1,Program2).`
e.g. 
```
caw00(off,add0,[],2,1,3,[1,2],[0,1],
%% Specs
[[[[[[v,a],1],[[v,b],2]],[],true],
[[[[v,a],2],[[v,b],1]],[],true]]],
[ %% Algorithm dictionary
        [[[n,1],1,1],[[v,a],[v,b]],":-",
        [       [[n,+],[[v,a],1,[v,c]]],
                [[n,=],[[v,c],[v,b]]]]],

        [[[n,1],1,1],[[v,a],[v,b]],":-",
        [       [[n,-],[[v,a],1,[v,c]]],
                [[n,=],[[v,c],[v,b]]]]]
],

[],Program2).
```
It has the output:
```
Program2=
[
	[[n,1],[[v,a],[v,b]],":-",
		[[[n,+],[[v,a],1,[v,c]]],
		[[n,=],[[v,c],[v,b]]]]],
	[[n,1],[[v,a],[v,b]],":-",
		[[[n,-],[[v,a],1,[v,c]]],
		[[n,=],[[v,c],[v,b]]]]],
	[[n,add0],[[v,a],[v,b]],":-",
		[[[n,1],[[v,b],[v,c]]],
		[[n,=],[[v,a],[v,c]]]]]
].
```

* Debug is true for trace, false for no trace.
* Function is the inputted name of the algorithm.
* Rules are the commands to build the code from, e.g. `[[[n,+],2,1],[[n,[]],1,0]]`, which mean the `"+"` function has 2 inputs and one output and the `[]` function has one input and verifies an empty list.  See the <a href="https://github.com/luciangreen/listprologinterpreter/blob/master/LPI_docs.md">List Prolog Interpreter Documentation</a> for more commands.
* MaxLength is the maximum number of commands per predicate.
* MaxPredicates is the maximum number of predicates per algorithm.
* TotalVars is the number of different variables per predicate.
* Specifications have the form `A=[Input_variable_list, Output_variable_list, True (if this specification in true, or false if trying to eliminate possibilities)]`, where a variable list is `[B]` where `B` is e.g. `[[v,c],[1,2]]`.  There is a list of specifications, e.g. `[A1,A2]` for a predicate (i.e. not the whole algorithm), and `[[A1,A2],[B1,B2]]` for the specifications for the algorithm, e.g. 
```
[[[[[[v,a],1],[[v,b],2]],[],true],
[[[[v,a],2],[[v,b],1]],[],true]]]
```
* Program1 is the initial program (usually `[]`).  Planned: partial multiple predicate algorithms may be given to the algorithm to complete.
* Program2 is the final outputted algorithm.
* CAWMP allows shorter and shorter lists of specifications to specify algorithms as its dictionary grows.

# Not tested updates

Neither Prolog nor List Prolog can return non-deterministic results after point of execution passes from a clause to a child clause, so the result of testing the repository is that multiple clauses cannot be generated in this case.

See the CAWPS Repository because it is all that is needed to build an algorithm bottom-up.  Without generating base cases first, neither CAWMP nor CAWPS can work, and CAWPS can generate more commands per predicate than CAWMP.

# Documentation of List Prolog Interpreter CAWMP Commands

See <a href="https://github.com/luciangreen/Combination-Algorithm-Writer-Multiple-Predicates/blob/master/LPCAWMP_docs.md">Documentation of List Prolog Interpreter CAWMP Commands</a>.

# Authors

Lucian Green - Initial programmer - <a href="https://www.lucianacademy.com/">Lucian Academy</a>

# License

I licensed this project under the BSD3 License - see the <a href="LICENSE">LICENSE.md</a> file for details

