# Combination Algorithm Writer with Multiple Predicates

Combination Algorithm Writer with Multiple Predicates is a SWI-Prolog algorithm that finds algorithms from given rules,  algorithm parts (from algdict.pl) and writes algorithms with multiple predicates that satisfy the given input and output.

# Getting Started

Please read the following instructions on how to install the project on your computer for writing code.

# Prerequisites

Install List Prolog Interpreter Repository (https://github.com/luciangreen/listprologinterpreter) first.

# Installing

* Download this repository.
* In SWI-Prolog, enter:
```
['cawplistprolog'].
```
* Running

Try CAWP regression tests (in <a href="https://github.com/luciangreen/Combination-Algorithm-Writer-Multiple-Predicates/blob/master/cawpverify.pl">cawpverify.pl</a>):
Example
`cawptest(off,N,S).`
Example Output
```
[cawptest,1,passed]

N = S, S = 1.
```


# Notes on CAWMP:

* Variables should be named a, b, c, and so on, without any gaps.
* In inputted data, i.e. specifications, the above applies.


CAWMP is called with the command:	`caw00(Debug,Function,Rules,MaxLength,MaxPredicates,TotalVars,Specifications,Program1,Program2).`
e.g. `caw00(on,function3,[],2,1,4,[[[[[[v,a],1],[[v,b],1]],[[[v,c],2]],true],[[[[v,a],1],[[v,b],2]],[[[v,c],3]],true],[[[[v,a],1],[[v,b],1]],[[[v,c],1]],fail],[[[[v,a],1],[[v,b],1]],[[[v,c],3]],fail]]],[],Program2).`
with output: `Program2=[[[n,function1],[[v,a],[v,b],[v,c]],":-",[[[n,+],[[v,a],[v,b],[v,c]]]]],[[n,function3],[[v,a],[v,b],[v,c]],":-",[[[n,function1],[[v,a],[v,b],[v,d]]],[[n,=],[[v,d],[v,c]]]]]].`

* Debug is true for trace, false for no trace.
* Function is the inputted name of the algorithm.
* Rules are the commands to build the code from, e.g. `[[[n,+],2,1],[[n,[]],1,0]]`, which mean the `"+"` function has 2 inputs and one output and the `[]` function has one input and verifies an empty list.  See the <a href="https://github.com/luciangreen/listprologinterpreter/blob/master/LPI_docs.md">List Prolog Interpreter Documentation</a> for more commands.
* MaxLength is the maximum number of commands per predicate.
* MaxPredicates is the maximum number of predicates per algorithm.
* TotalVars is the number of different variables per predicate.
* Specifications have the form `A=[Input_variable_list, Output_variable_list, True (if this specification in true, or false if trying to eliminate possibilities)]`, where a variable list is `[B]` where `B` is e.g. `[[v,c],[1,2]]`.  `[A1,A2]` is for a predicate, where there must be at least 2 specifications per predicate, and `[[A1,A2]]` for the specifications for the algorithm, e.g. `[[[[[[v,a],1],[[v,b],1]],[[[v,c],3]],true],[[[[v,a],1],[[v,b],1]],[[[v,c],3]],true]]]`.
* Program1 is the initial program (usually `[]`).  Planned: partial multiple predicate algorithms may be given to the algorithm to complete.
* Program2 is the final outputted algorithm.
* CAWMP allows shorter and shorter lists of specifications to specify algorithms as its dictionary grows.

# Authors

Lucian Green - Initial programmer - <a href="https://www.lucianacademy.com/">Lucian Academy</a>

# License

I licensed this project under the BSD3 License - see the <a href="LICENSE">LICENSE.md</a> file for details

