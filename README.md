# List Prolog Interpreter

List Prolog Interpreter is an interpreter for a different version of Prolog that is in list format, making it easier to generate List Prolog programs. This interpreter is an algorithm that parses and runs List Prolog code. I wrote the interpreter in SWI-Prolog.

# Getting Started

Please read the following instructions on how to install the project on your computer for writing code.

# Prerequisites

None

# Installing

* Please download and install SWI-Prolog for your machine at https://www.swi-prolog.org/build/.

* Download the repository to your machine.
In the SWI-Prolog environment, enter:
`['listprolog'].`    

* Running the tests
To run all tests, enter:
`test(off,NTotal,Score).`

To run a specific test:
`test1(off,TestNumber,Passed).`
where TestNumber is the test number from <a href="lpiverify4.pl">lpiverify4.pl</a>.

Example of an end to end test
The query `test1(off,1,Passed).`
tests the following predicate:
```
test(1,[[n,function],[1,1,[v,c]]],
[
        [[n,function],[[v,a],[v,b],[v,c]],":-",
        [
                [[n,+],[[v,a],[v,b],[v,c]]]
        ]
        ]
]
,[[[v,c], 2]]).
```
This query contains the query tested, the predicate and the required result.

Also, the query `test1(off,7,Passed).`
```
test(7,[[n,reverse],[[1,2,3],[],[v,l]]],
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
],[[[v,l], [3, 2, 1]]]).
```
tests the reverse predicate.

# Documentation

See <a href="https://github.com/luciangreen/listprologinterpreter/blob/master/LPI_docs.md">List Prolog Documentation</a>.

# Text to Breasonings

<a href="https://github.com/luciangreen/Text-to-Breasonings">Text to Breasonings</a> now has its own repository.

# Data to Types Documentation

See <a href="https://github.com/luciangreen/listprologinterpreter/blob/master/D2T_docs.md">Data to Types Documentation</a>.

# Versioning

We will use SemVer for versioning.

# Authors

Lucian Green - Initial programmer - <a href="https://www.lucianacademy.com/">Lucian Academy</a>

# License

I licensed this project under the BSD3 License - see the LICENSE.md file for details
