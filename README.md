# List Prolog Interpreter

List Prolog Interpreter (LPI) is an interpreter for a different version of Prolog that is in list format, making it easier to generate List Prolog programs. This interpreter is an algorithm that parses and runs List Prolog code. I wrote the interpreter in SWI-Prolog.

# Getting Started

Please read the following instructions on how to install the project on your computer for writing code.

# Prerequisites

The <a href="https://github.com/luciangreen/Languages">Languages repository</a> enables List Prolog Interpreter to be run in different languages.

<a href="https://github.com/luciangreen/culturaltranslationtool">Cultural Translation Tool</a>. Requires Translation Shell:

You may need to install gawk using Homebrew.

Install <a href="https://github.com/soimort/translate-shell">Translation Shell</a> on Mac, etc.
Change line in ctt.pl
`concat_list(["../../../trans ",FromLang,":",ToLang," '",Input1,"'"],F),` to correct location of <a href="https://github.com/soimort/translate-shell">trans</a>

# Installation from List Prolog Package Manager (LPPM)

* Optionally, you can install from LPPM by installing <a href="https://www.swi-prolog.org/build/">SWI-Prolog</a> for your machine, downloading the <a href="https://github.com/luciangreen/List-Prolog-Package-Manager">LPPM Repository</a>,
```
git clone https://github.com/luciangreen/List-Prolog-Package-Manager.git
cd List-Prolog-Package-Manager
```
loading LPPM with `['lppm'].` then installing the package by running `lppm_install("luciangreen","listprologinterpreter").`.

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

# LPI API

* To run LPI on a Prolog server:
* Move `lpi-api.pl` to the root (`/username/` or `~` on a server) of your machine.
* Re-enter the paths to your Prolog files in it.
* Enter `[lpi-api.pl]` in SWI-Prolog and `server(8000).`.
* On a local host access the algorithm at `http://127.0.0.1:8000` and replace 127.0.0.1 with your server address on a server.

# Diagram of List Prolog Converters

<img src="https://www.lucianacademy.com/files/Philosophy/LucianGreensPhilosophyMay2020/Diagram%20of%20List%20Prolog%20Converters.png" alt="Diagram of List Prolog Converters">

See the <a href="https://github.com/luciangreen">list of repositories by luciangreen</a> for these.

# Versioning

We will use SemVer for versioning.

# Authors

Lucian Green - Initial programmer - <a href="https://www.lucianacademy.com/">Lucian Academy</a>

# License

I licensed this project under the BSD3 License - see the LICENSE.md file for details
