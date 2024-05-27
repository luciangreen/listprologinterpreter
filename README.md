# List Prolog Interpreter

List Prolog Interpreter (LPI) makes it easier to generate List Prolog programs. It works by interpreting different versions of Prolog that are in a list format. Using an algorithm that was written in SWI-Prolog, it easily parses and runs List Prolog code.

# Getting Started

Please read the following instructions on how to install the project on your computer for writing code.

# Prerequisites

* Please download and install SWI-Prolog for your machine at `https://www.swi-prolog.org/build/`.

* You may need to install gawk using Homebrew.

* Install <a href="https://github.com/soimort/translate-shell">Translation Shell</a> on Mac, etc.
Change line in
```
culturaltranslationtool/ctt2.pl
trans_location("../../../gawk/trans").
```
to correct location of <a href="https://github.com/soimort/translate-shell">trans</a>.

# 1. Install manually

Download <a href="http://github.com/luciangreen/listprologinterpreter/">this repository</a>, the <a href="https://github.com/luciangreen/Languages"> Languages repository</a> (which enables List Prolog Interpreter to be run in different languages), <a href="https://github.com/luciangreen/SSI">SSI</a> and <a href="https://github.com/luciangreen/culturaltranslationtool">Cultural Translation Tool</a>.

# 2. Or Install from List Prolog Package Manager (LPPM)

* Download the <a href="https://github.com/luciangreen/List-Prolog-Package-Manager">LPPM Repository</a>:

```
mkdir GitHub
cd GitHub/
git clone https://github.com/luciangreen/List-Prolog-Package-Manager.git
cd List-Prolog-Package-Manager
swipl
['lppm'].
lppm_install("luciangreen","listprologinterpreter").
halt
```

# Running

* In Shell:
`cd listprologinterpreter`
`swipl`
`['listprolog'].`    

* Running the tests
To run all tests, enter:
`test(off,NTotal,Score).`

To run a specific test:
`test1(off,TestNumber,Passed).`
where TestNumber is the test number from <a href="lpiverify4.pl">lpiverify4.pl</a>.

Example of an end to end test
The query `test1(Debug,1,Passed).`
where `Debug (trace)=off`
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
,[[[[v,c], 2]]]).
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
],[[[[v,l], [3, 2, 1]]]]).
```
tests the reverse predicate.

To run all tests (main, types, open and open types) in any language:
```
test_all00("en",off,NTotal,Score).
test_all00("en2",off,NTotal,Score).
```
where "en2" is an English language with e.g. `"concatenate strings"` instead of `stringconcat` ("en", or see available <a href="https://github.com/soimort/translate-shell">language codes</a> - see the <a href="https://github.com/luciangreen/Languages"> Languages repository</a> for instructions about how to install different languages).

* Note 1: drag and drop contents of `test_open_and_types_open_data/` into an empty file in BBEdit (Mac) to copy and paste into Terminal for tests with input.

To run a test from one of main, types, open or open types, run one of:
```
test_all01(test,            4,"en2",off,1,Passed).
test_all01(test_types_cases,6,"en2",off,1,Passed).
test_all01(testopen_cases,  3,"en2",off,1,Passed).
test_all01(test_open_types, 5,"en2",off,1,Passed).
```
where 1 is replaced with the test number from

* <a href="https://github.com/luciangreen/listprologinterpreter/blob/master/lpiverify4.pl">lpiverify4.pl</a>
* <a href="https://github.com/luciangreen/listprologinterpreter/blob/master/lpiverify4_types.pl">lpiverify4_types.pl</a>
* <a href="https://github.com/luciangreen/listprologinterpreter/blob/master/lpiverify4_open.pl">lpiverify4_open.pl</a>
* <a href="https://github.com/luciangreen/listprologinterpreter/blob/master/lpiverify4_open_types.pl">lpiverify4_open_types.pl</a>

respectively.

* Run Prolog tests:
```
['lpiverify_pl.pl].
test_pl1(off,A,B).
```

* See note 1 above.

To run all tests (main, types, open and open types) back-translating to and from any language:
```
test_all_bt00("en2",off,NTotal,Score).
```

To run a test from one of main, types, open or open types, run one of:
```
test_all_bt01(test,            4,"en2",off,1,Passed).
test_all_bt01(test_types_cases,6,"en2",off,1,Passed).
test_all_bt01(testopen_cases,  3,"en2",off,1,Passed).
test_all_bt01(test_open_types, 5,"en2",off,1,Passed).
```
where 1 is replaced with the test number from

* <a href="https://github.com/luciangreen/listprologinterpreter/blob/master/lpiverify4.pl">lpiverify4.pl</a>
* <a href="https://github.com/luciangreen/listprologinterpreter/blob/master/lpiverify4_types.pl">lpiverify4_types.pl</a>
* <a href="https://github.com/luciangreen/listprologinterpreter/blob/master/lpiverify4_open.pl">lpiverify4_open.pl</a>
* <a href="https://github.com/luciangreen/listprologinterpreter/blob/master/lpiverify4_open_types.pl">lpiverify4_open_types.pl</a>

respectively.

* See note 1 above.

# Documentation

See <a href="https://github.com/luciangreen/listprologinterpreter/blob/master/LPI_docs.md">List Prolog Documentation</a> and <a href="https://github.com/luciangreen/listprologinterpreter/blob/master/LPI_Caveats.md">List Prolog Caveats</a>.

# Text to Breasonings

<a href="https://github.com/luciangreen/Text-to-Breasonings">Text to Breasonings</a> now has its own repository.

# Data to Types Documentation

See <a href="https://github.com/luciangreen/listprologinterpreter/blob/master/D2T_docs.md">Data to Types Documentation</a>.

# LPI API

* To run LPI on a Prolog server:
* Move `lpi-api.pl` to the root (`/username/` or `~` on a server) of your machine.
* Re-enter the paths to your Prolog files in it.
* Enter `[lpi-api.pl]` in SWI-Prolog and `server(8000).`.
* On a local host access the algorithm at `http://127.0.0.1:8000` and replace 127.0.0.1 with your server address.

# Diagram of List Prolog Converters

<img src="https://www.lucianacademy.com/files/Philosophy/LucianGreensPhilosophyMay2020/Diagram%20of%20List%20Prolog%20Converters.png" alt="Diagram of List Prolog Converters">

```
* Above: Cycle of Normal Prolog e.g. a(B,C):-d(E),f. to Simple List Prolog e.g. 
[[f1, [a, b, c, d, e], (:-),
[[+, [a, b, f]],
[+, [c, f, g]],
[+, [d, g, h]],
[=, [e, h]]]]]
(Prolog-to-List-Prolog Converter), to List Prolog e.g.
[
[[n,function],[[v,a],[v,b],[v,c]],":-",
[
  [[n,+],[[v,a],[v,b],[v,c]]]
  ]
]
]
(Simple-List-Prolog-to-List-Prolog) back to Normal Prolog (List-Prolog-to-Prolog-Converter).
```

See <a href="https://github.com/luciangreen/Simple-List-Prolog-to-List-Prolog">Simple-List-Prolog-to-List-Prolog</a>, <a href="https://github.com/luciangreen/Prolog-to-List-Prolog">Prolog-to-List-Prolog (includes Prolog to Simple List Prolog)</a> and <a href="https://github.com/luciangreen/List-Prolog-to-Prolog-Converter">List-Prolog-to-Prolog-Converter</a>.

# Occurs Check

* To return false if an equals4 variable is in terms of itself, for example:

```
occurs_check([v,a],[[v,a]]).
false.
```

* then enter `turn_occurs_check(on).`.  To return `true.` above, indicating any occurrences have been ignored, enter `turn_occurs_check(off).` (the default).

# List Prolog Interpreter with Prolog queries

* Run <a href="https://github.com/luciangreen/Philosophy">List Prolog Interpreter with Prolog queries</a>.

# Versioning

We will use SemVer for versioning.

# Authors

Lucian Green - Initial programmer - <a href="https://www.lucianacademy.com/">Lucian Academy</a>

# License

I licensed this project under the BSD3 License - see the LICENSE.md file for details
