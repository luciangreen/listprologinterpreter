# Data to Types Documentation

* Data to Types is an algorithm that converts data into a type statement, for example:

```
?- data_to_types([1,"a",[1]],[],T).
T = [[t, number], [t, string], [[t, brackets], [[[t, number]]]]].

?- data_to_types([1,["a"],1],[],T).
T = [[t, number], [[t, brackets], [[[t, string]]]], [[t, number]]].

?- data_to_types([[1],"a",1],[],T).
T = [[[t, brackets], [[[t, number]]]], [[t, string], [t, number]]].
```

# Getting Started

Please read the following instructions on how to install the project on your computer for converting data to types.

# Prerequisites

None.

# Installing

* Please download and install SWI-Prolog for your machine at https://www.swi-prolog.org/build/.

* Download the file <a href="https://github.com/luciangreen/listprologinterpreter/blob/master/data_to_types.pl">data_to_types.pl</a> to your machine.

* Use in the same way as the examples above.

# Authors

Lucian Green - Initial programmer - <a href="https://www.lucianacademy.com/">Lucian Academy</a>

# License

I licensed this project under the BSD3 License - see the LICENSE.md file for details