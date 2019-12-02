# TextToBreasoning Documentation

* TextToBreasoning is a set of algorithms that allow breasoning out (in this case, the computer thinks of the x, y and z dimensions of) objects to help earn high distinctions, have healthy children, earn jobs, sell products, <a href="https://github.com/luciangreen/mindreader">read minds, display spiritual screens</a> or time travel.

* TextToBreasonings can (1) breason out words over a local machine and a VPS host, (2) breason out characters over a local machine or (3) breason out words, breathsonings (judgement adjectives and adverbs), rooms, parts of rooms, directions in rooms, objects to prepare for an action and objects to finish an action over a local machine.

* Generally, 80 word breasonings are needed to earn a high distinction at undergraduate level and below, have healthy children or sell products.  This increases to 2*15*80=2400 breasonings per Honours level chapter, approximately 2*50*80=8000 breasonings per Masters level assignment and approximately 2*4*50*80=32,000 breasonings per PhD level assignment.

* 50 As (50*80=4000 breasonings) are required to earn a job.

* A time machine is being developed.

# Getting Started

Please read the following instructions on how to install the project on your computer for writing code.

# Prerequisites

### Caution:

follow instructions in <a href="https://github.com/luciangreen/listprologinterpreter/blob/master/Instructions_for_Using_texttobr(2).pl.txt">Instructions for Using texttobr(2)</a> when using texttobr, texttobr2 or mind reader to avoid medical problems.

# Installing and Running

* Please download and install SWI-Prolog for your machine at https://www.swi-prolog.org/build/.

* Download the repository to your machine.

* (1) To breason out words over a local machine and a VPS host, in the SWI-Prolog environment, enter:
`[listprolog].`

* Please follow the instructions in <a href="https://github.com/luciangreen/listprologinterpreter/blob/master/Setting_up_a_VPS_with_TextToBr.txt">"Setting up a VPS with TextToBr.txt"</a> to set up a VPS with TextToBr up to and including "Upload them to the VPS's...".

* In the SWI-Prolog environment, enter:
`texttobr2(N,File,String,M).`
* where N is the number of times to breason out the file, File is the file name, String is the string to breason out and M is the number of words in the file to breason out, e.g.:
`texttobr2(u,"file.txt",u,u).`
* Breasons out file.txt.
`texttobr2(2,"file.txt",u,u).`
* Breasons out file.txt twice.
`texttobr2(u,u,"Hello world.",u).`
* Breason out "Hello world.".
`texttobr2(3,u,"a b c",2).`
* Breasons out the first two words in "a b c" ("a" and "b") 3 times.

* (2) To breason out characters over a local machine, in the SWI-Prolog environment, enter:
`[texttobr].`

* In the SWI-Prolog environment, enter:
`texttobr(N,File,String,M).`
* where N is the number of times to breason out the file, File is the file name, String is the string to breason out and M is the number of words in the file to breason out, e.g.:
`texttobr(u,"file.txt",u,u).`
* Breasons out file.txt.
`texttobr(2,"file.txt",u,u).`
* Breasons out file.txt twice.
`texttobr(u,u,"Hello world.",u).`
* Breason out "Hello world.".
`texttobr(3,u,"abc",2).`
* Breasons out the first two characters in "abc" ("a" and "b") 3 times.

* (3) To breason out words, breathsonings (judgement adjectives and adverbs), rooms, parts of rooms, directions in rooms, objects to prepare for an action and objects to finish an action over a local machine, in the SWI-Prolog environment, enter:
`[load_texttobr].`

* In the SWI-Prolog environment, enter:
`texttobr2(u,u,u,u,false,false,false,false,false,false).`
* where the first four arguments may be changed as in (1) above, and only words are breasoned out.
`texttobr2(u,u,u,u,true,false,false,false,false,false).`
* where the first four arguments may be changed as in (1) above, and only words and breathsonings are breasoned out.
`texttobr2(u,u,u,u,true,true,true,true,true,true).`
* where the first four arguments may be changed as in (1) above, and words, breathsonings (judgement adjectives and adverbs), rooms, parts of rooms, directions in rooms, objects to prepare for an action and objects to finish an action are breasoned out.

# Versioning

We will use SemVer for versioning.

# Authors

Lucian Green - Initial programmer - <a href="https://www.lucianacademy.com/">Lucian Academy</a>

# License

I licensed this project under the BSD3 License - see the LICENSE.md file for details