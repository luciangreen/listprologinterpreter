%% mergetexttobrdict.pl

/** logs in, renames, 
copies bdrict1.txt and 2.txt to 1vps.vps and 2vps.txt
Loads dictionaries, uses term_to_atom to turn into term
Appends to home dictionaries
Runs texttobr
Uploads new dictionaries to vps
rename
**/

%% [mergetexttobrdict],
%% [edit].
%% mergetexttobrdict.

%%:- include('edit.pl').

use_module(library(pio)).
use_module(library(dcg/basics)).

mergetexttobrdict :-
	prolog_edit:run1.