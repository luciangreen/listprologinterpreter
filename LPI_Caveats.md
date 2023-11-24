# List Prolog Caveats

* Name predicates, variables Name (name), Name1 (name1), or Name_a_1 (name_a_1) not Name_a1 (name_a1).

* Use findall, not rely on semi-colon-like separated results for non-determinism.

* member(Item,List) in List Prolog now uses the same order of arguments as Prolog. member2(List, Item) can be used in CAW.

* get_item_n(List,Item_number,Item) replaces append and length (the programmer must copy this algorithm into their algorithm).

* <a href="https://github.com/luciangreen/Philosophy/">lucianpl</a> doesn't currently support other languages.
