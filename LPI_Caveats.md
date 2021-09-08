# List Prolog Caveats

* name predicates, variables Name (name), Name1 (name1), or Name_a_1 (name_a_1) not Name_a1 (name_a1)

* Use findall, not rely on semi-colon-like separated results for non-determinism

* member2(A,B) replaces member(B,A)

* get_item_n(List,Item_number,Item) replaces append and length

* <a href="https://github.com/luciangreen/Philosophy/">lucianpl</a> doesn't currently support other languages
