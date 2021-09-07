# List Prolog Caveats

* name predicates, variables Name (name), or Name_1 (name_1) not Name1 (name1)

* Use findall, not rely on semi-colon-like separated results for non-determinism

* member2(A,B) replaces member(B,A)

* get_item_n(List,Item_number,Item) replaces append and length

* lucianpl (at https://github.com/luciangreen/Philosophy) doesn't currently support other languages
