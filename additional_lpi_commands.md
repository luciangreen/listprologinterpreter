# Additional LPI Commands

 * Run Prolog in List Prolog.
 * [[n,shell_pl],[[I,QP,QV,P,OVar]]]
 * e.g. [[n,shell_pl],[[1,1],"a","A,A1","B is A+A1,B1 is A-A1,write([B,B1]).",[v,o]]]
 * I - inputs, e.g. [1,1] means A=1, A1=1
 * QP - query predicate, e.g. "a", which is used in the call and to build the predicate.
 * QV - query variables, e.g. "A,A1", which are used in the call and to build the predicate.
 * P - e.g. "B is A+A1,B1 is A-A1,write([B,B1]).", the body of the predicate. Convert result to an atom with term_to_atom/2 (not in example because it is not necessary). Return output from Prolog with write/1.
 * OVar - e.g. [v,o], the List Prolog variable to return output to. Take care to return the correct variable because it is hard to debug shell commands.
 ---
 * [[n,phrase_from_file],[[[n,string],[[v,out]]],[v,path]]]
 * [v,out] - replace with variable to save string to
 * [v,path] - atom file path to read
 ---
 * [[n,text_area
],["rows=\\"4\\" style=\\"width:100%\\"","a",[v,s]]]
 * "rows=\\"4\\" style=\\"width:100%\\"" - text area parameters - NB. Change two backslashes to one when copying
 * "a" - replace with value already in field
 * [v,s] - text entered in field
---
 * [[n,date_time_stamp],[Year,Month,Day,Hour,Minute,Seconds,Seconds2,Variable1]],
 * Converts the date and time to a number stamp
---
 * 	
[[n,interpret],[[lang,"en"/same],debug is on/off/same,[[n,predicate],[[v,variable],...]],Predicates]] - Legacy List Prolog call for SSI with language code, debug or trace mode, predicate and variable query and predicates to call.
 * For example, https://github.com/luciangreen/Philosophy/blob/master/paraphraser1_lp.pl
---
* In the following, ["predicate_name",[i,o]] is called as [[n,predicate_name],[[v,i],[v,o]]]

 ["word1",[i]],

 ["term_to_atom",[i,o]],
 ["term_to_atom",[o,i]],
 ["term_to_atom",[i,i]],

 ["shell",[i]],
 
 ["read_password",[i]],
 ["read_password",[o]],

 ["string_chars",[i,i]],
 ["string_chars",[i,o]],
 ["string_chars",[o,i]],
 ["atom_chars",[i,i]],
 ["atom_chars",[i,o]],
 ["atom_chars",[o,i]],
 ["atom_codes",[i,i]],
 ["atom_codes",[i,o]],
 ["atom_codes",[o,i]],
 ["atom_concat",[i,i,i]],
 ["atom_concat",[i,o,i]],
 ["atom_concat",[o,i,i]],
 ["atom_concat",[o,o,i]],
 ["atom_concat",[i,i,o]],
 ["atomic",[i]],
 ["atom_length",[i,i]],
 ["atom_length",[i,o]],
 ["sub_atom",[i,i,i,i,i]],
 ["sub_atom",[i,i,i,i,o]],
 ["sub_atom",[i,i,i,o,i]],
 ["sub_atom",[i,i,i,o,o]],
 ["sub_atom",[i,i,o,i,i]],
 ["sub_atom",[i,i,o,i,o]],
 ["sub_atom",[i,i,o,o,o]],
 ["sub_atom",[i,o,i,i,i]],
 ["sub_atom",[i,o,i,i,o]],
 ["sub_atom",[i,o,i,o,i]],
 ["sub_atom",[i,o,i,o,o]],
 ["sub_atom",[i,o,o,i,i]],
 ["sub_atom",[i,o,o,i,o]],
 ["sub_atom",[i,o,o,o,i]],
 ["sub_atom",[i,o,o,o,o]],
 ["sub_string",[i,i,i,i,i]],
 ["sub_string",[i,i,i,i,o]],
 ["sub_string",[i,i,i,o,i]],
 ["sub_string",[i,i,i,o,o]],
 ["sub_string",[i,i,o,i,i]],
 ["sub_string",[i,i,o,i,o]],
 ["sub_string",[i,i,o,o,o]],
 ["sub_string",[i,o,i,i,i]],
 ["sub_string",[i,o,i,i,o]],
 ["sub_string",[i,o,i,o,i]],
 ["sub_string",[i,o,i,o,o]],
 ["sub_string",[i,o,o,i,i]],
 ["sub_string",[i,o,o,i,o]],
 ["sub_string",[i,o,o,o,i]],
 ["sub_string",[i,o,o,o,o]],

 ["char_code",[i,i]],
 ["char_code",[i,o]],
 ["char_code",[o,i]],
 ["number_chars",[i,i]],
 ["number_chars",[i,o]],
 ["number_chars",[o,i]],
 ["number_codes",[i,i]],
 ["number_codes",[i,o]],
 ["number_codes",[o,i]],
 ["close",[i,i]],
 ["close",[i]],
 ["stream_property",[i,i]],
 ["stream_property",[i,o]],
 ["stream_property",[o,i]],
 ["stream_property",[o,o]],

 ["at_end_of_stream",[]],
 ["at_end_of_stream",[i]],
 ["set_stream_position",[i,i]],
 ["compound",[i]],
 ["copy_term",[i,i]],
 ["copy_term",[i,o]],
 ["copy_term",[o,i]],
 ["copy_term",[o,o]],
 ["current_prolog_flag",[i,i]],
 ["current_prolog_flag",[i,o]],
 ["current_prolog_flag",[o,i]],
 ["current_prolog_flag",[o,o]],
 ["current_input",[i]],
 ["current_input",[o]],
 ["current_output",[i]],
 ["current_output",[o]],
 ["float",[i]],
 ["get_byte",[i,i]],
 ["get_byte",[i,o]],
 ["get_byte",[i]],
 ["get_byte",[o]],
 ["peek_byte",[i,i]],
 ["peek_byte",[i,o]],
 ["peek_byte",[i]],
 ["peek_byte",[o]],
 ["put_byte",[i,o]],
 ["put_byte",[o,o]],
 ["put_byte",[i]],
 ["put_byte",[o]],
 
 ["peek_char",[i,i]],
 ["peek_char",[i,o]],
 ["peek_char",[i]],
 ["peek_char",[o]],

 ["peek_code",[i,i]],
 ["peek_code",[i,o]],
 ["peek_code",[i]],
 ["peek_code",[o]],
 
 ["get_char",[i]],
 ["get_char",[o]],
 ["get_char",[i,i]],
 ["get_char",[i,o]],
 ["get_code",[i]],
 ["get_code",[o]],
 ["get_code",[i,i]],
 ["get_code",[i,o]],
 
 ["halt",[]],
 ["halt",[i]],

 ["set_prolog_flag",[i,i]],
 ["integer",[i]],
 ["set_input",[i]],
 ["set_output",[i]],
 ["open",[i,i,o,i]],
 ["open",[i,i,o]],
 ["read",[i,o]],
 ["read",[i,i]],
 ["nonvar",[i]],

 ["sin",[i,i]],
 ["sin",[i,o]],
 ["cos",[i,o]],
 ["cos",[i,i]],
 ["atan",[i,i]],
 ["atan",[i,o]],
 ["log",[i,i]],
 ["log",[i,o]],
 ["sqrt",[i,i]],
 ["sqrt",[i,o]],

 ["put_char",[i,i]],
 ["put_char",[i]],
 ["put_code",[i,i]],
 ["put_code",[i]],
 ["nl",[]],
 ["nl",[i]],
 
 ["read_term",[i,i,i]],
 ["read_term",[i,o,i]],
 ["read_term",[i,i]],
 ["read_term",[i,o]],
 ["read",[i,i]],
 ["read",[i,o]],
 ["read",[i]],
 ["read",[o]],

 ["write",[i,i]],
 ["write",[i]],
 ["writeq",[i,i]],
 ["writeq",[i]],

 ["abs",[i,i]],
 ["abs",[i,o]],
 ["sign",[i,i]],
 ["sign",[i,o]],
 ["floor",[i,i]],
 ["floor",[i,o]],
 ["round",[i,i]],
 ["round",[i,o]]
