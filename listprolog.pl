:- op(600, yfx, ':').
:- op(500, xfx, '&').
:- op(500, xfx, '•').

:-set_prolog_flag(stack_limit, 40000000000).

:-style_check(-discontiguous).
:-style_check(-singleton).

% List Prolog Interpreter loads files from these repositories, which must be
% cloned beside it (in the same parent folder). Say clearly which are missing
% before the includes below fail one by one.
lpi_dependency('Languages').
lpi_dependency('SSI').
lpi_dependency('Philosophy').
lpi_dependency('List-Prolog-to-Prolog-Converter').
lpi_dependency('Prolog-to-List-Prolog').
lpi_dependency('culturaltranslationtool').

lpi_dependency_file('Languages', 'make_docs.pl').
lpi_dependency_file('SSI', 'find_pred_sm.pl').
lpi_dependency_file('Philosophy', 'sub_term_with_address.pl').
lpi_dependency_file('List-Prolog-to-Prolog-Converter', 'lp2pconverter2.pl').

lpi_check_dependencies(Dir) :-
    findall(Missing,
            ( lpi_dependency(Repo),
              atomic_list_concat([Dir, '/../', Repo], RepoDir),
              (   \+ exists_directory(RepoDir)
              ->  Missing = repo(Repo)
              ;   lpi_dependency_file(Repo, File),
                  atomic_list_concat([RepoDir, '/', File], Path),
                  \+ exists_file(Path),
                  Missing = file(Repo, File)
              ) ),
            Missing),
    (   Missing == []
    ->  true
    ;   format(user_error, "~nList Prolog Interpreter: some files it loads are missing.~n", []),
        forall(member(repo(R), Missing),
               format(user_error, "  Missing repository ~w. Clone it beside listprologinterpreter:~n    git clone https://github.com/luciangreen/~w~n", [R, R])),
        forall(member(file(R, F), Missing),
               format(user_error, "  Missing file ~w in ../~w (update that repository).~n", [F, R])),
        nl(user_error)
    ).

:- prolog_load_context(directory, Dir), lpi_check_dependencies(Dir).

:-include('grammar.pl').
%%:-include('lpi_caw_commands.pl').
:-include('listprologinterpreter1listrecursion4.pl').
:-include('listprologinterpreter3preds5.pl').
:-include('lpiverify4.pl').
:-include('lpiverify4_open.pl').
:-include('lpiverify4_types.pl').
:-include('lpiverify4_open_types.pl').
:-include('lpiverify_pl.pl').
%%:-include('caw5 copy 12.pl').
%%:-include('cawpverify.pl').
%%:-include('rcawp.pl').
%%:-include('rcaw.pl').
%%:-include('texttobr2.pl').
:-include('la_strings.pl').
:-include('la_string_codes.pl').
:-include('la_maths.pl').
:-include('la_files.pl').
:-include('la_strings_string.pl').
%:-include('../Languages/lang_db_generator.pl'). % leave off, run separately through Languages
%:-include('lpiverify4-fr.pl').
:-include('operators.pl').
:-include('lpiverify4_test_lang_all.pl').
:-include('lpiverify4_test_bt_lang_all.pl').
:-include('../Languages/make_docs.pl').
:-include('../SSI/find_pred_sm.pl').
:-include('e4_fa_get_vals.pl').
%:-include('equals4_first_args.pl').
:-include('expression_not_var.pl').
:-include('collect_arguments.pl').
:-include('reserved_words2.pl').
:-include('expand_types.pl').
:-include('replace_in_term.pl').
:-include('preds_converters_and_matrices.pl').
%:-include('numbers_of_items_correspond.pl').
:-include('match_get_put_vals.pl').
%:-include('insert_cuts.pl').
:-include('../Philosophy/sub_term_with_address.pl').
:-include('../List-Prolog-to-Prolog-Converter/lp2pconverter2.pl').
:-include('la_terms.pl').
:-include('convert_to_starlog.pl').
:-include('la_io.pl').
:-include('la_diff.pl').
:-include('la_predicates.pl').
