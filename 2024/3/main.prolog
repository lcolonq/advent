#!/usr/bin/env swipl

:- initialization(main, main).

parse_until([], Left, Sep, [Sep | Left]) :- !.
parse_until(Res, Left, Sep, [X | Rest]) :-
    parse_until(Sub, Left, Sep, Rest), !,
    append([X], Sub, Res).

parse_num_until(Num, Left, Sep, Inp) :-
    parse_until(AL, Left, Sep, Inp),
    atomic_list_concat(AL, ANum),
    atom_number(ANum, Num).

parse1(0, [], []) :- !.
parse1(Res, Left, [m, u, l, '(' | Rest]) :-
    parse_num_until(Num0, L0, ',', Rest),
    parse_num_until(Num1, L1, ')', L0), !,
    parse1(Sub, Left, L1), !,
    Res is (Num0 * Num1) + Sub.
parse1(Res, Left, [_ | Rest]) :- parse1(Res, Left, Rest), !.

parse2(0, [], _, []) :- !.
parse2(Res, Left, on, [m, u, l, '(' | Rest]) :-
    parse_num_until(Num0, L0, ',', Rest),
    parse_num_until(Num1, L1, ')', L0), !,
    parse2(Sub, Left, on, L1), !,
    Res is (Num0 * Num1) + Sub.
parse2(Res, Left, _, [d, o, '(', ')' | Rest]) :-
    parse2(Res, Left, on, Rest), !.
parse2(Res, Left, _, [d, o, n, '\'', t, '(', ')' | Rest]) :-
    parse2(Res, Left, off, Rest), !.
parse2(Res, Left, State, [_ | Rest]) :-
    parse2(Res, Left, State, Rest), !.

main(_Argv) :-
    read_file_to_string("input.txt", Inp, []),
    string_chars(Inp, InpChars),
    parse1(P1, _, InpChars),
    parse2(P2, _, on, InpChars),
    format('~w~n~w~n', [P1, P2]).
