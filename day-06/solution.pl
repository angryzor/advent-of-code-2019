:- set_prolog_flag(double_quotes, codes).
:- use_module(library(pio)).
:- use_module(library(dcg/basics)).
:- use_module(library(dcg/high_order)).

:- op(700, xfx, *>). % direct orbit
:- op(700, xfx, *>>). % transitive orbit

% parsing
orbit(A *> B) --> string(A), ")", !, string(B), "\n", !.
database(D) --> sequence(orbit, D).

% solution
transitive_path(A, B, []) :- A *> B.
transitive_path(A, C, [B|HS]) :- A *> B, transitive_path(B, C, HS).

A *>> B :- transitive_path(A, B, _).

part1(Count) :-
	findall([A,B], A *>> B, Orbits),
	length(Orbits, Count).

hop_count(A, B, Count) :-
	transitive_path(C, A, APath),
	transitive_path(C, B, BPath),
	length(APath, APathLength),
	length(BPath, BPathLength),
	Count is APathLength + BPathLength.

part2(MinimalHops) :-
	findall(Hops, hop_count("SAN", "YOU", Hops), AllHops),
	min_list(AllHops, MinimalHops).

% run it
run(Part) :-
	phrase_from_file(database(Orbits), "input.txt"),
	maplist(assert, Orbits),
	call(Part).
