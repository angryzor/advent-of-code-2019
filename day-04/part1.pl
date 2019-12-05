% Some parsing

chars_numbers([], []).
chars_numbers([Char|Chars], [Number|Numbers]) :-
	atom_string(Char, String),
	number_string(Number, String),
	chars_numbers(Chars, Numbers).

number_digits(Number, Digits) :-
	number_string(Number, String),
	string_chars(String, Chars),
	chars_numbers(Chars, Digits).

% Search code

has_adjacent([X,X|_]).
has_adjacent([X,Y|R]) :- X \= Y, has_adjacent([Y|R]).

is_incrementing([_]).
is_incrementing([X,Y|R]) :- X =< Y, is_incrementing([Y|R]).

is_valid(Number) :-
	between(172851, 675869, Number),
	number_digits(Number, Digits),
	has_adjacent(Digits),
	is_incrementing(Digits).

valid_number_count(Count) :-
	findall(Number, is_valid(Number), Numbers),
	length(Numbers, Count).
