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

adjacent_prefix([X], [X], []).
adjacent_prefix([X,X|R], [X|S], T) :- adjacent_prefix([X|R], S, T).
adjacent_prefix([X,Y|R], [X], [Y|R]) :- X \= Y.

has_adjacent([X,X|R]) :-
	adjacent_prefix([X,X|R], Prefix, _),
	length(Prefix, 2).
has_adjacent([X,X|R]) :-
	adjacent_prefix([X,X|R], Prefix, Postfix),
	length(Prefix, Length),
	Length \= 2,
	has_adjacent(Postfix).
has_adjacent([X,Y|R]) :- X \= Y, has_adjacent([Y|R]).

is_valid(Number) :-
	between(172851, 675869, Number),
	number_digits(Number, Digits),
	has_adjacent(Digits),
	msort(Digits, Digits).

valid_number_count(Count) :-
	findall(Number, is_valid(Number), Numbers),
	length(Numbers, Count).
