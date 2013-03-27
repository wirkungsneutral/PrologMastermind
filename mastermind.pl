% Berechnet die weißen und schwarzen Pins
% +Guess: 
% +Answer:
% -Blacks:
% -Whites:
calc_guess(Guess, Answer, Blacks, Whites) :-
	calc_black(Guess, Answer, Blacks),
	Blacks == 4,
	write('Gewonnen'), nl, !.

calc_guess(Guess, Answer, Blacks, Whites) :-
	calc_black(Guess, Answer, Blacks),
	calc_white(Guess, Answer, Help),
	Whites is Help - Blacks.

% Guess List, Answer List, Number of Black Pins
calc_black([], [], Blacks) :- 
	Blacks is 0.

calc_black([GH|GR], [GH|AR], Blacks) :-
	calc_black(GR, AR, X), 
	Blacks is 1 + X.
		
calc_black([A|GR], [B|AR], Blacks) :-
	A \== B, 
	calc_black(GR, AR, Blacks).
	
% +Guess List, +Answer List, -Number of White Pins
calc_white([], _, Whites) :- 
	Whites is 0.

calc_white([GH|GR], A, Whites) :-
	member(GH, A), 
	delete_first_occurence(A, GH, AwithoutGH),
	calc_white(GR, AwithoutGH, X), 
	Whites is 1 + X.

calc_white([GH|GR], A, Whites) :- 
	nonmember(GH, A),
	calc_white(GR, A, Whites).

%List to delete Element from, Element, New List
delete_first_occurence([], _, []).	
	
delete_first_occurence([Element|LR], Element, LR).
	
delete_first_occurence([LH|LR], Element, NewList) :-
	LH \== Element,
	delete_first_occurence(LR, Element, X),
	append([LH], X, NewList).

nonmember(_,[]).
nonmember(E,[LH|LR]):- E\==LH, nonmember(E,LR).


