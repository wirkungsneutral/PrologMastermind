% Berechnet die weißen und schwarzen Pins
% +Guess: 
% +Answer:
% -Blacks:
% -Whites:
calc_guess(Guess, Answer, Blacks, Whites) :-
	calc_black(Guess, Answer, Blacks),
	calc_white(Guess, Answer, Help),
	Whites is Help - Blacks.

% G = Guess, A = Answer
calc_black([], [], Blacks) :- 
	Blacks is 0.

calc_black([GH|GR], [GH|AR], Blacks) :-
	calc_black(GR, AR, X), 
	Blacks is 1 + X.
		
calc_black([A|GR], [B|AR], Blacks) :-
	A \== B, 
	calc_black(GR, AR, Blacks).
	

calc_white([], _, Whites) :- Whites is 0.

calc_white([GH|GR], A, Whites) :-
	member(GH, A), 
	delete(A, GH, AwithoutGH),
	calc_white(GR, AwithoutGH, X), 
	Whites is 1 + X.

calc_white([GH|GR], A, Whites) :- 
	not(member(GH, A)),
	calc_white(GR, A, Whites).



