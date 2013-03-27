
%http://www.cs.oswego.edu/~odendahl/coursework/notes/prolog/synopsis/con.html
a([]) :- give_random_color(C),print_color(C).

println(A):-write(A),nl.
%print(A):-write(A).
%test([]) : give_white([1,3,3,1],[1, 2, 2 ,2],).

give_random_color(C):- random_between(1,6,C).

print_color(1):- write('green').
print_color(2):- write('yellow').
print_color(3):- write('red').
print_color(4):- write('blue').
print_color(5):- write('pink').
print_color(6):- write('orange').

print_try(A):-write('['),print_try_h(A).
print_try_h([]):- 
	println(']').
print_try_h([AH|[]]):- 
	print_color(AH),
	println(']').
print_try_h([AH|AR]):-
	print_color(AH),
	write(', '),
	print_try_h(AR)
.	

fight(Answer,MaxAttempts):-
	init(Answer,FirstAttempt), 
	Attempts is MaxAttempts - 1, 
	fight_loop(Answer, [FirstAttempt], Attempts)
. 

init(Answer,FirstAttempt):- 
	write('[init] Correct answer: '),
	print_try(Answer),
	write('[init] Length: '),
	length(Answer,L),
	println(L),
	gen_random(L,FirstAttempt1),
	write('[init] Testing: '),
	print_try(FirstAttempt1),
	calc_guess(FirstAttempt1, Answer, B, W),
	write('[init] Black: '),write(B),write('  White: '),println(W),
	append([FirstAttempt1],[B,W],FirstAttempt),
	!
.

 
fight_loop(_,_,0):- 
	println('[fight_loop] I lost :('),
	!
.

fight_loop(Answer, AlreadyTried, AttemptsLeft) :- 
	write('[fight_loop] Attempts Left: '),
	writeln(AttemptsLeft), 
	Attempts is AttemptsLeft - 1,
	get_possible_answer(Guess,AlreadyTried),
	write('[fight_loop] I guess: '),
	print_try(Guess),
	calc_guess(Guess, Answer, B,W), 
	write('[fight_loop] Black: '),write(B),write('  White: '),println(W),
	fight_loop(Answer,[AlreadyTried,[Guess,B,W]],Attempts),
	!
.


get_possible_answer(_,[]). %:-println('End of life').
get_possible_answer(Guess, [[PreviousTry,Blacks,Whites]|ATR]):-
	%println(Guess), 
	Guess\==PreviousTry,
	%println(Guess),
	calc_guess(Guess,PreviousTry,Blacks,Whites),
	calc_guess(Guess,PreviousTry,Blacks,Whites),
	%println(Guess), 
	get_possible_answer(Guess,ATR),
	%println(Guess),	
	number_constraint(Guess)
. 

number_constraint([]).
number_constraint([GH|GR]):-
	GH #<7,
	GH #>0, labeling([],[GH]),
	number_constraint(GR)
.

check_h(Guess,Answer,B,W):-
	check(Guess,Answer,Answer,[],W,B)
.
check([],_,_,H,W,0):-length(H,W).
check([E|GR],[E|AT],A,H,W,B):-
	delete_first_occurence(H, E, H1),
	check(GR,AT,A,H1,W,B1),
	B is B1 + 1
.
check([GH|GR],[AH|AT],A,H,W,B):-
	GH\==AH,
	member(GH,A),
	delete_first_occurence(A, GH, A1),
	append([GH],H,H1),
	check(GR,AT,A1,H1,W,B)
.
check([GH|GR],[AH|AT],A,H,W,B):-
	GH\==AH,
	nonmember(GH,A),
	check(GR,AT,A,H,W,B)
.
	

gen_random(0,[]).
gen_random(Length, R):-
	L is Length -1,	
	gen_random(L,R1),
	give_random_color(C),
	append([C],R1,R)
.
 
%delete_first([],_,[]).
%delete_first([E|LR],E,LR).
%delete_first([LH|LR],E,R):- 	
%	LH \== E,
%	delete_first(LR,E,R1),
%	append([LH],R1,R).
	
nonmember(_,[]).	
nonmember(E,[LH|LR]):- E\==LH, nonmember(E,LR).

% Berechnet die weissen und schwarzen Pins
% +Guess: 
% +Answer:
% -Blacks:
% -Whites:
%calc_guess(Guess, Answer, Blacks, _) :-
%	calc_black(Guess, Answer, Blacks),
%	Blacks == 4,
%	println('Gewonnen'), !.

calc_guess(Guess, Answer, Blacks, Whites) :-
	calc_black(Guess, Answer, Blacks),
%	println(Guess),
%	println(Blacks),
	calc_white(Guess, Answer, Help),
%	println(Help),
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

