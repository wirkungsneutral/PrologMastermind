%http://www.cs.oswego.edu/~odendahl/coursework/notes/prolog/synopsis/con.html
a([]) :- give_random_color(C),print_color(C).

println(A):-write(A),nl.
print(A):-write(A).
%test([]) : give_white([1,3,3,1],[1, 2, 2 ,2],).

give_random_color(C):- random_between(1,6,C).

print_color(1):- print('green').
print_color(2):- print('yellow').
print_color(3):- print('red').
print_color(4):- print('blue').
print_color(5):- print('pink').
print_color(6):- print('orange').

print_try(A):-print('['),print_try_h(A).
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
	print('[init] Correct answer: '),
	println(Answer),
	print('[init] Length: '),
	length(Answer,L),
	println(L),
	gen_random(L,FirstAttempt),
	print('[init] Testing: '),
	print_try(FirstAttempt)
.
 
fight_loop(_,_,0):- 
	println('[fight_loop] I lost :('),!
.

fight_loop(Answer, AlreadyTried, AttemptsLeft) :- 
	print('[fight_loop] Attempts Left:'),nl,
	Attempts is AttemptsLeft - 1,
	fight_loop(Answer,AlreadyTried,Attempts)
.

gen_random(0,[]).
gen_random(Length, R):-
	L is Length -1,	
	gen_random(L,R1),
	give_random_color(C),
	append([C],R1,R)
.
 
delete_first([],_,[]).
delete_first([E|LR],E,LR).
delete_first([LH|LR],E,R):- 	
	LH \== E,
	delete_first(LR,E,R1),
	append([LH],R1,R).
	
nonmember(_,[]).	
nonmember(E,[LH|LR]):- E\==LH, nonmember(E,LR).