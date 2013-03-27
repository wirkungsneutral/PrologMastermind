%http://www.cs.oswego.edu/~odendahl/coursework/notes/prolog/synopsis/con.html
a([]) :- give_random_color(C),print_color(C).

%test([]) : give_white([1,3,3,1],[1, 2, 2 ,2],).

give_random_color(C):- random_between(1,6,C).

print_color(1):- write(green).
print_color(2):- write(yellow).
print_color(3):- write(red).
print_color(4):- write(blue).
print_color(5):- write(pink).
print_color(6):- write(orange).

% Weiss = Vorhanden aber an anderer Stelle
% Schwarz = Vorhanden an richtiger Stelle
%give_score(Guess,Answer,Black, White) :- .

%give_white_help([GH|GR],[GH|AR],WhiteHelp):-

	
search_and_remove([F|LR],F,R):- R = LR.
search_and_remove([LH|LR],F,R):- search_and_remove(LR,F,R1),append(R1,[LH],R).
  
give_black([],[],Black):- Black = 0.
give_black([GH|GR],[AH|AR],Black):- 
	AH == GH, 
	give_black(GR,AR,Black1), 
	Black is Black1+1.
give_black([GH|GR],[AH|AR],Black):- 
	GH \== AH, 
	give_black(GR,AR,Black).

reverse([],B,C) :- C=B.
reverse([AH|AR],B,C) :- append([AH],B,Z), reverse(AR,Z,C).
