:- include(bb_planner).

weight(mum,60).
weight(dad,85).
weight(alfie,20).
weight(bianca,25).
weight(daisy,50).
weight(puppy,10).

total_weight([],0).
total_weight([H|T], W):-total_weight(T,X), weight(H,N),W is N+X.

subset1([], []).
subset1([H|T], [H|NextT]):- subset1(T, NextT).
subset1([_|T], NextT):- subset1(T, NextT).

weight_limited_selection(L, Max, S):- subset1(L,S),total_weight(S,X), X =<Max.

remove_boat( _, [], []).
remove_boat( R, [R|T], T).
remove_boat( R, [H|T], [H|T2]) :- H \= R, remove_boat( R, T, T2).

remove_members([], _, []).
remove_members([X|Tail], L2, Result):- member(X, L2), !, remove_members(Tail, L2, Result).
remove_members([X|Tail], L2, [X|Result]):- remove_members(Tail, L2, Result).

add_boat(X,L,[X|L]).

combine([], B, B).
combine(A, [], A).
combine([X|A], [X|B], [X|C]) :- combine(A, B, C).
combine([X|A], [Y|B], [X|C]) :- dif(X,Y), combine(A, [Y|B], C).

initial_state(  [ [i,     [] ], [boat, puppy, alfie, daisy, dad, bianca, mum]= []  ]).
goal_state(  [ [lr,     _ ], []=[boat, alfie, bianca, dad, daisy, mum, puppy]  ]).

% when nobody on right side, boat on left side, people cross from lr
transition( [[i, _], L1=R1], [[lr, Cross1], L2=R2] ) :- cross(Cross1,L1,R1,L2,R2).
% when somebody on right side, boat on right side, people cross from rl
transition( [[lr, _], L1=R1], [[rl, Cross1], L2=R2] ) :- cross(Cross1,L1,R1,L2,R2).
% when somebody on right side, boat on right side, people cross from rl
transition( [[rl, _], L1=R1], [[lr, Cross1], L2=R2] ) :- cross(Cross1,L1,R1,L2,R2).

% Conditions for boat crossing :
boat_condition(Cross1):-(   (\+member(bianca, Cross1)) ; ( member(B, Cross1), bianca\=B) ), % Bianca can't cross alone
    					(   (\+member(daisy, Cross1))  ; ( member(D, Cross1), daisy\=D) ), % Daisy can't cross alone
    					(   (\+member(alfie, Cross1))  ; ( member(mum, Cross1);  member(dad,Cross1)) ), % Alfie can only cross w/ Mum or Dad
    					(   (\+member(puppy, Cross1))  ; ( member(daisy, Cross1); member(bianca, Cross1)) ), % puppy can only cross w/ daisy or bianca
    					(   total_weight(Cross1,W), W >0 ), !.

% Conditions for who can be on either side of river:
side_condition(S) :- 	(   (\+member(alfie, S)) ; ( member(C, S), alfie\=C) ), % Alfie can't be alone on river side
    					(   (\+member(puppy, S)) ; ( member(A, S), puppy\=A) ), % Puppy can't be alone on river side
    					(   (\+member(daisy, S)) ; ( member(B, S), daisy\=B) ), % Daisy can't be alone on river side
    					(   (\+member(puppy, S), \+member(alfie, S)) ; ( member(D, S), alfie\=D,puppy\=D) ), !. % alfie and puppy can't be together alone

% cross predicate for boat is on the left side,
% and family members can only cross from left to right
cross( Cross1,L1, R1, L2, R2 ) :-
    member(boat, L1), weight_limited_selection(L1, 100, Cross1),  % find Cross1 - those who are crossing on the boat from lr
    remove_members(L1,Cross1,LBuff),remove_boat(boat,LBuff,L2),  % find L2 - those remained on left side
    side_condition(L2),
    combine(Cross1,R1,RBuff),sort(RBuff,RBuff2),add_boat(boat,RBuff2,R2),  % find R2 - those crossed to right side
    side_condition(R2).

% cross predicate for boat is on the right side,
% and family members can only cross from right to left
cross( Cross1,L1, R1, L2, R2 ) :-
    member(boat, R1), weight_limited_selection(R1, 100, Cross1),% find Cross1 - those who are crossing on the boat from rl
    remove_members(R1,Cross1,RBuff),remove_boat(boat,RBuff,R2), % find R2 - those remained on right side
    side_condition(R2),
    combine(Cross1,L1,LBuff),sort(LBuff,LBuff2),add_boat(boat,LBuff2,L2), % find L2 - those who crossed to left side
    side_condition(L2).

legal_state( [[_,J], _=_] ):- boat_condition(J).

equal_set(X, Y) :- permutation(X, Y),!.

equivalent_states( [[_,_], A1=_],[[_,_], A2=_] ):- equal_set(A1,A2).
equivalent_states( [[_,_], _=B1],[[_,_], _=B2] ):- equal_set(B1,B2).

loopcheck(on).                  % Don't allow search to go into a loop. 
