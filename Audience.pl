main :- generator3(X), tester3(X), write(X).


% Generator 3 Test Predicate
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

x_generator3( N ) :- 
    x_generator3_loop(
    [ 1024 , 9409 , 23716 , 51529 , 123904 , 185761 , 868624 , 962361
    , 982081, 1000000 ], 0, N ).
    x_generator3_loop( [], C, C ). 
x_generator3_loop( [T|TS], C, N ) :-
    generator3( T ),
    C1 is C + 1,
    x_generator3_loop( TS, C1, N ). 
x_generator3_loop( [_|TS], C, N ) :-
    x_generator3_loop( TS, C, N ).


% Tester 3 Test Predicate
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

x_tester3( N ) :- 
    x_tester3_loop(
    [ 123056 , 128036 , 139076 , 142076 , 148056 , 159076 , 173096 , 189036
    , 193056, 198076 ], 0, N ).
x_tester3_loop( [], C, C ). 
x_tester3_loop( [T|TS], C, N ) :-
    tester3( T ),
    C1 is C + 1,
    x_tester3_loop( TS, C1, N ).


% Code For Easier To Ask The Asudience
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

generator3(X) :-
    between(1000, 1000000, X), perfect_square(X, 1).

perfect_square(N, M) :-
    N is M * M.
perfect_square(N, M) :-
    M * M < N, 
    M1 is M + 1, 
    perfect_square(N, M1).

is_odd(N) :-
    1 is N mod 2.

numbers_to_list(N, [N]) :-
    N < 10.
numbers_to_list(N, X) :-
    N >= 10, 
    div_mod(N, 10, D, M), 
    numbers_to_list(D, R), 
    append(R, [M], X).

div_mod(A, B, D, M) :-
    D is A div B, 
    M is A mod B.

include_0(L) :-
    member(0, L).


    
get_last([X],X).
get_last([_|T],R):- last(T,R).

get_first([X],X).
get_first([H|_], H).

get_second([X],X).
get_second([_, H2|_], H2).

get_third([X],X).
get_third([_, _, H3|_], H3).

get_last_but_one([E, _], E).
get_last_but_one([_|T], X) :-
    get_last_but_one(T, X).

different([]).
different([H|T]) :-
    \+ member(H, T), different(T).

multiple(N, X, C) :-
    N * C =:= X.
multiple(N, X, C) :-
    N * C < X,
    C1 is C + 1, 
    multiple(N, X, C1).


not_zero(N) :-
    N =\= 0.

tester3(X) :- % it should check different and include 0 with their actual numbers then turn the thing into list
    numbers_to_list(X, XS), 
    different(XS), 
    include_0(XS), 
    get_last_but_one(XS, Last_But_One),
    is_odd(Last_But_One), 
    get_last(XS, Last),
    length(XS, Len),
    Last is Len,
    get_first(XS, First), 
    get_second(XS, Second), 
    get_third(XS, Third), 
    not_zero(First),
    not_zero(Second),
    not_zero(Third),
    not_zero(Last_But_One),
    multiple(First, Second, 1),
    multiple(First, Third, 1),
    multiple(First, Last_But_One, 1).