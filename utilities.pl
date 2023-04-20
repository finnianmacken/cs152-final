remove_from_list(Es0, Ws, Es) :-
        phrase(remove_(Ws, 0, Es0), Es).

% uses dcgs for sequence generation, removing specified elements from a list
remove_([], _, Es) --> seq(Es).
remove_([W|Ws], Pos0, [E|Es]) -->
        { Pos #= Pos0 + 1,
          zcompare(R, W, Pos0) },
        remove_at_pos0(R, E, [W|Ws], Ws1),
        remove_(Ws1, Pos, Es).

remove_at_pos0(=, _, [_|Ws], Ws) --> [].
remove_at_pos0(>, E, Ws0, Ws0) --> [E].


% taken from the flatten predicate from the list module: https://www.swi-prolog.org/pldoc/doc/_SWI_/library/lists.pl?show=src#flatten/2
flatten(List, FlatList) :-
    flatten(List, [], FlatList0),
    !,
    FlatList = FlatList0.
    
flatten(Var, Tl, [Var|Tl]) :-
    var(Var),
    !.

flatten([], Tl, Tl) :- !.
flatten([Hd|Tl], Tail, List) :-
    !,
    flatten(Hd, FlatHeadTail, List),
    flatten(Tl, Tail, FlatHeadTail).
flatten(NonList, Tl, [NonList|Tl]).

% https://stackoverflow.com/questions/18337235/can-you-write-between-3-in-pure-prolog
between(N, M, K) :- N =< M, K = N.
between(N, M, K) :- N < M, N1 is N+1, between(N1, M, K).

% https://www.swi-prolog.org/pldoc/doc/_SWI_/library/apply.pl?show=src#exclude/3
exclude(Goal, List, Included) :-
    exclude_(List, Goal, Included).

exclude_([], _, []).
exclude_([X1|Xs1], P, Included) :-
    (   call(P, X1)
    ->  Included = Included1
    ;   Included = [X1|Included1]
    ),
    exclude_(Xs1, P, Included1).

include(Goal, List, Included) :-
    include_(List, Goal, Included).

include_([], _, []).
include_([X1|Xs1], P, Included) :-
    (   call(P, X1)
    ->  Included = [X1|Included1]
    ;   Included = Included1
    ),
    include_(Xs1, P, Included1).



% https://www.swi-prolog.org/pldoc/doc/_SWI_/library/lists.pl?show=src#member/2
member(El, [H|T]) :-
    member_(T, El, H).

member_(_, El, El).
member_([H|T], El, _) :-
    member_(T, El, H).

% https://stackoverflow.com/questions/5935133/prolog-how-to-remove-every-second-element-of-a-list
remove_second([], []).
remove_second([X], [X]).
remove_second([X,_|Xs], [X|Ys]) :- remove_second(Xs, Ys).

member__(X, Y) :-
    member(Y, X).