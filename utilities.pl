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
