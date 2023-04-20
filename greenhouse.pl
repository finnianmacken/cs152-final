% dynamic and discontiguous declarations
:- dynamic(greenhouse_allocation/4).
:- dynamic(extended_growth/4).
:- dynamic(no_gardener/2).
:- dynamic(slots_per_month/1).
:- dynamic(slots_per_year/1).
:- dynamic(empty_greenhouse/2).
:- dynamic(soil_allocation/4).

:- discontiguous(greenhouse_allocation/4).
:- discontiguous(empty_greenhouse/2).

constraints(Cs) :-
        Goal = greenhouse_allocation(Greenhouse,Vegetable,Gardener,Times),
        setof(greenhouse_schedule(Greenhouse,Vegetable,Gardener,Times), Goal, Rs0),
        maplist(timed_schedule, Rs0, Cs).

timed_schedule(R, R-Slots) :- R = greenhouse_schedule(_,_,_,Times), length(Slots, Times).

greenhouses(Greenhouses) :-
        setof(Greenhouse, Vegetable^Times^Gardener^greenhouse_allocation(Greenhouse, Vegetable, Gardener, Times), Greenhouses).

gardeners(Gardeners) :-
        setof(Gardener, Greenhouse^Vegetable^Times^greenhouse_allocation(Greenhouse, Vegetable, Gardener, Times), Gardeners).

soils(Soils) :-
        findall(Soil, soil_allocation(Soil, _Greenhouse, _Vegetable, _Slot), Soils0),
        sort(Soils0, Soils).

constraints_variables(Cs, Vars) :-
        constraints(Cs),
        pairs_slots(Cs, Vars), 
        slots_per_year(SPY),
        Max #= SPY - 1,
        Vars ins 0..Max,
        maplist(constrain_vegetable_planting, Cs),

        % apply constraints to scheduler components
        greenhouses(Greenhouses),
        maplist(constrain_greenhouse(Cs), Greenhouses),

        gardeners(Gardeners),
        maplist(constrain_gardener(Cs), Gardeners),

        soils(Soils),
        maplist(constrain_soil(Cs), Soils).

quotient(S, Q) :-
        slots_per_month(SPM),
        Q #= S // SPM.

remove_from_list(Es0, Ws, Es) :-
        phrase(remove_(Ws, 0, Es0), Es).

% uses dcgs for sequence generation
remove_([], _, Es) --> seq(Es).
remove_([W|Ws], Pos0, [E|Es]) -->
        { Pos #= Pos0 + 1,
          zcompare(R, W, Pos0) },
        remove_at_pos0(R, E, [W|Ws], Ws1),
        remove_(Ws1, Pos, Es).

remove_at_pos0(=, _, [_|Ws], Ws) --> [].
remove_at_pos0(>, E, Ws0, Ws0) --> [E].

extended_growth_pairs(Slots, F-S) :-
        nth0(F, Slots, S1),
        nth0(S, Slots, S2),
        S2 #= S1 + 1.

constrain_vegetable_planting(greenhouse_schedule(Greenhouse,Vegetable,_Gardener,_Times)-Slots) :-
        strictly_ascending(Slots),
        maplist(quotient, Slots, Qs0),
        findall(F-S, extended_growth(Greenhouse, Vegetable, F, S), Gs),
        
        % deal with extended growth periods
        maplist(extended_growth_pairs(Slots), Gs), 
        pairs_values(Gs, Seconds0),
        sort(Seconds0, Seconds),
        remove_from_list(Qs0, Seconds, Qs),
        strictly_ascending(Qs).

all_diff_from(ListVars, F) :- maplist(#\=(F), ListVars).

constrain_greenhouse(Cs, Greenhouse) :-
        tfilter(greenhouse_req(Greenhouse), Cs, Sub),
        pairs_slots(Sub, ListVars),
        all_different(ListVars),
        findall(S, empty_greenhouse(Greenhouse, S), Vacants),
        maplist(all_diff_from(ListVars), Vacants).

constrain_gardener(Cs, Gardener) :-
        tfilter(gardener_req(Gardener), Cs, Sub),
        pairs_slots(Sub, ListVars),
        all_different(ListVars),
        findall(F, no_gardener(Gardener, F), Fs),
        maplist(quotient, ListVars, Qs),
        maplist(all_diff_from(Qs), Fs).

samesoil_var(Reqs, r(Greenhouse,Vegetable,Lesson), Var) :-
        memberchk(greenhouse_schedule(Greenhouse,Vegetable,_Gardener,_Times)-Slots, Reqs),
        nth0(Lesson, Slots, Var).

constrain_soil(Reqs, Soil) :-
        findall(r(Greenhouse,Vegetable,Lesson), soil_allocation(Soil,Greenhouse,Vegetable,Lesson), RReqs),
        maplist(samesoil_var(Reqs), RReqs, Soilvars),
        all_different(Soilvars).

strictly_ascending(Ls) :- chain(#<, Ls).

greenhouse_req(G0, greenhouse_schedule(G1, _Vegetable, _Gardener, _Times)-_, T) :- =(G0, G1, T).

gardener_req(Ga0, greenhouse_schedule(_Greenhouse, _Vegetable, Ga1, _Times)-_, T) :- =(Ga0, Ga1,T).

pairs_slots(Ps, ListVars) :-
        pairs_values(Ps, ListVars0),
        append(ListVars0, ListVars).

generate_schedule(Cs, ListVars) :-
    constraints_variables(Cs, ListVars),
    labeling([ff], ListVars); true. % true allo
