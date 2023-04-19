:- dynamic(greenhouse_allocation/4).
:- dynamic(extended_growth/4).
:- dynamic(no_gardener/2).
:- dynamic(slots_per_month/1).
:- dynamic(slots_per_year/1).
:- dynamic(empty_greenhouse/2).
:- dynamic(soil_allocation/4).

:- discontiguous(greenhouse_allocation/4).
:- discontiguous(empty_greenhouse/2).

requirements(Rs) :-
        Goal = greenhouse_allocation(Greenhouse,Vegetable,Gardener,Times),
        setof(greenhouse_schedule(Greenhouse,Vegetable,Gardener,Times), Goal, Rs0),
        maplist(timed_schedule, Rs0, Rs).

timed_schedule(R, R-Slots) :- R = greenhouse_schedule(_,_,_,Times), length(Slots, Times).

greenhouses(Greenhouses) :-
        setof(Greenhouse, Vegetable^Times^Gardener^greenhouse_allocation(Greenhouse, Vegetable, Gardener, Times), Greenhouses).

gardeners(Gardeners) :-
        setof(Gardener, Greenhouse^Vegetable^Times^greenhouse_allocation(Greenhouse, Vegetable, Gardener, Times), Gardeners).

soils(Soils) :-
        findall(Soil, soil_allocation(Soil, _Greenhouse, _Vegetable, _Slot), Soils0),
        sort(Soils0, Soils).

requirements_variables(Rs, Vars) :-
        requirements(Rs),
        pairs_slots(Rs, Vars), 
        slots_per_year(SPY),
        Max #= SPY - 1,
        Vars ins 0..Max,
        maplist(constrain_vegetable_planting, Rs),
        greenhouses(Greenhouses),
        gardeners(Gardeners),
        soils(Soils),
        maplist(constrain_gardener(Rs), Gardeners),
        maplist(constrain_greenhouse(Rs), Greenhouses),
        maplist(constrain_soil(Rs), Soils).

slot_quotient(S, Q) :-
        slots_per_month(SPM),
        Q #= S // SPM.

list_without_nths(Es0, Ws, Es) :-
        phrase(without_(Ws, 0, Es0), Es).

without_([], _, Es) --> seq(Es).
without_([W|Ws], Pos0, [E|Es]) -->
        { Pos #= Pos0 + 1,
          zcompare(R, W, Pos0) },
        without_at_pos0(R, E, [W|Ws], Ws1),
        without_(Ws1, Pos, Es).

without_at_pos0(=, _, [_|Ws], Ws) --> [].
without_at_pos0(>, E, Ws0, Ws0) --> [E].

slots_couplings(Slots, F-S) :-
        nth0(F, Slots, S1),
        nth0(S, Slots, S2),
        S2 #= S1 + 1.



constrain_vegetable_planting(greenhouse_schedule(Greenhouse,Vegetable,_Gardener,_Times)-Slots) :-
        strictly_ascending(Slots),
        maplist(slot_quotient, Slots, Qs0),
        findall(F-S, extended_growth(Greenhouse, Vegetable, F, S), Gs),
        maplist(slots_couplings(Slots), Gs), 
        pairs_values(Gs, Seconds0),
        sort(Seconds0, Seconds),
        list_without_nths(Qs0, Seconds, Qs),
        strictly_ascending(Qs).

all_diff_from(Vs, F) :- maplist(#\=(F), Vs).

constrain_greenhouse(Rs, Greenhouse) :-
        tfilter(greenhouse_req(Greenhouse), Rs, Sub),
        pairs_slots(Sub, Vs),
        all_different(Vs),
        findall(S, empty_greenhouse(Greenhouse, S), Vacants),
        maplist(all_diff_from(Vs), Vacants).

constrain_gardener(Rs, Gardener) :-
        tfilter(gardener_req(Gardener), Rs, Sub),
        pairs_slots(Sub, Vs),
        all_different(Vs),
        findall(F, no_gardener(Gardener, F), Fs),
        maplist(slot_quotient, Vs, Qs),
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

pairs_slots(Ps, Vs) :-
        pairs_values(Ps, Vs0),
        append(Vs0, Vs).

generate_schedule(Rs, Vs) :-
    requirements_variables(Rs, Vs),
    labeling([ff], Vs); true.
