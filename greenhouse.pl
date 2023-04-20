% dynamic and discontiguous declarations
:- dynamic(pod_allocation/4).
:- dynamic(extended_growth/4).
:- dynamic(gardener_free_weeks/2).
:- dynamic(slots_per_cycle/1).
:- dynamic(slots_per_year/1).
:- dynamic(empty_week/1).

:- discontiguous(pod_allocation/4).
:- discontiguous(empty_week/1).

assign_variables(Cs, Vars) :-
        constraints(Cs),
        pairs_slots(Cs, Vars), 
        slots_per_year(SPY),
        Max #= SPY - 1,
        Vars ins 0..Max,

        % apply constraints to scheduler components
        maplist(constrain_vegetable_planting, Cs),
        
        %% pods
        pods(Pods),
        maplist(constrain_pod(Cs), Pods),

        %% gardeners
        gardeners(Gardeners),
        maplist(constrain_gardener(Cs), Gardeners).

constraints(Cs) :-
        % connects allocations to schedules
        Goal = pod_allocation(Pod,Vegetable,Gardener,Weeks),
        setof(pod_schedule(Pod,Vegetable,Gardener,Weeks), Goal, Cs0),
        maplist(timed_schedule, Cs0, Cs).

timed_schedule(R, R-Slots) :- R = pod_schedule(_,_,_,Weeks), length(Slots, Weeks).

pods(Pods) :-
        setof(Pod, Vegetable^Weeks^Gardener^pod_allocation(Pod, Vegetable, Gardener, Weeks), Pods).

gardeners(Gardeners) :-
        setof(Gardener, Pod^Vegetable^Weeks^pod_allocation(Pod, Vegetable, Gardener, Weeks), Gardeners).

integer_division(S, Q) :-
        slots_per_cycle(SPM),
        Q #= S // SPM.

% Imposes the constraint that the growing period S must be immediately after the growing period F
extended_growth_pairs(Slots, F-S) :-
        nth0(F, Slots, S1),
        nth0(S, Slots, S2),
        S2 #= S1 + 1.

create_coupling(r(P, V), Value, r(P, V)-[Value-Y]) :-
    Y #= Value + 1.

generate_couplings(r(P, V, T), Couplings) :-
    Range is T / 2, 
    findall(X, between(0, Range, X), Y),
    remove_second(Y, Couplings0),
    maplist(create_coupling(r(P, V)), Couplings0, Couplings).

generate_couplings_from_pods(Output) :-
    % find all information about multi-week vegetables
    findall(r(P,V,T), (pod_allocation(P, V, _Gardener, T), member__([r(P, broccoli, T)], r(P,V,T))), VTs),
    % generate couplings for each one
    maplist(generate_couplings, VTs, Output).

extended_growth(Pod, Vegetable, F, S) :-
    generate_couplings_from_pods(Output0),
    flatten(Output0, Output),
    member(r(Pod, Vegetable)-[F-S], Output).

constrain_vegetable_planting(pod_schedule(Pod, Vegetable,_Gardener,_Weeks)-Slots) :-
        strictly_ascending(Slots),
        maplist(integer_division, Slots, Qs0),

        %% create extended growth period constraints

        % WORKSPACE
        findall(r(P,V,T), (pod_allocation(P, V, _Gardener, T), member__([r(P, broccoli, T), r(P, potatoes, T)], r(P,V,T))), VTs),
        % END WORKSPACE

        findall(F-S, extended_growth(Pod, Vegetable, F, S), Gs),
        maplist(extended_growth_pairs(Slots), Gs), 

        %% remove the second element in each pair from the list because its position is enforced by the extended_growth_pairs constraint
        pairs_values(Gs, Seconds0),
        sort(Seconds0, Seconds),
        remove_from_list(Qs0, Seconds, Qs),
        strictly_ascending(Qs).

all_diff_from(ListVars, F) :- maplist(#\=(F), ListVars).

constrain_pod(Cs, Pod) :-
        tfilter(pod_req(Pod), Cs, Sub),
        pairs_slots(Sub, ListVars),
        all_different(ListVars),

        % enforces empty pod constraints by asserting that the specified slots aredifferent from ListVars
        findall(S, empty_week(S), Vacants),
        maplist(all_diff_from(ListVars), Vacants).

constrain_gardener(Cs, Gardener) :-
        tfilter(gardener_req(Gardener), Cs, Sub),
        pairs_slots(Sub, ListVars),
        all_different(ListVars),

        % apply the no gardener day constraints by asserting that all free Weeks are different from the integer
        % divisions of the slot variables

        findall(FreeWeeks0, gardener_free_weeks(Gardener, FreeWeeks0), FreeWeeksNested),
        flatten(FreeWeeksNested, FreeWeeks),
        maplist(integer_division, ListVars, Qs),
        maplist(all_diff_from(Qs), FreeWeeks).

strictly_ascending(Ls) :- chain(#<, Ls).

pod_req(G0, pod_schedule(G1, _Vegetable, _Gardener, _Weeks)-_, T) :- =(G0, G1, T).

gardener_req(Ga0, pod_schedule(_Pod, _Vegetable, Ga1, _Weeks)-_, T) :- =(Ga0, Ga1,T).

pairs_slots(Ps, ListVars) :-
        pairs_values(Ps, ListVars0),
        append(ListVars0, ListVars).

generate_schedule(Cs, ListVars) :-
    assign_variables(Cs, ListVars),
    labeling([ff], ListVars); true. % true allo
