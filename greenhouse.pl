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
        Goal = pod_allocation(Pod,Vegetable,Gardener,Times),
        setof(pod_schedule(Pod,Vegetable,Gardener,Times), Goal, Cs0),
        maplist(timed_schedule, Cs0, Cs).

timed_schedule(R, R-Slots) :- R = pod_schedule(_,_,_,Times), length(Slots, Times).

pods(Pods) :-
        setof(Pod, Vegetable^Times^Gardener^pod_allocation(Pod, Vegetable, Gardener, Times), Pods).

gardeners(Gardeners) :-
        setof(Gardener, Pod^Vegetable^Times^pod_allocation(Pod, Vegetable, Gardener, Times), Gardeners).

integer_division(S, Q) :-
        slots_per_cycle(SPM),
        Q #= S // SPM.

% Imposes the constraint that the growing period S must be immediately after the growing period F
extended_growth_pairs(Slots, F-S) :-
        nth0(F, Slots, S1),
        nth0(S, Slots, S2),
        S2 #= S1 + 1.

% extended_growth(Pod, Vegetable, F, S) :-
%         vegetable_times(VegetableTimes),

%         pod_allocation(Pod, Vegetable, _Gardener, _Times)
%         growing_cycle(Vegetable, Cycle),
%         Cycle #= 2.


constrain_vegetable_planting(pod_schedule(Pod,Vegetable,_Gardener,_Times)-Slots) :-
        strictly_ascending(Slots),
        maplist(integer_division, Slots, Qs0),

        %% create extended growth period constraints

        % WORKSPACE
        findall(r(V,T), pod_allocation(_Pod, V, _Gardener, T), VTs),
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

pod_req(G0, pod_schedule(G1, _Vegetable, _Gardener, _Times)-_, T) :- =(G0, G1, T).

gardener_req(Ga0, pod_schedule(_Pod, _Vegetable, Ga1, _Times)-_, T) :- =(Ga0, Ga1,T).

pairs_slots(Ps, ListVars) :-
        pairs_values(Ps, ListVars0),
        append(ListVars0, ListVars).

generate_schedule(Cs, ListVars) :-
    assign_variables(Cs, ListVars),
    labeling([ff], ListVars); true. % true allo
