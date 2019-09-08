% breadth-first version, avoids infinite recursion

:- module(planner,[calculate_plan/2, apply_action/3]).
:- use_module(story_data).

calculate_plan(InitialState, Plan) :-
    ord_empty(ClosedSet),
    state_plan_node(InitialState, [], Node),
    process_queue([Node], ClosedSet, NodeOut),
    !,
    Plan = NodeOut.plan.

state_plan_node(State, Plan, node{ state:State, plan:Plan }).

process_queue(OpenList, ClosedSet, _) :-
    % this clause just writes to the debug monitor
    length(OpenList, OpenLen),
    length(ClosedSet, ClosedLen),
    debug(planner(process_queue), 'open ~w closed ~w', [OpenLen, ClosedLen]),
    fail.
process_queue([HeadNode|_], _, HeadNode) :-
    state_plan_node(HeadState, _, HeadNode),
    done(HeadState),
    !.
process_queue([HeadNode|TailNodes], ClosedSet, NodeOut) :-
    findall(Node, take_action(HeadNode, ClosedSet, Node), OutcomeNodes),
    append(TailNodes, OutcomeNodes, Queue),
    state_plan_node(HeadState, _, HeadNode),
    close_state(ClosedSet, HeadState, ClosedList0),
    process_queue(Queue, ClosedList0, NodeOut).

done(State) :-
    findall(G, member(goal(G), State), Goals),
    intersection(Goals, State, Goals).

% take an action; check its outcome against the closed list; and add its action term to the plan
take_action(NodeIn, ClosedSet, NodeOut) :-
    % action/1 is defined in the story data
    action(ActionStep, ActionDict),
    state_plan_node(StateIn, PlanIn, NodeIn),
    apply_action(StateIn, ActionDict, StateOut),
    state_not_closed(StateOut, ClosedSet),
    append(PlanIn, [ActionStep], PlanOut),
    state_plan_node(StateOut, PlanOut, NodeOut).

apply_action(StateIn, ActionDict, StateOut) :-
    subtract(StateIn, ActionDict.negprereqs, StateIn),
    intersection(ActionDict.prereqs, StateIn, ActionDict.prereqs),
    subtract(StateIn, ActionDict.removes, S0),
    append(S0, ActionDict.adds, StateOut).

state_not_closed(State, ClosedSet) :-
    list_to_ord_set(State, StateOrdSet),
    \+ ord_memberchk(StateOrdSet, ClosedSet).

close_state(ClosedSetIn, State, ClosedSetOut) :-
    list_to_ord_set(State, StateOrdSet),
    ord_add_element(ClosedSetIn, StateOrdSet, ClosedSetOut).
