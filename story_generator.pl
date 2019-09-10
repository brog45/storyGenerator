:- module(story_generator,[generate_story/2]).
:- use_module(story_data).
:- use_module(planner).

generate_story(State, StoryOut) :-
    calculate_plan(State, Plan),
    apply_plan(State, Plan, StoryOut).

apply_plan(_, [], []).
apply_plan(State, [Action|_], [Event|TailOut]) :-
    choose_event(Action, Event, EventDict),
    !,
    apply_action(State, EventDict, NewState),
    calculate_plan(NewState, NewPlan),
    apply_plan(NewState, NewPlan, TailOut).
apply_plan(State, [Action|T], [Action|TailOut]) :-
    action(Action, ActionDict), 
    apply_action(State, ActionDict, NewState),
    apply_plan(NewState, T, TailOut).

choose_event(Action, Event, ActionDict) :-
    event(Action, Probability, Event, ActionDict),
    maybe(Probability), !,
    debug(story_generator(choose_event), 'Replacing ~w with ~w', [Action, Event]).
