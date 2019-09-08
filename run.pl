:- use_module(planner).
:- use_module(story_data).
:- use_module(story_dcg_html).
:- use_module(story_generator).

:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_error)).
:- use_module(library(http/html_write)).
:- use_module(library(http/http_parameters)).
:- use_module(library(http/http_session)).

:- http_handler(/, home_page, []).
:- http_handler('/settings', settings_handler, []).
:- http_handler('/story', story_page, []).

home_page(_Request) :-
    reply_html_page([title('Story Generator')],
                    [   h1('Story Generator')
                    ,   form([ method=post, action=settings ],
                             [ div(label([ 'Name of Character: '
                                         , input([type=text, name=name, required])
                                         ]))
                             , div(label([ 'Name of Pet: '
                                         , input([type=text, name=pet, required])
                                         ]))
                             , div(label([ 'Type of Animal: '
                                         , select([name=animal, required],
                                                  [ option([])
                                                  , option(bunny)
                                                  , option(kitty)
                                                  , option(puppy)
                                                  ])
                                         ]))
                             , input([ type=submit ])
                             ])
                    ]
                    ).

settings_handler(Request) :-
    http_parameters(Request,
        [   name(Name,      [form_data])
        ,   pet(Pet,        [form_data])
        ,   animal(Animal,  [form_data])
        ]),
    http_session_retractall(player(_)),
    http_session_retractall(pet(_,_)),
    http_session_assert(player(Name)),
    http_session_assert(pet(Pet, Animal)),
    http_redirect(moved_temporary, story, Request).

story_page(Request) :-
    (   session_data(player(Name), pet(Pet, Animal))
    ->  reply_story(player(Name), pet(Pet, Animal))
    ;   http_redirect(moved_temporary, /, Request)
    ).

reply_story(player(Name), pet(Pet, Animal)) :-
    init(Name, Pet, Animal, State),
    generate_story(State, Story),
    phrase(story(Story), StoryHtml),
    !,
    reply_html_page([title('A Story')], 
                    [   h1('A Story')
                    ,   div([class=story], StoryHtml)
                    ,   button([onclick='javascript:document.location.reload()'], 
                               ['Generate another story'])
                    ]).

session_data(player(Name), pet(Pet, Animal)) :-
    http_session_data(player(Name)),
    http_session_data(pet(Pet, Animal)).

go :-
    Port = 8080,
    http_server(http_dispatch, [port(Port)]).

% vim: et ts=4 sw=4 ai
