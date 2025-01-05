:- module(start, [start/0]).

:- use_module(ui).
:- use_module(library(pce)).
:- use_module(game_logic).
:- use_module(figure_logic).

:- initialization start.

start:- 
    %writeln('Starting App'),
    new(Frame, frame('Start interface')),
    send(Frame, background, colour(white)),
    main_interface(Dialog),
    send(Frame, append, Dialog),
    send(Frame, open).