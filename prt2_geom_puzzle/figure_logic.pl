:- module(figure_logic, [
    size/1,
    figure/3,
    colour/2,
    generate_description/2,
    generate_type/2,
    generate_bourder_colour/2,
    generate_fill_colour/2,
    generate_unique_figures/3,
    validate_description/2,
    get_figures_descroptions/15,
    get_single_figure/3,
    change_way/2,
    change_description/5
]).

:- use_module(library(random)).

% --- Вспомогательные предикаты ---
same(X,X).

by_index(E, [E|_], 0).
by_index(E, [_|T], I) :- 
    I > 0, 
    I2 is I - 1, 
    by_index(E, T, I2).

size(100).

% --- Фигуры ---
figure(0, Box, S):- new(Box, box(S, S)).
figure(1, Circle, S):- new(Circle, circle(S)).
figure(2, Triangle, S):- new(Triangle, path), send_list(Triangle, append, [point(0, 0), point(S/2, -S), point(S,0), point(0,0)]).
figure(3, Trapezoid, S):- 
    new(Trapezoid, path),
    send_list(Trapezoid, append, [
        point(0, 0), 
        point(S, 0), 
        point(3*S/4, -S), 
        point(S/4, -S), 
        point(0, 0)
    ]).

figure(4, Rectangle, S):- new(Rectangle, box(3*S/4, S)).

colour_black('#000000').
colour_green('#228B22').
colour_blue('#00BFFF').
colour_purple('#9370DB').
colour_pink('#FF1493').

colour(ColourInd, Colour):-
    Colours = [colour_black, colour_green, colour_blue, colour_purple, colour_pink],
    nth0(ColourInd, Colours, ColourPredicate),
    call(ColourPredicate, Colour).

% --- Генерация описания фигуры ---
generate_description(Desc, Diff):-
    %writeln('Generating figure description...'),
    generate_type(Type, Diff),
    generate_bourder_colour(BColourInd, Diff),
    generate_fill_colour(IColourInd, Diff),
    not(same(BColourInd, IColourInd)),
    Desc = [Type, BColourInd, IColourInd];
    generate_description(Desc, Diff).
    %writeln('Figure description denerated').

% --- Генерация типа от сложности ---
generate_type(Type, 2):- random(0, 5, Type).
generate_type(Type, _):- random(0, 3, Type).

% --- Генерация цвета границы ---
generate_bourder_colour(0, 0).
generate_bourder_colour(ColourInd, _):- random(0, 4, ColourInd).

% --- Генерация цвета заливки ---
generate_fill_colour(4, 0).
generate_fill_colour(ColourInd, _):- random(0, 5, ColourInd).

% --- Проверка корректности двух описаний ---
validate_description(Desc1, Desc2):- 
    %writeln('Validating description...'),
    Desc1 = [_, BColourInd1, IColourInd1], 
    Desc2 = [_, BColourInd2, IColourInd2],
    not(same(BColourInd1, IColourInd2)),
    not(same(BColourInd2, IColourInd1)).
    %writeln('Description validated').

% --- Генерация пар фигур ---
generate_unique_figures(Desc1, Desc2, Diff):- 
    generate_description(Desc1, Diff),
    generate_description(Desc2, Diff),
    validate_description(Desc1, Desc2),
    Desc1 = [Type1, _, _],
    Desc2 = [Type2, _, _],
    dif(Type1, Type2).

% --- Проверка различности пар фигур ---
ensure_different_pairs(Desc1, Desc2, Desc3, Desc4):-
    %writeln('ensure_different_pairs...'),
    not((Desc1 = Desc3, Desc2 = Desc4)),
    not((Desc1 = Desc4, Desc2 = Desc3)).
    %writeln('ensure_different_pairs').

% --- Основная функция генерации описаний фигур ---
% Для сложностей 0 и 1 генерируем 4 пары фигур
% Для сложности 2 генерируем дополнительно ещё 3 пары
get_figures_descroptions(Desc1, Desc2, Desc3, Desc4, Desc5, Desc6, Desc7, Desc8, Desc9, Desc10, Desc11, Desc12, Desc13, Desc14, Diff):- 
    generate_unique_figures(Desc1, Desc2, Diff),
    generate_unique_figures(Desc3, Desc4, Diff),
    ensure_different_pairs(Desc1, Desc2, Desc3, Desc4),
    change_description(Desc4, Desc3, Desc5, Desc6, Diff),
    change_description(Desc4, Desc3, Desc7, Desc8, Diff),
    (Diff = 2 ->
       (
         change_description(Desc4, Desc3, Desc9, Desc10, Diff),
         change_description(Desc4, Desc3, Desc11, Desc12, Diff),
         change_description(Desc4, Desc3, Desc13, Desc14, Diff)
       )
    ; 
       (Desc9=[],Desc10=[],Desc11=[],Desc12=[],Desc13=[],Desc14=[])).

% --- Создание фигуры по описанию ---
get_single_figure(Figure, Desc, Size):-
    %writeln('get_single_figure...'),
    Desc = [Type, BColourInd, IColourInd],
    figure(Type, Figure, Size),
    colour(BColourInd, BColour),
    colour(IColourInd, IColour),
    send(Figure, colour, colour(BColour)),
    send(Figure, fill_pattern, colour(IColour)),
    send(Figure, pen, 2).
    %writeln('get_single_figure').

% --- Логика изменения описаний ---
change_way(How, 0):- random(0, 2, How).
change_way(How, _):- random(0, 6, How).

change_description(Desc11, Desc12, Desc21, Desc22, Diff):-
    %writeln('change_description...'),
    change_way(How, Diff),
    change_desc_(Desc11, Desc12, Desc21, Desc22, How, Diff),
    validate_description(Desc21, Desc22);
    change_description(Desc11, Desc12, Desc21, Desc22, Diff).
    %writeln('change_description').

change_desc_(Desc11, Desc12, Desc21, Desc12, 0, Diff):-
    generate_type(Type, Diff),
    Desc11 = [Type11, BColourInd11, IColourInd11],
    Desc21 = [Type, BColourInd11, IColourInd11],
    not(same(Type11, Type));
    change_desc_(Desc11, Desc12, Desc21, Desc12, 0, Diff).

change_desc_(Desc11, Desc12, Desc11, Desc22, 1, Diff):-
    generate_type(Type, Diff),
    Desc12 = [Type12, BColourInd12, IColourInd12],
    Desc22 = [Type, BColourInd12, IColourInd12],
    not(same(Type12, Type));
    change_desc_(Desc11, Desc12, Desc11, Desc22, 1, Diff).

change_desc_(Desc11, Desc12, Desc21, Desc12, 2, Diff):-
    generate_bourder_colour(BColourInd, Diff),
    Desc11 = [Type11, BColourInd11, IColourInd11],
    Desc21 = [Type11, BColourInd, IColourInd11],
    not(same(BColourInd, IColourInd11)),
    not(same(BColourInd11, BColourInd));
    change_desc_(Desc11, Desc12, Desc21, Desc12, 2, Diff).

change_desc_(Desc11, Desc12, Desc11, Desc22, 3, Diff):-
    generate_bourder_colour(BColourInd, Diff),
    Desc12 = [Type12, BColourInd12, IColourInd12],
    Desc22 = [Type12, BColourInd, IColourInd12],
    not(same(BColourInd, IColourInd12)),
    not(same(BColourInd12, BColourInd));
    change_desc_(Desc11, Desc12, Desc11, Desc22, 3, Diff).

change_desc_(Desc11, Desc12, Desc21, Desc12, 4, Diff):-
    generate_fill_colour(IColourInd, Diff),
    Desc11 = [Type11, BColourInd11, IColourInd11],
    Desc21 = [Type11, BColourInd11, IColourInd],
    not(same(BColourInd11, IColourInd)),
    not(same(IColourInd11, IColourInd));
    change_desc_(Desc11, Desc12, Desc21, Desc12, 4, Diff).

change_desc_(Desc11, Desc12, Desc11, Desc22, 5, Diff):-
    generate_fill_colour(IColourInd, Diff),
    Desc12 = [Type12, BColourInd12, IColourInd12],
    Desc22 = [Type12, BColourInd12, IColourInd],
    not(same(IColourInd, BColourInd12)),
    not(same(IColourInd12, IColourInd));
    change_desc_(Desc11, Desc12, Desc11, Desc22, 5, Diff).
