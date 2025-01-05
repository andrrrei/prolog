:- module(game_logic, [
    difficulty/1,
    ok_mode/1,
    answer/1,
    prepare_next_question/4,
    generate_question/4,
    answers_location/4,
    answers_location_hard/2
]).

:- use_module(figure_logic).

:- dynamic ok_mode/1.
:- dynamic answer/1.

% --- Состояние для кнопки ok ---
ok_mode(0).

% Правильный ответ хранится в answer/1

% --- Определение уровней сложности ---
difficulty(0).
difficulty(1).
difficulty(2).

% --- Очищаем предыдущее состояние (answer/1), генерируем новый вопрос ---
prepare_next_question(_PictureQ, _PictureA, Diff, Data):-
    %writeln('Preparing next question...'),
    retractall(answer(_)),
    generate_question(Diff, Data, CorrectAnswer, _DescList),
    asserta(answer(CorrectAnswer)),
    asserta(ok_mode(0)).
    %writeln('Next question prepared').

% --- Генерирует новые описания фигур для вопроса и ответов ---
% Теперь для режима hard мы генерируем 6 пар ответов
% Для Diff<2 как прежде, 3 пары ответов.
generate_question(2, 
    data(Desc1, Desc2, Desc3, Desc4, Desc5, Desc6, Desc7, Desc8, [Desc9, Desc10, Desc11, Desc12, Desc13, Desc14], Ys), 
    CorrectAnswer, 
    AllDescs
) :-
    get_figures_descroptions(Desc1, Desc2, Desc3, Desc4, Desc5, Desc6, Desc7, Desc8, Desc9, Desc10, Desc11, Desc12, Desc13, Desc14, 2),
    answers_location_hard(Ys, CorrectAnswer),
    append([Desc1, Desc2, Desc3, Desc4, Desc5, Desc6, Desc7, Desc8], [Desc9, Desc10, Desc11, Desc12, Desc13, Desc14], AllDescs).

generate_question(Diff, 
    data(Desc1, Desc2, Desc3, Desc4, Desc5, Desc6, Desc7, Desc8, [], [Y1, Y2, Y3]), 
    CorrectAnswer, 
    [Desc1, Desc2, Desc3, Desc4, Desc5, Desc6, Desc7, Desc8]
) :-
    dif(Diff, 2),
    get_figures_descroptions(Desc1, Desc2, Desc3, Desc4, Desc5, Desc6, Desc7, Desc8, _, _, _, _, _, _, Diff),
    answers_location(Y1, Y2, Y3, CorrectAnswer).



% --- Перемешивание ответов (для Diff<2) ---
answers_location(Y1, Y2, Y3, R):-
    random(1, 4, R),
    Y1 = 150,
    Y2 = 300,
    Y3 = 450.

% --- Расположение ответов для режима hard (6 ответов) ---
answers_location_hard([Y1,Y2,Y3,Y4,Y5,Y6], R):-
    random(1,7,R),
    Y1 = 110,
    Y2 = 220,
    Y3 = 330,
    Y4 = 440,
    Y5 = 550,
    Y6 = 660.
