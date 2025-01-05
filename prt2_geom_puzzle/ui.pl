:- module(ui, [
    main_interface/1,
    play_interface/1,
    add_question_section/2,
    add_answer_section/4,
    add_buttons/5,
    handle_ok_button/5,
    display_answers/2,
    show_correct_answer/3,
    draw_figure/5
]).

:- use_module(library(pce)).
:- use_module(game_logic).
:- use_module(figure_logic).

% --- Константы размеров ---
pictureQWidth(350).
pictureQHeight(700).
pictureAWidth(350).
pictureAHeight(700).

% --- Главное меню ---
main_interface(Dialog):-
    %writeln('Initializing main interface...'),
    new(Dialog, dialog),
    send(Dialog, background, colour(white)),
    send(Dialog, width(1000)),
    send(Dialog, height(480)),

    new(Menu, dialog_group('')),
    send(Dialog, append, Menu),
    send(Menu, gap, size(100, 30)),

    new(Buttons, dialog_group('')),
    new(Label, label(title, 'Choose the difficulty of the game:')),
    send(Label, font, font(screen, bold, 18)),
    send(Buttons, append, Label), 
    send(Menu, append, Buttons),
    send(Buttons, gap, size(60, 20)),

    new(ButtonEasy, button('1 - Easy', message(@prolog, play_interface_, Dialog, 0))),
    send(ButtonEasy, font, font(screen, bold, 14)),
    send(Buttons, append, ButtonEasy),

    new(ButtonMedium, button('2 - Medium', message(@prolog, play_interface_, Dialog, 1))),
    send(ButtonMedium, font, font(screen, bold, 14)),
    send(Buttons, append, ButtonMedium, next_row),

    new(ButtonHard, button('3 - Hard', message(@prolog, play_interface_, Dialog, 2))),
    send(ButtonHard, font, font(screen, bold, 14)),
    send(Buttons, append, ButtonHard, next_row),

    new(ButtonLeave, button(' Leave game ', message(Dialog, destroy))),
    send(ButtonLeave, font, font(screen, bold, 16)),
    send(Menu, append, ButtonLeave).
    %writeln('Main interface initialized').

play_interface_(Dialog, Difficulty):- send(Dialog, destroy), play_interface(Difficulty).

:- dynamic current_difficulty/1.

% --- Интерфейс игры ---
play_interface(Diff):-
    retractall(answer(_)),
    retractall(ok_mode(_)),
    retractall(current_difficulty(_)),
    asserta(current_difficulty(Diff)),  % Устанавливаем текущую сложность
    asserta(ok_mode(0)),   
    new(Frame, frame('Game interface')),

    new(Dialog, dialog),
    new(MainArea, dialog_group('')),
    send(Dialog, background, colour(white)),
    add_question_section(MainArea, PictureQ),
    add_answer_section(MainArea, PictureA, Select, Diff),
    send(MainArea, alignment, center),
    send(Dialog, append, MainArea),
    
    add_buttons(Dialog, PictureQ, PictureA, Select, Diff),

    generate_question(Diff, Data, CorrectAnswer, DescList),
    asserta(answer(CorrectAnswer)),

    draw_question(PictureQ, DescList),
    draw_answers(PictureA, Data, DescList),

    send(Frame, append, Dialog),
    send(Frame, open).

% --- Добавление поля с вопросами
add_question_section(MainArea, Picture):-
    %writeln('Adding question section...'),
    new(Question, dialog_group('')),
    new(Label, label(title, 'Complete the sequence:')),
    send(Label, font, font(screen, bold, 14)), 
    send(Question, append, Label),
    pictureQWidth(W),
    pictureQHeight(H),
    new(Picture, picture),
    send(Picture, width(W)),
    send(Picture, height(H)),
    send(Picture, background, colour(grey90)),
    send(Question, append, Picture),
    send(MainArea, append, Question).
    %writeln('Question section added').
    
% --- Добавление поля с ответами ---
add_answer_section(MainArea, Picture, Select, Diff):-
    (Diff =:= 2 ->
        Choices = [1, 2, 3, 4, 5, 6, 'Skip']  % Hard: 6 ответов
    ;
        Choices = [1, 2, 3, 'Skip']  % Easy/Medium: 3 ответа
    ),
    new(Answer, dialog_group('')),
    new(Label, label(title, 'Choose one answer:')),
    send(Label, font, font(screen, bold, 14)),
    send(Answer, append, Label),
    pictureAWidth(W),
    pictureAHeight(H),
    new(Picture, picture),
    send(Picture, width(W)),
    send(Picture, height(H)),
    send(Picture, background, colour(grey90)),
    send(Answer, append, Picture),
    
    new(Select, menu('Your answer', list_browser)),
    send_list(Select, append, Choices),
    send(Select, selection, 1),  % Установить начальный выбор как 1
    
    send(MainArea, append, Answer, right),
    send(MainArea, append, Select).


% --- Создание управляющих кнопок ---
add_buttons(Dialog, PictureQ, PictureA, Select, Diff):-
    %writeln('Adding buttons...'),
    new(Buttons, dialog_group('')),
    send(Buttons, gap, size(245, 8)),
    send(Dialog, append, Buttons),

    new(ButtonOk, button('Ok')),
    send(ButtonOk, font, font(screen, bold, 16)),
    send(ButtonOk, message, message(@prolog, handle_ok_button, PictureQ, PictureA, Diff, Select, ButtonOk)),
    send(Buttons, append, ButtonOk),

    new(MainMenu, button('Main menu', and(message(Dialog, destroy), message(@prolog, start)))),
    send(MainMenu, font, font(screen, bold, 16)),
    send(Buttons, append, MainMenu).
    %writeln('Buttons added').

% --- Поведение при нажатии 'Ok' ---
handle_ok_button(PictureQ, PictureA, Diff, Select, ButtonOk):-
    ok_mode(M),
    (   M = 0 ->
        asserta(ok_mode(1)), retract(ok_mode(0)),
        display_answers(PictureA, Select),
        send(ButtonOk, label, 'Next task')
    ;   M = 1 ->
        asserta(ok_mode(0)), retract(ok_mode(1)),
        send(PictureQ, clear),
        send(PictureA, clear),
        retract(answer(_)),
        generate_question(Diff, Data, CorrectAnswer, DescList),
        asserta(answer(CorrectAnswer)),
        draw_question(PictureQ, DescList),
        draw_answers(PictureA, Data, DescList),
        send(ButtonOk, label, 'Ok')
    ).

% --- Отображение ответов ---
display_answers(PictureA, Select):-
    %%writeln('Displaying answers...'),
    get(Select, selection, UserAnswer),
    (var(UserAnswer) -> UserAnswer = 1 ; true),
    answer(RightAnswer),
    writeln(['UserAnswer:', UserAnswer, 'RightAnswer:', RightAnswer]),
    % Показываем правильный ответ
    show_correct_answer(RightAnswer, RightAnswer, PictureA),
    % Показываем, если выбран неправильный
    (UserAnswer \= RightAnswer, UserAnswer \= 'Skip' -> show_correct_answer(UserAnswer, RightAnswer, PictureA); true).
    %%writeln('Answers displayed').

% --- Подсветка правильного и неправильного ответа ---
show_correct_answer(Answer1, Answer2, Picture):-
    %%writeln('Showing correct answer...'),
    (Answer1 == 'Skip' -> 
        true 
    ; (
        current_difficulty(D),
        % Если hard режим:
        (D == 2 ->
           (Answer1 == 1 -> BaseY=110
             ; Answer1 == 2 -> BaseY=220
             ; Answer1 == 3 -> BaseY=330
             ; Answer1 == 4 -> BaseY=440
             ; Answer1 == 5 -> BaseY=550
             ; Answer1 == 6 -> BaseY=660
            )
        ;
            (Answer1 == 1 -> BaseY=150
             ; Answer1 == 2 -> BaseY=300
             ; Answer1 == 3 -> BaseY=450
            )
        ),

        X = 175,
        size(Size),
        PenX is X + Size/2 + 25,
        PenY is BaseY,
        %writeln(PenY),
        (Answer1 =:= Answer2 ->
            % Рисуем зелёную галочку
            new(Mark, path),
            send(Mark, pen, 6),
            send(Mark, colour, colour('#9ACD32')),
            send_list(Mark, append, [point(PenX, PenY-10), point(PenX+10, PenY), point(PenX+20, PenY-20)]),
            send(Picture, display, Mark)
        ;
            % Рисуем красный крестик
            new(Mark, path),
            send(Mark, pen, 6),
            send(Mark, colour, colour('#8B0000')),
            send_list(Mark, append, [point(PenX, PenY-15), point(PenX+15, PenY)]),
            send(Picture, display, Mark),

            new(Mark2, path),
            send(Mark2, pen, 6),
            send(Mark2, colour, colour('#8B0000')),
            send_list(Mark2, append, [point(PenX, PenY), point(PenX+15, PenY-15)]),
            send(Picture, display, Mark2)
        )
    )).
    %%writeln('Correct answer showed').



% --- Отрисовка фигур ---
draw_figure(Picture, DescB, DescS, X, Y):-
    %writeln('Drawing figures...'),
    size(Size),
    get_single_figure(BigFigure, DescB, Size),
    get_single_figure(SmallFigure, DescS, Size / 4),
    send(Picture, display, BigFigure, point(X - Size/2, Y - Size/2)),
    send(Picture, display, SmallFigure, point(X - Size/8, Y - Size/8)).
    %writeln('Figures drawed').

% --- Отрисовка вопроса ---
draw_question(PictureQ, [Desc1, Desc2, Desc3, Desc4, _Desc5, _Desc6, _Desc7, _Desc8|_Extra]):-
    %writeln('Drawing question...'),
    size(Size),
    X is 175,
    StartY is 100,
    Spacing is 150,

    draw_figure(PictureQ, Desc1, Desc2, X, StartY),
    draw_figure(PictureQ, Desc2, Desc1, X, StartY + Spacing),
    draw_figure(PictureQ, Desc3, Desc4, X, StartY + 2*Spacing),

    % Рисуем линию, обозначающую пропущенное место для 4-й пары
    YLine is StartY + 3*Spacing-20,
    draw_question_sign(PictureQ, X, YLine),

    % Стрелки
    YFirstCenter is StartY + Size/2,
    YSecondCenter is (StartY + Spacing) - Size/2,
    new(Arrow1, line(X, YFirstCenter, X, YSecondCenter)),
    send(Arrow1, pen, 2),
    send(Arrow1, colour, colour(black)),
    send(Arrow1, arrows, second),
    send(PictureQ, display, Arrow1),

    YThirdCenter is (StartY + 2*Spacing) + Size/2,
    YFourthCenter is YThirdCenter + (YSecondCenter - YFirstCenter),
    new(Arrow2, line(X, YThirdCenter, X, YFourthCenter)),
    send(Arrow2, pen, 2),
    send(Arrow2, colour, colour(black)),
    send(Arrow2, arrows, second),
    send(PictureQ, display, Arrow2).
    %writeln('Question drawed').

draw_question_sign(Picture, X, Y):- 
    new(Sign, path),
    send(Sign, pen, 4),
    send_list(Sign, append, [point(0, 20), point(0, 10), point(10, 0), point(40, 0), point(50, 10), point(50, 35), point(30, 50), point(25, 60), point(25, 80)]),

    new(Circ, circle(10)),
    send(Circ, pen, 4),

    send(Picture, display, Sign, point(X - 25, Y)),
    send(Picture, display, Circ, point(X - 3, Y + 90)).

% --- Отрисовка ответов ---
draw_answers(PictureA, data(_D1,_D2,Desc3,Desc4,Desc5,Desc6,Desc7,Desc8,ExtraDescs,Ys), _DescList):-
    X is 175,
    size(Size),
    answer(R),
    length(Ys, Len),
    (
        Len = 3 ->
            nth1(1, Ys, Y1),
            nth1(2, Ys, Y2),
            nth1(3, Ys, Y3),
            (R =:= 1 ->
                draw_figure(PictureA, Desc4, Desc3, X, Y1),
                draw_figure(PictureA, Desc5, Desc6, X, Y2),
                draw_figure(PictureA, Desc7, Desc8, X, Y3)
            ; R =:= 2 ->
                draw_figure(PictureA, Desc5, Desc6, X, Y1),
                draw_figure(PictureA, Desc4, Desc3, X, Y2),
                draw_figure(PictureA, Desc7, Desc8, X, Y3)
            ; R =:= 3 ->
                draw_figure(PictureA, Desc5, Desc6, X, Y1),
                draw_figure(PictureA, Desc7, Desc8, X, Y2),
                draw_figure(PictureA, Desc4, Desc3, X, Y3)
            ),

            % Нумерация
            new(Text1, text('1')),
            send(Text1, font, font(screen, bold, 16)),
            send(PictureA, display, Text1, point(X - 100, Y1 - Size/4 + 5)),

            new(Text2, text('2')),
            send(Text2, font, font(screen, bold, 16)),
            send(PictureA, display, Text2, point(X - 100, Y2 - Size/4 + 5)),

            new(Text3, text('3')),
            send(Text3, font, font(screen, bold, 16)),
            send(PictureA, display, Text3, point(X - 100, Y3 - Size/4 + 5))

        ;
        Len = 6 ->
            nth1(1, Ys, Y1),
            nth1(2, Ys, Y2),
            nth1(3, Ys, Y3),
            nth1(4, Ys, Y4),
            nth1(5, Ys, Y5),
            nth1(6, Ys, Y6),
            [Desc9,Desc10,Desc11,Desc12,Desc13,Desc14] = ExtraDescs,
            % C - правильная пара, A,B,D,E,F - остальные
            C = (Desc4,Desc3),
            A = (Desc5,Desc6),
            B = (Desc7,Desc8),
            D = (Desc9,Desc10),
            E = (Desc11,Desc12),
            F = (Desc13,Desc14),

            (R =:= 1 ->
                % Правильная C на первую позицию
                place_pair(PictureA, C, X, Y1),
                place_pair(PictureA, A, X, Y2),
                place_pair(PictureA, B, X, Y3),
                place_pair(PictureA, D, X, Y4),
                place_pair(PictureA, E, X, Y5),
                place_pair(PictureA, F, X, Y6)
            ; R =:= 2 ->
                % C на вторую позицию
                place_pair(PictureA, A, X, Y1),
                place_pair(PictureA, C, X, Y2),
                place_pair(PictureA, B, X, Y3),
                place_pair(PictureA, D, X, Y4),
                place_pair(PictureA, E, X, Y5),
                place_pair(PictureA, F, X, Y6)
            ; R =:= 3 ->
                % C на третью позицию
                place_pair(PictureA, A, X, Y1),
                place_pair(PictureA, B, X, Y2),
                place_pair(PictureA, C, X, Y3),
                place_pair(PictureA, D, X, Y4),
                place_pair(PictureA, E, X, Y5),
                place_pair(PictureA, F, X, Y6)
            ; R =:= 4 ->
                % C на четвертую позицию
                place_pair(PictureA, A, X, Y1),
                place_pair(PictureA, B, X, Y2),
                place_pair(PictureA, D, X, Y3),
                place_pair(PictureA, C, X, Y4),
                place_pair(PictureA, E, X, Y5),
                place_pair(PictureA, F, X, Y6)
            ; R =:= 5 ->
                % C на пятую позицию
                place_pair(PictureA, A, X, Y1),
                place_pair(PictureA, B, X, Y2),
                place_pair(PictureA, D, X, Y3),
                place_pair(PictureA, E, X, Y4),
                place_pair(PictureA, C, X, Y5),
                place_pair(PictureA, F, X, Y6)
            ; R =:= 6 ->
                % C на шестую позицию
                place_pair(PictureA, A, X, Y1),
                place_pair(PictureA, B, X, Y2),
                place_pair(PictureA, D, X, Y3),
                place_pair(PictureA, E, X, Y4),
                place_pair(PictureA, F, X, Y5),
                place_pair(PictureA, C, X, Y6)
            ),

            % Нумерация вариантов
            forall(nth1(I, Ys, Ycoord), (
                number_chars(I, [Cnum]),
                atom_chars(AtomI, [Cnum]),
                new(Txt, text(AtomI)),
                send(Txt, font, font(screen, bold, 16)),
                send(PictureA, display, Txt, point(X - 100, Ycoord - Size/4 + 5))
            ))
    ).

% Вспомогательный предикат для размещения пары
place_pair(PictureA, (DescB,DescS), X, Y):-
    draw_figure(PictureA, DescB, DescS, X, Y).
