:- use_foreign_library('Lib/plOpenGL.so').
:- use_module('Lib/plOpenGL.pl').
:- use_module('Lib/plGL_defs.pl').
:- use_module('Lib/plGLU_defs.pl').
:- use_module('Lib/plGLUT_defs.pl').
:- use_module('Lib/plGL.pl').
:- use_module('Lib/plGLU.pl').
:- use_module('Lib/plGLUT.pl').
:- use_module(library(clpr)).

:- dynamic  lst/1,
            gameScene/1,
            listenPress/1,
            addTile/1,
            height/1,width/1,
            return/2,
            var/2,
            globalDt/1,
            ddTile/1.


lst([]). %[[0,1], [0.0, 0.0], [10.0, 30.0], 2]

gameScene([[0.0, 0.0], [10.0, 30.0]]). %pos %size

moveSpeed(50).
listenPress(true).
addTile(false).

height(600.0).
width(800.0).
globalDt(0.1).

return(init, -1).


%-------------------------------------
getObjectByPos(X, Y, Obj, Id):-
    lst(TmpList),
    length(TmpList, LngTiles),
    recursiveSearch(TmpList, X, Y, Obj, Revers_Id),
    (
        (Revers_Id = -1) ->
            (   
                Id is -1      
            );
            (
                Id is LngTiles - Revers_Id - 1
            )  
    ).
       
recursiveSearch(List, X, Y, Result, Reverse_Id):-
    length(List, Size),
    (
        (Size > 0) ->
            (  
                [H|T] = List,
                [ID|_] = H,
                [Tmp_X,Tmp_Y] = ID,
                (
                    (Tmp_X = X, Tmp_Y = Y) ->
                        (   
                            Result = H,
                            length(T, TailSize),        
                            Reverse_Id is TailSize             
                        );
                        (
                            length(T, TailSize),
                            (
                                (TailSize > 0) ->
                                    (   
                                        recursiveSearch(T, X, Y, Result, Reverse_Id)          
                                    );
                                    (
                                        Reverse_Id is -1
                                    )  
                            )
                        )  
                )
            );
            (
                Reverse_Id is -1
            )
    ).

getObjectById(Id, Result):-
    lst(TmpList),
    rSearch(TmpList, Id, Result).

getLocObjById(List, Id, Result):-
    rSearch(List, Id, Result).

rSearch(List, Id, Result):-
    [H|T] = List,
    (
        (Id = 0) ->
            (  
                Result = H
            );
            (
                NewId is Id - 1,
                rSearch(T, NewId, Result)
            )
    ).


%------------------------------------- 

%-------------------------------------   
drawQuad(Pos, Size, Count, Type) :-     %if type 0 then Tile else BG
    [PX_L,PY_L] = Pos,
    [SX_L,SY_L] = Size,
    (
        (Type = 0) ->
            (   
                {Count = 2 ^ Ansver},
                Clr is 1.0 - (float(Ansver) - 1.0) / 10.0,
                glColor3f(Clr, Clr, Clr)
            );
            (
                glColor3f(0.2, 0.2, 0.6)
            )  
    ), %if
    PX is float(PX_L),
    PY is float(PY_L),
    SY is float(SY_L),
    SX is float(SX_L),
    PX_SX is float(PX) + float(SX),
    PY_SY is float(PY) + float(SY),
    kGL_QUADS(QD),
    glBegin(QD),
        glVertex3f(   PX, PY, 0.0),
        glVertex3f(   PX, PY_SY, 0.0),
        glVertex3f(   PX_SX, PY_SY, 0.0),
        glVertex3f(   PX_SX, PY, 0.0),
	glEnd.

%------------------------------------- 

%------------------------------------- 
draw():-
    gameScene(TmpGS),
    [GPos, GSize] = TmpGS,
    drawQuad(GPos, GSize, 0, 1),

    lst(TmpList),

    retract(listenPress(_)),
    assert(listenPress(true)),
    rdTiles(TmpList);

    listenPress(Lp),
    addTile(At),
    (
        (At = true) ->
            (
                (Lp = true) ->
                    (   
                        createRndTile(),
                        retract(addTile(_)),
                        assert(addTile(false))
                    );
                    (
                        write('')
                    )  
            );
            (
                write('')
            )  
    ). %if
    
    %создание нового элемента!!!!!!!! если addTile true и listenPress true то создать


rdTiles(List):-
    length(List, LL),
    LL > 0,
    [H|T] = List,
    [Tid, Pos, Size, Count] = H,

    drawQuad(Pos, Size, Count, 0),

    [X_id| Y_id] = Tid,
    X is X_id,
    Y is Y_id,
    getObjectByPos(X, Y, _, ResultId),
    move(ResultId, MoveActive),
    (
        (MoveActive = true) ->
            (   
                retract(listenPress(_)),
                assert(listenPress(false))
            );
            (
                write('')
            )  
    ), %if
    length(T, LngTiles),
    LngTiles > 0,
    rdTiles(T).


    
%------------------------------------- 

%------------------------------------- 
createTile(I, J):-
    gameScene(TmpGS),
    [GPos, GSize] = TmpGS,
    [PX, PY] = GPos,
    [SX, _] = GSize,  

    NewSize is SX / 4.0 - 12.5,
    TSize = [NewSize, NewSize],

    NewXp is PX + ((NewSize + 10.0) * I) + 10.0,
    NewYp is PY + ((NewSize + 10.0) * J) + 10.0,
    TPos = [NewXp, NewYp],
    TId = [I, J],

    random(0, 100, RndNum),
    (
        (RndNum > 90) ->
            (   
                TCount = 4
            );
            (
                TCount = 2
            )  
    ), %if
    Tile = [TId, TPos, TSize, TCount],
    lst(OldList),
    append(OldList, [Tile], NewList),

    retract(lst(_)),
    assert(lst(NewList)).
%------------------------------------- 

%------------------------------------- 
createRndTile():-
    lst(TmpList),
    length(TmpList, TileLength),
    CountPos is 16 - TileLength - 1,
    (
        (CountPos > 0) ->
            (   
                random(0, CountPos, RndNum)
            );
            (
                RndNum is 0
            )  
    ), %if
    rfCreate(0, 0, RndNum).

rfCreate(I, J, LastPass):-
    getObjectByPos(I, J, _,Id),
    (
        (Id = -1) ->
        (   
            (LastPass = 0) ->
            (   
                createTile(I, J)   
            );
            (
                %next
                Lp is LastPass - 1,
                (    
                    (J < 3) ->
                    (   
                        NewJ is J + 1,
                        NewI is I,
                        rfCreate(NewI,NewJ,Lp)
                    );
                    (
                        (I < 3) ->
                        (   
                            NewJ is 0,
                            NewI is I + 1,
                            rfCreate(NewI,NewJ,Lp)
                        )        
                    )         
                )  
            )         
        );
        (  
            %next  
            (J < 3) ->
            (   
                NewJ is J + 1,
                NewI is I,
                rfCreate(NewI,NewJ,LastPass)
            );
            (
                (I < 3) ->
                (   
                    NewJ is 0,
                    NewI is I + 1,
                    rfCreate(NewI,NewJ,LastPass)
                )        
            )         
        )  
    ).

%------------------------------------- 

%------------------------------------- 
startGame():-
    retract(lst(_)),
    assert(lst([])),
    width(W),
    height(H),
    Size is H / 1.2,
    GSize = [Size, Size],
    PX is W / 2.0 - Size / 2.0,
    PY is H / 2.0 - Size / 2.0,
    GPos = [PX, PY],
    NewGF = [GPos, GSize],
    retract(gameScene(_)),
    assert(gameScene(NewGF)),
    createRndTile(),
    createRndTile().

%------------------------------------- 

%------------------------------------- 
deleteFromGList(Id):-
    lst(TmpList),
    rDeleteElem(TmpList, Id, Result),
    retract(lst(_)),
    assert(lst(Result)).

rDeleteElem(List, Id, Result):-
    getLocObjById(List, Id, Elem),
    delete(List, Elem, Result).
%------------------------------------- 

%------------------------------------- 
relocateTile(Id, I, J):-
    getObjectById(Id, Elem),
    [_|T] = Elem,
    NewId = [I, J],
    append([NewId], T, NewEilem),
    deleteFromGList(Id),
    lst(TmpList),
    append(TmpList, [NewEilem], NewList),
    retract(lst(_)),
    assert(lst(NewList)).

%------------------------------------- 

%------------------------------------- 
move(Id, MoveActive):-
    getObjectById(Id, Tile),
    [Tid, Tpos, Tsize, Tcount] = Tile,
    [TSX, TSY] = Tsize,
    [TIX, TIY] = Tid,

    gameScene(TmpGS),
    [Gpos, _] = TmpGS,
    [GPX, GPY] = Gpos,

    Pt_x is (GPX + ((TSX + 10.0) * TIX) + 10.0),
    Pt_y is (GPY + ((TSY + 10.0) * TIY) + 10.0),

    Point = [Pt_x, Pt_y],
    shiftX(Tpos, Point, Result_1),
    shiftY(Result_1, Point, Result_2),
    equalPosX(Result_2, Point, Result_3),
    equalPosY(Result_3, Point, Result_4),
    (
        (Result_4 \= Tpos) ->
            (   
                MoveActive = true,
                FinalObj = [Tid, Result_4, Tsize, Tcount],
                deleteFromGList(Id),
                lst(OldList),
                append(OldList, [FinalObj], NewList),
                retract(lst(_)),
                assert(lst(NewList))
            );
            (
                MoveActive = false
            )  
    ).

   

equalPosX(Pos, Point, Result):-
    globalDt(Dt),
    moveSpeed(Ms),
    [PX, PY] = Pos,
    [PtX, _] = Point,
    (
        (abs(PX - PtX) =< (Ms * Dt)) ->
            (
                Result = [PtX, PY]
            );
            (
                Result = Pos
            )
    ).
equalPosY(Pos, Point, Result):-
    globalDt(Dt),
    moveSpeed(Ms),
    [PX, PY] = Pos,
    [_, PtY] = Point,
    (
        (abs(PY - PtY) =< (Ms * Dt)) ->
            (
                Result = [PX, PtY]
            );
            (
                Result = Pos
            )
    ).

shiftX(Pos, Point, Result):-
    globalDt(Dt),
    moveSpeed(Ms),
    [PX, PY] = Pos,
    [PtX, _] = Point,
    (
        (PX \= PtX) ->
            (
                (PtX - PX > 0) ->
                    (
                        NewPX is PX + Ms * Dt,
                        Result = [NewPX, PY]
                    );
                    (
                        NewPX is PX - Ms * Dt,
                        Result = [NewPX, PY]
                    )
            );
            (
                Result = Pos
            )
    ).
shiftY(Pos, Point, Result):-
    globalDt(Dt),
    moveSpeed(Ms),
    [PX, PY] = Pos,
    [_, PtY] = Point,
    (
        (PY \= PtY) ->
            (
                (PtY - PY > 0) ->
                    (
                        NewPY is PY + Ms * Dt,
                        Result = [PX, NewPY]
                    );
                    (
                        NewPY is PY - Ms * Dt,
                        Result = [PX, NewPY]
                    )
            );
            (
                Result = Pos
            )
    ).

%------------------------------------- 

%Не работает увеличение tile
%------------------------------------- 
shiftL(Id, CountLoop, ReturnCL):-
    %Получили список
    lst(TmpList),
    %Длинна списка
    length(TmpList, ListSize),
    (
        (Id < ListSize) ->
            (
            %Получили tile
            getObjectById(Id, Tile),
            [Tid|_] = Tile,
            [X_id|Y_id] = Tid,
            TmpX is X_id,
            (TmpX > 0) -> %-----
                (
                    TmpX_id is X_id - 1,%-----
                    TmpY_id is Y_id,
                    getObjectByPos(TmpX_id, TmpY_id, Ntile, Nid), 
                    (
                    (Nid = -1) ->
                        (
                            %переместить влево на 1
                            NXP is X_id - 1,
                            NYP is Y_id,
                            relocateTile(Id, NXP, NYP),
                            %сброс рекурсии 
                            NCL is CountLoop + 1,
                            shiftL(0, NCL, ReturnCL)
                        );
                        (
                            [_, _, _, NTCount] = Ntile,
                            [_, _, _, TCount] = Tile,
                            (
                            (NTCount = TCount) -> %сравнить размеры tile
                                (
                                    %удалить Ntile
                                    deleteFromGList(Nid),
                                    %получаем сново 
                                    getObjectByPos(TmpX, TmpY_id, _, MainTid),
                                    %переместить влево на 1 tile
                                    NXP is X_id - 1, %-----
                                    NYP is Y_id,
                                    relocateTile(MainTid, NXP, NYP),
                                    incTileByPos(NXP, NYP),
                                    %сброс рекурсии    
                                    NCL is CountLoop + 1,
                                    shiftL(0, NCL, ReturnCL)  
                                );
                                (     
                                    %следующий цикл       
                                    NCL is CountLoop + 1,
                                    NewId is Id + 1,
                                    shiftL(NewId, NCL, ReturnCL) 
                                )
                            )
                        )
                    )
                );
                (
                    %следующий цикл       
                    NCL is CountLoop + 1,
                    NewId is Id + 1,
                    shiftL(NewId, NCL, ReturnCL)       
                )
            );
            (
                ReturnCL is CountLoop
            )
    ).

shiftR(Id, CountLoop, ReturnCL):-
    %Получили список
    lst(TmpList),
    %Длинна списка
    length(TmpList, ListSize),
    (
        (Id < ListSize) ->
            (
            %Получили tile
            getObjectById(Id, Tile),
            [Tid|_] = Tile,
            [X_id|Y_id] = Tid,
            TmpX is X_id,
            (TmpX < 3) -> %-----
                (
                    TmpX_id is X_id + 1,%-----
                    TmpY_id is Y_id,
                    getObjectByPos(TmpX_id, TmpY_id, Ntile, Nid), 
                    (
                    (Nid = -1) ->
                        (
                            %переместить влево на 1
                            NXP is X_id + 1,
                            NYP is Y_id,
                            relocateTile(Id, NXP, NYP),
                            %сброс рекурсии 
                            NCL is CountLoop + 1,
                            shiftR(0, NCL, ReturnCL)
                        );
                        (
                            [_, _, _, NTCount] = Ntile,
                            [_, _, _, TCount] = Tile,
                            (
                            (NTCount = TCount) -> %сравнить размеры tile
                                (
                                    %удалить Ntile
                                    deleteFromGList(Nid),
                                    %получаем сново 
                                    getObjectByPos(TmpX, TmpY_id, _, MainTid),
                                    %переместить влево на 1 tile
                                    NXP is X_id + 1, %-----
                                    NYP is Y_id,
                                    relocateTile(MainTid, NXP, NYP),
                                    incTileByPos(NXP, NYP),
                                    %сброс рекурсии    
                                    NCL is CountLoop + 1,
                                    shiftR(0, NCL, ReturnCL)  
                                );
                                (     
                                    %следующий цикл       
                                    NCL is CountLoop + 1,
                                    NewId is Id + 1,
                                    shiftR(NewId, NCL, ReturnCL) 
                                )
                            )
                        )
                    )
                );
                (
                    %следующий цикл       
                    NCL is CountLoop + 1,
                    NewId is Id + 1,
                    shiftR(NewId, NCL, ReturnCL)       
                )
            );
            (
                ReturnCL is CountLoop
            )
    ).

shiftU(Id, CountLoop, ReturnCL):-
    %Получили список
    lst(TmpList),
    %Длинна списка
    length(TmpList, ListSize),
    (
        (Id < ListSize) ->
            (
            %Получили tile
            getObjectById(Id, Tile),
            [Tid|_] = Tile,
            [X_id|Y_id] = Tid,
            TmpY is Y_id,
            (TmpY > 0) -> %-----
                (
                    TmpX_id is X_id,%-----
                    TmpY_id is Y_id - 1,
                    getObjectByPos(TmpX_id, TmpY_id, Ntile, Nid), 
                    (
                    (Nid = -1) ->
                        (
                            %переместить влево на 1
                            NXP is X_id,
                            NYP is Y_id - 1,
                            relocateTile(Id, NXP, NYP),
                            %сброс рекурсии 
                            NCL is CountLoop + 1,
                            shiftU(0, NCL, ReturnCL)
                        );
                        (
                            [_, _, _, NTCount] = Ntile,
                            [_, _, _, TCount] = Tile,
                            (
                            (NTCount = TCount) -> %сравнить размеры tile
                                (
                                    %удалить Ntile
                                    deleteFromGList(Nid),
                                    %получаем сново 
                                    getObjectByPos(TmpX_id, TmpY, _, MainTid),
                                    %переместить влево на 1 tile
                                    NXP is X_id, %-----
                                    NYP is Y_id - 1,
                                    relocateTile(MainTid, NXP, NYP),
                                    incTileByPos(NXP, NYP),
                                    %сброс рекурсии    
                                    NCL is CountLoop + 1,
                                    shiftU(0, NCL, ReturnCL)  
                                );
                                (     
                                    %следующий цикл       
                                    NCL is CountLoop + 1,
                                    NewId is Id + 1,
                                    shiftU(NewId, NCL, ReturnCL) 
                                )
                            )
                        )
                    )
                );
                (
                    %следующий цикл       
                    NCL is CountLoop + 1,
                    NewId is Id + 1,
                    shiftU(NewId, NCL, ReturnCL)       
                )
            );
            (
                ReturnCL is CountLoop
            )
    ).

shiftD(Id, CountLoop, ReturnCL):-
    %Получили список
    lst(TmpList),
    %Длинна списка
    length(TmpList, ListSize),
    (
        (Id < ListSize) ->
            (
            %Получили tile
            getObjectById(Id, Tile),
            [Tid|_] = Tile,
            [X_id|Y_id] = Tid,
            TmpY is Y_id,
            (TmpY < 3) -> %-----
                (
                    TmpX_id is X_id,%-----
                    TmpY_id is Y_id + 1,
                    getObjectByPos(TmpX_id, TmpY_id, Ntile, Nid), 
                    (
                    (Nid = -1) ->
                        (
                            %переместить влево на 1
                            NXP is X_id,
                            NYP is Y_id + 1,
                            relocateTile(Id, NXP, NYP),
                            %сброс рекурсии 
                            NCL is CountLoop + 1,
                            shiftD(0, NCL, ReturnCL)
                        );
                        (
                            [_, _, _, NTCount] = Ntile,
                            [_, _, _, TCount] = Tile,
                            (
                            (NTCount = TCount) -> %сравнить размеры tile
                                (
                                    %удалить Ntile
                                    deleteFromGList(Nid),
                                    %получаем сново 
                                    getObjectByPos(TmpX_id, TmpY, _, MainTid),
                                    %переместить влево на 1 tile
                                    NXP is X_id, %-----
                                    NYP is Y_id + 1,
                                    relocateTile(MainTid, NXP, NYP),
                                    incTileByPos(NXP, NYP),
                                    %сброс рекурсии    
                                    NCL is CountLoop + 1,
                                    shiftD(0, NCL, ReturnCL)  
                                );
                                (     
                                    %следующий цикл       
                                    NCL is CountLoop + 1,
                                    NewId is Id + 1,
                                    shiftD(NewId, NCL, ReturnCL) 
                                )
                            )
                        )
                    )
                );
                (
                    %следующий цикл       
                    NCL is CountLoop + 1,
                    NewId is Id + 1,
                    shiftD(NewId, NCL, ReturnCL)       
                )
            );
            (
                ReturnCL is CountLoop
            )
    ).
%------------------------------------- 

%------------------------------------- 

incTileByPos(I, J):-
    getObjectByPos(I, J, Tile, Id),
    [Tid, Tpos, Tsize, Tcount] = Tile,
    NewCount is Tcount * 2,
    NewTile = [Tid, Tpos, Tsize, NewCount],
    deleteFromGList(Id),
    lst(OldList),
    append(OldList, [NewTile], NewList),
    retract(lst(_)),
    assert(lst(NewList)).

%------------------------------------- 

system_time(Value) :-
    get_time(Stamp),
    stamp_date_time(Stamp, DateTime, local),
    date_time_value(second, DateTime, SecTime),
    Value is SecTime / 1000.

remove_list([], _, []).
remove_list([X|Tail], L2, Result):- 
    member(X, L2), !, 
    remove_list(Tail, L2, Result). 

remove_list([X|Tail], L2, [X|Result]):- 
    remove_list(Tail, L2, Result).


display:-
        % defs
	kGL_COLOR_BUFFER_BIT(COLOR_BUFFER),
	% gl code
	glClear(COLOR_BUFFER),
        draw(),
	glFlush.

init:-
	% defs
	kGL_BLEND(BLEND),
	kGL_SRC_ALPHA(ALPHA),
	kGL_FLAT(FLAT),
	kGL_ONE_MINUS_SRC_ALPHA(MINUS_ALPHA),
	% gl code
	glEnable(BLEND),
	glBlendFunc(ALPHA, MINUS_ALPHA),
	glShadeModel(FLAT),
	glClearColor(0.0, 0.0, 0.0, 0.0).

reshape:-
	% defs
	X is 0,
	Y is 0,
	width(W),
	width(H),
	kGL_MODELVIEW(MV),
    kGL_PROJECTION(PR),
	% gl code
	glMatrixMode(PR),
	glLoadIdentity,
        gluOrtho2D(0.0, W, 0.0, W),
    glMatrixMode(MV),
	glLoadIdentity,
    	glViewport(X,Y,W,H).

idle:-
    display.    

main:-
	% defs
    startGame(),

	width(W),
	height(H),
	kGLUT_SINGLE(SINGLE),
	kGLUT_RGB(RGB),
	% gl code
	glutInit,
	glutInitDisplayMode(SINGLE \/ RGB),
	glutInitWindowSize(W, H),
	glutInitWindowPosition(0,0),
	glutCreateWindow('2048. WASD to control(uppercase), R to restart'),
	init,
    glutIdleFunc(idle),
	glutDisplayFunc,
	glutReshapeFunc,
	glutKeyboardFunc,
	glutMainLoop.


keyboard(87,_,_) :-
	shiftD(0,0,Loop),
    lst(List),
    length(List, Size),
    Loop > Size,
    retract(addTile(_)),
    assert(addTile(true)).


keyboard(83,_,_) :-
	shiftU(0,0,Loop),
    lst(List),
    length(List, Size),
    Loop > Size,
    retract(addTile(_)),
    assert(addTile(true)).
keyboard(65,_,_) :-
	shiftL(0,0,Loop),
    lst(List),
    length(List, Size),
    Loop > Size,
    retract(addTile(_)),
    assert(addTile(true)).
keyboard(68,_,_) :-
	shiftR(0,0,Loop),
    lst(List),
    length(List, Size),
    Loop > Size,
    retract(addTile(_)),
    assert(addTile(true)).
keyboard(27,_,_) :-
	glutDestroyWindow.
keyboard(82,_,_) :-
    startGame().