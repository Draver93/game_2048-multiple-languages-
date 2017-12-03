:- module(plOpenGL,
	[
	   loadGLTextures/4,
	   sleep/1,
	   size/2
	]).


%%	loadGLTexture(+Filename, -Width, -Height, -Data)
% Load Texture
loadGLTextures(Filename,Width,Height,Data):-
    c_loadGLTextures(Filename,Width,Height,Data).

%%	sleep(+X)
% Sleep for X seconds
sleep(X):-
	X1 = X,
	c_sleep(X1).

size([],0).
size([_|T],N) :- size(T,N1), N is N1+1.










