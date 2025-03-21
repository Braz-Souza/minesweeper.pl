% Todo o documento abaixo foi documentado usando js doc
:- use_module(library(random)).
:- dynamic bomba/2.
:- dynamic campo/1.
:- dynamic descoberto/3.
:- dynamic marcado/2.
:- dynamic quantidade_bombas/1.
:- dynamic tamanho_grid/1.
:- dynamic qtd_descobertos/1.

quantidade_bombas(10).
tamanho_grid(8).
qtd_descobertos(0).

% Deleta todos os descobertos
delete_all_descobertos :- !.
delete_all_descobertos :-
	descoberto(X, Y, _),
	retract(descoberto(X,Y,_)),
	delete_all_descobertos.

% Coloca o falor de campos descobertos para 0
% E deleta todos os descobertos
reset_descobertos :- 
	retract(qtd_descobertos(_)),
	assertz(qtd_descobertos(0)),
	delete_all_descobertos.

% Adiciona um a quantidade de campos descobertos
add_descoberto :- 
	retract(qtd_descobertos(N)),
	S is N+1,
	assertz(qtd_descobertos(S)).

% Apresenta todas as bombas presentes no mapa
mostrar_bombas :- 
	bomba(X, Y), 
	\+descoberto(X,Y,9), 
	assertz(descoberto(X, Y, 9)), 
	mostrar_bombas.

% Gera uma posição aleatoria R para um tamanho N
random_pos(R, N) :-
	random_between(1, N, R).

% Gera uma bomba dado o tamanho do grid fornecido
% Se a posicao gerada aleatoria ja corresponder uma bomba
% entre em recursao ate gerar uma posição sem bomba
gerar_bomba(GRID) :- 
	random_pos(X, GRID),
	random_pos(Y, GRID),
	\+bomba(X,Y),
	assertz(bomba(X,Y)), !.
gerar_bomba(GRID) :- 
	random_pos(X, GRID),
	random_pos(Y, GRID),
	bomba(X,Y),
	gerar_bomba(GRID).	

% Gera um campo de bombas, com a quantidade sendo
% definida pelo fato quantidade_bombas/1
gerar_campo_bombas(0) :- !.
gerar_campo_bombas(N) :-
	tamanho_grid(GRID),
	gerar_bomba(GRID),
	S is N-1,
	gerar_campo_bombas(S).

% Reverte uma lista
rev(L,R) :- reverse(L, R, []).
reverse([],L,L). 
reverse([H|T],R,Acc) :- reverse(T,R,[H|Acc]).

% Cria uma linha para o campo
cria_linha([], _, 0, _) :- !.
cria_linha(['X'|T], C, N, X) :- 
	descoberto(X, N, 9),
	S is N-1,
	cria_linha(T, C, S, X), !.
cria_linha([H|T], C, N, X) :- 
	descoberto(X, N, V),
	S is N-1,
	H is V,
	cria_linha(T, C, S, X), !.
cria_linha(['⚑'|T], C, N, X) :- 
	marcado(X, N),
	S is N-1,
	cria_linha(T, C, S, X), !.
cria_linha([H|T], H, N, X) :- 
	S is N-1,
	cria_linha(T, H, S, X).

% Cria uma matriz que representara o campo
matriz(L, N) :- matriz(L, 1, N).
matriz([], S, N) :- S is N+1, !.
matriz([H|T], AUX, N) :- 
	S is AUX+1, 
	matriz(T, S, N),
	cria_linha(L, #, N, AUX),
	rev(L, H).

% Função para printar a matriz definida
print_matriz(L, N) :- print_matriz(L, 0, N).
print_matriz([], N, N) :- !.
print_matriz([H|T], AUX, N) :- 
	NAUX is AUX+1,
	write(NAUX), 
	write(' - '),
	write(H), 
	nl, 
	print_matriz(T, NAUX, N).

% Definição do campo
campo(L) :- tamanho_grid(N), matriz(L, N).

% Definição para printar o campo
print_campo :- 
	write('\nX -  1 2 3 4 5 6 7 8\n'), % TODO: trocar para poder ficar o tamanho modularizado
	campo(L), 
	tamanho_grid(N),
	print_matriz(L, N).

% Regra para ver se existe uma bomba 
% se existir R=:=1, caso R=:=0
e_bomba(X, Y, 0) :- \+bomba(X, Y), !.
e_bomba(X, Y, 1) :- bomba(X, Y).

% Regra que verifica todos os campos ao redor da posição fornecida
% X: Posição x para verificar ao redor
% Y: Posição y para verificar ao redor
% V: Numero entre 0 e 8, representando a quantidade de bombas ao redor
olhar_ao_redor(X, Y, V) :- 
	UP is X-1, DOWN is X+1, LEFT is Y-1, RIGHT is Y+1,
	e_bomba(UP, Y, R1), 
	e_bomba(DOWN, Y, R2), 
	e_bomba(X, LEFT, R3), 
	e_bomba(X, RIGHT, R4),
	e_bomba(UP, LEFT, R5), 
	e_bomba(DOWN, LEFT, R6), 
	e_bomba(UP, RIGHT, R7), 
	e_bomba(DOWN, RIGHT, R8),
	V is R1 + R2 + R3 + R4 + R5 + R6 + R7 + R8.

% Regra para limpar campos ao redor da posição fornecida 
% X: Posição x para limpar ao redor 
% Y: Posição y para limpar ao redor
limpar_ao_redor(X, Y) :- 
	UP is X-1, DOWN is X+1, LEFT is Y-1, RIGHT is Y+1,
	select(UP, LEFT),
	select(UP, RIGHT),
	select(UP, Y),
	select(DOWN, LEFT),
	select(DOWN, RIGHT),
	select(DOWN, Y),
	select(X, LEFT),
	select(Y, RIGHT).


% Regra para fazer a seleção de uma posição
% para a recursão se o 
% 	campo selecionado está fora do grid definido
% 	campo selecionado ja foi descoberto
% Se encontrar uma bomba o jogo acaba mostrando as bombas
% Se o campo selecionado não houver bombas ao redor
% 	seleciona todos os campos ao seu redor
% Se o campo houver bombas ao redor mostre a quantidade no campo 
% X: Pos x para ser selecionada
% Y: Pos y para ser selecionada
select(X, Y) :- 
	tamanho_grid(N),
	(X<1; Y<1; X>N;Y>N), !.
select(X, Y) :- descoberto(X, Y, _), !.

select(X, Y) :- 
	bomba(X, Y), 
	write('JOGO FINALIZADO, BOMBA SELECIONADA!'),
	mostrar_bombas, !.

select(X, Y) :-  
	olhar_ao_redor(X,Y, 0),
	assertz(descoberto(X, Y, 0)),
	limpar_ao_redor(X,Y),
	add_descoberto,
	verify_end_game, !.

select(X, Y) :-  
	olhar_ao_redor(X,Y,V), 
	assertz(descoberto(X, Y, V)),
	add_descoberto,
	verify_end_game, !.

% Regra para verificar o fim de jogo
% Verificando se a area do grid é igual 
% a soma entre os itens descobertos e as bombas
verify_end_game :- !.
verify_end_game :- 
	quantidade_bombas(N),
	tamanho_grid(L),
	qtd_descobertos(X),	
	S is L*L - X,
       	N == S,	
	write('Fim de jogo atingido, parabéns!!').

% Faz a selecao da posicao definida e mostra o campo
selecionar(X, Y) :-
	select(X, Y),
	print_campo.

% Define o campo passado como marcado se ja estava marcado
% 	ou desmarca se já estava marcado
marcar(X, Y) :-
	marcado(X, Y),
	retract(marcado(X,Y)), 
	print_campo, !.

marcar(X, Y) :- 
	assertz(marcado(X,Y)),
	print_campo.

% Faz o inicio do jogo, reiniciando a quantidade de descobertos
% gerando as bombas necessarias para o jogo iniciar
% faz o print do campo
iniciar_jogo :-
	reset_descobertos,
	quantidade_bombas(N),
	gerar_campo_bombas(N),
	print_campo.


%% COMO JOGAR?
% 1) inicie o jogo com a regra `iniciar_jogo.`
% 2) Escolha um campo para selecionar com `selecionar(X,Y)` trocando
% 	X para a linha escolhida
% 	Y para a coluna escolhida
% 3) Escolha um campo para marcar com a possibilidade de bomba com `marcar(X,Y)` trocando
% 	X para a linha escolhida
% 	Y para a coluna escolhida
% 4) Após selecionar todos os campos permitidos (TODO: ou marcar todas as bombas corretas)
% 	o jogo termina com vitoria

%% TODO
% fim_jogo :- definir fim de jogo e apresentar o jogo completo
% vitoria :- definir tela de vitoria apresentando junto os status_do_jogo
% derrota :- definir tela de derrota apresentando junto os status_do_jogo
% status_do_jogo :- apresentar status do modo de jogo, tempo, quantidade de bombas e campo
% explorar_sistema :- definir algoritmo que ao executar explora o sistema do campo

