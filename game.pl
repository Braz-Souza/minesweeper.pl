% Todo o documento abaixo foi documentado usando js doc
:- use_module(library(random)).
:- dynamic bomba/2.
:- dynamic campo/1.
:- dynamic descoberto/3.
:- dynamic marcado/2.
:- dynamic quantidade_bombas/1.
:- dynamic tamanho_grid/1.
:- dynamic qtd_descobertos/1.
:- dynamic start_time/1.
:- dynamic finalizado/1.

quantidade_bombas(9).
tamanho_grid(9).
qtd_descobertos(0).
finalizado(0).

% Define o jogo como finalizado
set_fim :- 
	retract(finalizado(_)),
	assertz(finalizado(1)).

% Coloca o falor de campos descobertos para 0
% E deleta todos os descobertos, bombas e marcados
reset_campo :- 
	retract(qtd_descobertos(_)),
	assertz(qtd_descobertos(0)),
	retract(finalizado(_)),
	assertz(finalizado(0)),
	retractall(descoberto(_,_,_)),
	retractall(bomba(_,_)),
	retractall(marcado(_,_)).

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
mostrar_bombas :- !.

% Gera uma posição aleatoria R para um tamanho N
random_pos(R, N) :-
	random_between(1, N, R).

% Gera uma bomba dado o tamanho do grid fornecido
% Se a posicao gerada aleatoria ja corresponder uma bomba
% entre em recursao ate gerar uma posição sem bomba
gerar_bomba(GRID) :- 
	random_pos(X, GRID),
	random_pos(Y, GRID),
	gerar_bomba(GRID, X, Y).
gerar_bomba(GRID, X, Y) :-
	bomba(X, Y), 
	gerar_bomba(GRID), !.
gerar_bomba(_, X, Y) :- 
	assertz(bomba(X,Y)).

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
    tamanho_grid(N),
    numlist(1, N, Numbers),
    maplist(atom_number, Atoms, Numbers),
    atomic_list_concat(Atoms, ' ', Header),
    format('\nX -  ~w\n', [Header]),
    campo(L), 
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
	select(UP, Y),
	select(DOWN, Y),
	select(X, LEFT),
	select(X, RIGHT),
	select(UP, LEFT),
	select(DOWN, LEFT),
	select(UP, RIGHT),
	select(DOWN, RIGHT).

% Regra para verificar se a posicao fornecida fica fora do grid
fora_do_grid(X, Y) :-
	tamanho_grid(N),
	(X<1; Y<1; X>N;Y>N), !.

% Regra para fazer a seleção de uma posição
% para a recursão se o 
% 	campo selecionado está fora do grid definido
% 	campo selecionado está marcado no momento
% 	campo selecionado ja foi descoberto
% Se encontrar uma bomba o jogo acaba mostrando as bombas
% Se o campo selecionado não houver bombas ao redor
% 	seleciona todos os campos ao seu redor
% Se o campo houver bombas ao redor mostre a quantidade no campo 
% X: Pos x para ser selecionada
% Y: Pos y para ser selecionada
select(X, Y) :- 
	fora_do_grid(X, Y), !.

select(X, Y) :- 
	descoberto(X, Y, _), !.

select(X, Y) :-
	marcado(X, Y), !.

select(X, Y) :- 
	bomba(X, Y), 
	mostrar_bombas,
	set_fim,
	nl, write('===== DERROTA ====='), nl,
	status_do_jogo, !.

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
verify_end_game :- 
	quantidade_bombas(N),
	tamanho_grid(L),
	qtd_descobertos(X),	
	S is L*L - X,
       	N =:= S,	
	set_fim,
	nl, write('===== VITÓRIA ====='), nl,
	status_do_jogo.

verify_end_game :- !.

% Faz a selecao da posicao definida e mostra o campo
selecionar(X, Y) :-
	select(X, Y),
	print_campo.

% Define o campo passado como marcado se ja estava marcado
% 	ou desmarca se já estava marcado
% 	ou não marca se ja esta descoberto
marcar(X, Y) :-
	descoberto(X, Y, _), !.

marcar(X, Y) :-
	marcado(X, Y),
	retract(marcado(X,Y)),
	print_campo, !.

marcar(X, Y) :- 
	assertz(marcado(X,Y)),
	print_campo.

% Define a marcação sem fazer print do campo

% Faz o inicio do jogo, reiniciando a quantidade de descobertos
% gerando as bombas necessarias para o jogo iniciar
% faz o print do campo
iniciar_jogo :-
    retractall(start_time(_)),
    get_time(StartTime),
    assertz(start_time(StartTime)),
	reset_campo,
	quantidade_bombas(N),
	gerar_campo_bombas(N),
	print_campo.

format_time(Seconds, Formatted) :-
    Minutes is floor(Seconds / 60),
    RemainingSeconds is integer(Seconds - Minutes * 60),
    format(atom(Formatted), '~`0t~d~2|:~`0t~d~2|', [Minutes, RemainingSeconds]).

status_do_jogo :-
    start_time(Start),
    get_time(End),
    Elapsed is End - Start,
    format_time(Elapsed, TimeString),
    quantidade_bombas(Bombs),
    tamanho_grid(Size),
    qtd_descobertos(Uncovered),
    findall(_, marcado(_, _), Flags),
    length(Flags, FlagsCount),
    findall(1, (marcado(X, Y), bomba(X, Y)), CorrectFlagsList),
    length(CorrectFlagsList, CorrectFlags),
    nl,
    write('=== STATUS DO JOGO ==='), nl,
    write('Tempo decorrido: '), write(TimeString), nl,
    write('Quantidade de bombas: '), write(Bombs), nl,
    write('Tamanho do grid: '), write(Size), nl,
    write('Campos descobertos: '), write(Uncovered), nl,
    write('Bandeiras colocadas: '), write(FlagsCount), nl,
    write('Bandeiras corretas: '), write(CorrectFlags), write('/'), write(Bombs), nl,
    write('Campo final:'), nl.

explorar_sistema :-
        selecionar_aleatorio,
        explorar_sistema(3), !.

explorar_sistema(0) :-
        finalizado(0),
        explorar_sistema, !.
explorar_sistema(0) :-
        finalizado(1),
        print_campo, !.

explorar_sistema(_) :-
		finalizado(1),
		print_campo, !.

explorar_sistema(N) :-
        verificar_cada_campo,
        S is N-1,
        explorar_sistema(S).

verificar_cada_campo :-
        tamanho_grid(GRID),
        CAMPOS is GRID*GRID,
        verificar_cada_campo(CAMPOS, GRID).

verificar_cada_campo(0, _) :- !.
verificar_cada_campo(_, _) :-
		finalizado(1), !.
verificar_cada_campo(N, GRID) :-
        X is (N-1)//GRID +1,
        Y is (N-1) mod GRID +1,
        NewN is N-1,
        marcar_possiveis(X, Y),
        selecionar_possiveis_de(X, Y),
        verificar_cada_campo(NewN, GRID).

marcar_possiveis(X, Y) :-
        descoberto(X, Y, V),
        nao_descoberto_ao_redor(X, Y, ND),
        V == ND,
        marcar_ao_redor_de(X,Y).
marcar_possiveis(_, _) :- !.

e_descoberto(X, Y, 0) :-
        fora_do_grid(X, Y), !.
e_descoberto(X, Y, 0) :-
        descoberto(X,Y,_), !.
e_descoberto(X, Y, 1) :-
        \+descoberto(X,Y,_).

nao_descoberto_ao_redor(X, Y, R) :-
        UP is X-1, DOWN is X+1, LEFT is Y-1, RIGHT is Y+1,
        e_descoberto(UP, Y, R1),
        e_descoberto(DOWN, Y, R2),
        e_descoberto(X, LEFT, R3),
        e_descoberto(X, RIGHT, R4),
        e_descoberto(UP, LEFT, R5),
        e_descoberto(DOWN, LEFT, R6),
        e_descoberto(UP, RIGHT, R7),
        e_descoberto(DOWN, RIGHT, R8),
        R is R1 + R2 + R3 + R4 + R5 + R6 + R7 + R8.

marcar_ao_redor_de(X, Y) :-
        UP is X-1, DOWN is X+1, LEFT is Y-1, RIGHT is Y+1,
        mark(UP, Y),
        mark(DOWN, Y),
        mark(X, LEFT),
        mark(X, RIGHT),
        mark(UP, LEFT),
        mark(DOWN, LEFT),
        mark(UP, RIGHT),
        mark(DOWN, RIGHT).

mark(X, Y) :-
        fora_do_grid(X, Y), !.

mark(X, Y) :-
        descoberto(X, Y, _), !.

mark(X,Y) :-
        marcado(X,Y), !.

mark(X, Y) :-
        assertz(marcado(X,Y)).

selecionar_possiveis_de(X, Y) :-
        descoberto(X, Y, V),
        marcado_ao_redor(X, Y, M),
        V == M,
        limpar_ao_redor(X,Y).
selecionar_possiveis_de(_, _) :- !.

e_marcado(X, Y, 0) :-
        fora_do_grid(X, Y), !.
e_marcado(X, Y, 0) :-
        \+marcado(X,Y), !.
e_marcado(X, Y, 1) :-
        marcado(X,Y).

marcado_ao_redor(X, Y, R) :-
        UP is X-1, DOWN is X+1, LEFT is Y-1, RIGHT is Y+1,
        e_marcado(UP, Y, R1),
        e_marcado(DOWN, Y, R2),
        e_marcado(X, LEFT, R3),
        e_marcado(X, RIGHT, R4),
        e_marcado(UP, LEFT, R5),
        e_marcado(DOWN, LEFT, R6),
        e_marcado(UP, RIGHT, R7),
        e_marcado(DOWN, RIGHT, R8),
        R is R1 + R2 + R3 + R4 + R5 + R6 + R7 + R8.

selecionar_aleatorio :-
        tamanho_grid(GRID),
        random_pos(X, GRID),
        random_pos(Y, GRID),
        selecionar_aleatorio(X, Y).
selecionar_aleatorio(X, Y) :-
        marcado(X, Y),
        selecionar_aleatorio, !.
selecionar_aleatorio(X, Y) :-
        descoberto(X, Y, _),
        selecionar_aleatorio, !.
selecionar_aleatorio(X, Y) :-
        select(X, Y).
