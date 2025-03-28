# Documentação do Agente Explorador – Jogo Campo Minado em Prolog

## 1. Introdução

Este documento descreve o agente explorador desenvolvido em Prolog, cuja finalidade é interagir com um ambiente representado por um campo minado. O agente utiliza um conjunto de regras e fatos dinâmicos para gerenciar e explorar o ambiente, identificando bombas, marcando posições e determinando o fim do jogo. A base de conhecimento foi estruturada utilizando os recursos dinâmicos do Prolog, e as operações do agente são realizadas por meio de consultas (queries) interativas.

## 2. Definição do Ambiente
O ambiente é estruturado como uma **grade (grid) bidimensional** que simula um campo de minas, muito similar ao jogo tradicional de *Minesweeper*. Nele, os elementos fundamentais são:

- **Tamanho e Distribuição:**  
  - O tamanho da grade é definido pelo fato `tamanho_grid/1`, configurado com valor 10, resultando em uma matriz 10x10.  
  - A quantidade de bombas é definida por `quantidade_bombas/1` (10 bombas), que são posicionadas aleatoriamente no grid através do predicado `gerar_campo_bombas/1`.

- **Estados das Células:**  
  Cada célula na matriz pode assumir diferentes estados, que são representados por:
  - **Descoberto:** Quando a célula foi explorada, o fato `descoberto/3` armazena sua posição e o valor (número de bombas adjacentes ou o valor 9 para indicar uma bomba).  
  - **Marcado:** Representado pelo fato `marcado/2`, quando o agente marca a célula, sinalizando a suspeita de uma bomba.  
  - **Inexplorado:** Células que ainda não foram selecionadas pelo agente.

- **Interação e Sensoriamento:**  
  - O agente interage com o ambiente por meio de operações como `selecionar/2` (para explorar uma célula) e `marcar/2` (para sinalizar possíveis bombas).  
  - O ambiente "percebe" a existência de bombas através dos predicados `e_bomba/3` e `olhar_ao_redor/3`, que calculam o número de bombas adjacentes a uma célula.
  - A visualização do ambiente é feita pela construção de uma matriz que representa o campo, com a função `print_campo` que utiliza regras para montar e exibir cada linha da grade.

- **Dinamicidade do Conhecimento:**  
  - O ambiente é dinâmico, permitindo a atualização dos fatos (por exemplo, incrementando o número de campos descobertos com `add_descoberto` ou reiniciando o campo com `reset_campo`).
  - A base de conhecimento é alterada em tempo real, onde novos fatos são assertados ou retractados conforme o progresso do jogo, representando a aquisição ou remoção de conhecimento do agente.

- **Objetivo do Agente no Ambiente:**  
  - O agente explorador deve interagir com essa grade para descobrir todas as células que não contenham bombas (atingindo assim a condição de vitória), ou, ao errar, desencadeia a visualização de todas as bombas, terminando o jogo com derrota.
  - Funções auxiliares como `verify_end_game` avaliam se o jogo deve terminar (por vitória ou derrota) com base na relação entre campos descobertos e a quantidade de bombas.

## 3. Definição das Tarefas do Agente

O agente explorador foi projetado para executar uma série de tarefas que permitem a interação dinâmica com o ambiente. Entre as principais tarefas, destacam-se:

- **Inicialização e Preparação do Ambiente:**  
  - **Iniciar o Jogo:**  
    O agente inicia o jogo por meio da regra `iniciar_jogo`, que executa a reinicialização dos estados (resetando o campo e a contagem de células descobertas), gera as bombas aleatoriamente conforme a quantidade definida e exibe o campo inicial.  
  - **Reset do Campo:**  
    Antes de cada partida, o ambiente é limpo utilizando `reset_campo`, removendo informações de descobertas, bombas e marcações anteriores.

- **Interação Direta com o Ambiente:**  
  - **Selecionar Células (Explorar):**  
    O agente pode explorar o ambiente selecionando uma posição com o predicado `selecionar(X,Y)`.  
    - Se a posição selecionada contiver uma bomba, o jogo exibe todas as bombas e encerra a partida com derrota.  
    - Se a célula não tiver bombas próximas (olhar_ao_redor retorna 0), o agente realiza a limpeza automática ao redor da posição, expandindo a área explorada.  
    - Caso haja bombas adjacentes, a célula exibe a contagem de bombas ao redor.
  
  - **Marcar Células:**  
    A tarefa de marcação, realizada através do predicado `marcar(X,Y)`, permite ao agente sinalizar as posições suspeitas de conter bombas.  
    - Se uma célula já foi marcada, a regra a desmarca, possibilitando correções na estratégia de marcação.  
    - Células já descobertas não podem ser marcadas, evitando inconsistências na base de conhecimento.

- **Processamento e Atualização do Conhecimento:**  
  - **Sensoriamento do Ambiente:**  
    Por meio da regra `olhar_ao_redor(X, Y, V)`, o agente calcula quantas bombas estão adjacentes a uma célula. Essa informação é fundamental para a tomada de decisão quanto à seleção ou marcação das células.
  
  - **Atualização dos Fatos Dinâmicos:**  
    A base de conhecimento é atualizada dinamicamente com fatos que representam:  
    - O número de células descobertas (`qtd_descobertos/1`),  
    - As células marcadas (`marcado/2`),  
    - As células descobertas e seus respectivos valores (`descoberto/3`),  
    possibilitando que o agente adquira ou remova informações conforme interage com o ambiente.

- **Verificação do Status do Jogo:**  
  - **Condição de Vitória/Derrota:**  
    Após cada ação de exploração ou marcação, o agente invoca `verify_end_game` para avaliar se as condições de vitória (todas as células seguras foram descobertas) ou de derrota (selecionou uma bomba) foram alcançadas.  
  - **Exibição do Status:**  
    A regra `status_do_jogo` apresenta informações como tempo decorrido, quantidade de bombas, tamanho do grid, número de campos descobertos e bandeiras colocadas, permitindo ao agente (e ao usuário) acompanhar o andamento da partida.

- **Exploração Automatizada:**  
  - **Algoritmo Exploratória:**  
    A tarefa `explorar_sistema` implementa uma abordagem de verificação de cada célula do grid, onde o agente:  
    - Verifica se o número de células não descobertas ao redor de uma posição corresponde ao número indicado na célula,  
    - Marca automaticamente as posições suspeitas,  
    - Seleciona aquelas que já possuem todas as bandeiras necessárias, expandindo a área segura.  
    Essa tarefa é parte de uma estratégia para facilitar a resolução do campo sem a necessidade de interações manuais em todas as posições.

## 4. Especificação do Conhecimento

- **Fatos Dinâmicos:**
  - **bombas/2:**  
    - Representa as posições das bombas no grid.  
    - São adicionadas dinamicamente através de `gerar_bomba/3` e removidas ou verificadas conforme a interação do agente.
  
  - **descoberto/3:**  
    - Armazena as células que foram exploradas, associando cada posição a um valor.  
    - Esse valor pode ser o número de bombas adjacentes ou um valor especial (como 9) para indicar que uma bomba foi encontrada.
  
  - **marcado/2:**  
    - Registra as células marcadas pelo agente como suspeitas de conter bombas.  
    - A marcação pode ser inserida ou removida (quando o agente corrige um erro) via a regra `marcar/2`.
  
  - **quantidade_bombas/1, tamanho_grid/1 e qtd_descobertos/1:**  
    - Esses fatos definem a configuração inicial do ambiente (tamanho do grid e número de bombas) e monitoram a evolução do jogo (número de células já descobertas).
  
  - **start_time/1:**  
    - Armazena o instante em que o jogo foi iniciado, possibilitando o cálculo do tempo decorrido e o status do jogo.

- **Atualização do Conhecimento:**
  - **Construção de Fatos:**
    - Ao selecionar uma célula com `selecionar/2`, o agente utiliza a regra `olhar_ao_redor/3` para contar as bombas adjacentes e, então, **asserta** um fato `descoberto(X, Y, V)` onde V é o número de bombas encontradas.  
    - Quando o agente marca uma célula com `marcar/2`, um fato `marcado(X, Y)` é **assertado** na base de conhecimento.
  
  - **Remoção de Fatos:**
    - Durante o reset ou a reinicialização do jogo (`reset_campo`), são **retractados** os fatos referentes a células descobertas, bombas e marcações, permitindo que o agente inicie uma nova partida com uma base de conhecimento limpa.
  
  - **Acúmulo de Conhecimento:**
    - A cada célula explorada, o agente atualiza o contador `qtd_descobertos/1` por meio de `add_descoberto`, acumulando conhecimento sobre o progresso do jogo.
    - A verificação de fim de jogo (por exemplo, através de `verify_end_game`) depende da relação entre os fatos armazenados (quantidade de células descobertas versus a quantidade total de células e bombas), permitindo que o agente determine se o objetivo foi alcançado.

- **Consultas e Atualizações Dinâmicas:**
  - **Sensoriamento e Decisão:**
    - O agente realiza consultas que combinam os fatos existentes para decidir as próximas ações. Por exemplo, ao chamar `olhar_ao_redor(X, Y, V)`, ele consulta os fatos `bomba/2` para somar a quantidade de ameaças próximas.
  - **Pipeline de Consultas:**
    - As funções que lidam com a marcação ou seleção de células (como `select/2`, `marcar/2` e `limpar_ao_redor/2`) formam um pipeline de operações que atualiza o conhecimento do agente conforme ele explora o ambiente.

## 5. Atuação sobre o Ambiente – Pipeline de Consultas

A interação do agente com o ambiente é realizada através de consultas (queries) que executam as tarefas descritas. Um exemplo de pipeline de atuação seria:

1. **Inicializar o Jogo:**  
   Inicia o ambiente e prepara a base de conhecimento com as bombas geradas e o campo configurado.  
   ```prolog
   iniciar_jogo.
   ```

2. **Selecionar Células (Explorar):**  
   O agente escolhe uma célula para explorar. Por exemplo, para selecionar a célula na linha 3, coluna 4:  
   ```prolog
   selecionar(3, 4).
   ```

3. **Marcar Células:**  
   Se o agente suspeita que determinada célula contém uma bomba, pode marcá-la. Por exemplo, para marcar a célula na linha 5, coluna 6:  
   ```prolog
   marcar(5, 6).
   ```

4. **Continuar a Exploração:**  
   O agente pode alternar entre selecionar e marcar outras posições, conforme a necessidade de explorar o campo. Por exemplo:  
   ```prolog
   selecionar(2, 3).
   marcar(4, 4).
   ```

> **Observação:**  
> Além deste pipeline interativo, é possível automatizar a ação do agente utilizando o predicado `explorar_sistema/0`, que implementa um algoritmo de verificação e marcação das células com base no conhecimento acumulado, permitindo que o agente aja de forma autônoma durante a exploração do campo.

Este pipeline exemplifica como as consultas básicas (inicializar, selecionar e marcar) podem ser encadeadas para conduzir o jogo, enquanto o `explorar_sistema/0` oferece uma abordagem automatizada para a resolução do campo.

## 6. Conclusão

A implementação apresentada demonstra um agente explorador para um jogo de campo minado, em que as tarefas de inicialização, seleção, marcação e exploração automática são executadas através de uma base de conhecimento dinâmica. A definição do ambiente, combinada com as regras de interação e os predicados responsáveis pela manipulação dos fatos, possibilita a construção de um sistema autônomo e interativo. Esse agente exemplifica conceitos de inteligência artificial, onde a percepção (através da leitura do grid) e a ação (selecionar ou marcar campos) se combinam para atingir o objetivo final – concluir o jogo com vitória ou identificar corretamente as bombas.

## 7. Anexos

### 7.1. Código Fonte Completo

```prolog
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

quantidade_bombas(10).
tamanho_grid(10).
qtd_descobertos(0).

% Coloca o valor de campos descobertos para 0
% E deleta todos os descobertos, bombas e marcados
reset_campo :- 
    retract(qtd_descobertos(_)),
    assertz(qtd_descobertos(0)),
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

% Gera uma posição aleatória R para um tamanho N
random_pos(R, N) :-
    random_between(1, N, R).

% Gera uma bomba dado o tamanho do grid fornecido
% Se a posição gerada aleatória já corresponder a uma bomba
% entra em recursão até gerar uma posição sem bomba
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

% Cria uma matriz que representará o campo
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
% Se existir R =:= 1, caso contrário R =:= 0
e_bomba(X, Y, 0) :- \+bomba(X, Y), !.
e_bomba(X, Y, 1) :- bomba(X, Y).

% Regra que verifica todos os campos ao redor da posição fornecida
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

% Regra para verificar se a posição fornecida fica fora do grid
fora_do_grid(X, Y) :-
    tamanho_grid(N),
    (X<1; Y<1; X>N; Y>N), !.

% Regra para seleção de uma posição
select(X, Y) :- 
    fora_do_grid(X, Y), !.
select(X, Y) :- 
    descoberto(X, Y, _), !.
select(X, Y) :- 
    bomba(X, Y), 
    mostrar_bombas,
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
verify_end_game :- 
    quantidade_bombas(N),
    tamanho_grid(L),
    qtd_descobertos(X),    
    S is L*L - X,
    N =:= S,    
    nl, write('===== VITÓRIA ====='), nl,
    status_do_jogo.
verify_end_game :- !.

% Regra que retorna true se o jogo acabou
has_game_ended :- 
    quantidade_bombas(N),
    tamanho_grid(L),
    qtd_descobertos(X),    
    S is L*L - X,
    N == S.

% Faz a seleção da posição definida e mostra o campo
selecionar(X, Y) :-
    select(X, Y),
    print_campo.

% Define o campo como marcado se já estava marcado ou desmarca se já estava marcado
marcar(X, Y) :-
    descoberto(X, Y, _), !.
marcar(X, Y) :-
    marcado(X, Y),
    retract(marcado(X,Y)), 
    print_campo, !.
marcar(X, Y) :- 
    assertz(marcado(X,Y)),
    print_campo.

% Inicia o jogo: reinicia os descobertos, gera bombas e imprime o campo
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

%% COMO JOGAR?
% 1) Inicie o jogo com a regra `iniciar_jogo.`
% 2) Escolha um campo para selecionar com `selecionar(X,Y)` (X: linha, Y: coluna)
% 3) Escolha um campo para marcar (possível bomba) com `marcar(X,Y)`
% 4) Ao selecionar todos os campos permitidos (ou marcar todas as bombas corretas),
%    o jogo termina com vitória.

%% TODO
% fim_jogo :- definir fim de jogo e apresentar o jogo completo
% vitoria :- definir tela de vitória apresentando junto os status_do_jogo
% derrota :- definir tela de derrota apresentando junto os status_do_jogo
% status_do_jogo :- apresentar status do modo de jogo, tempo, quantidade de bombas e campo
% explorar_sistema :- definir algoritmo que ao executar explora o sistema do campo

%% Algoritmo exploratório simples
explorar_sistema :-
    has_game_ended, !.
explorar_sistema :-
    verificar_cada_campo,
    marcar_possiveis,
    selecionar_campos_garantidos,
    print_campo.

verificar_cada_campo :-
    tamanho_grid(GRID),
    CAMPOS is GRID*GRID,
    verificar_cada_campo(CAMPOS, GRID).

verificar_cada_campo(0, _) :- !.
verificar_cada_campo(N, GRID) :-
    X is (N-1)//GRID + 1,
    Y is (N-1) mod GRID + 1,
    NewN is N-1,
    marcar_possiveis(X, Y),
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
    marcar(UP, Y), 
    marcar(DOWN, Y), 
    marcar(X, LEFT), 
    marcar(X, RIGHT),
    marcar(UP, LEFT), 
    marcar(DOWN, LEFT), 
    marcar(UP, RIGHT), 
    marcar(DOWN, RIGHT).

selecionar_campos_garantidos :-
    write("Selecionando campos garantidos\n").
```

## Como jogar?

1) Inicie o agente explorador do campo minado com `iniciar_jogo.`. Por padrão a quantidade de bombas é 10 e o tamanho do grid é 8x8.
2) Escolha um campo para explorar com `selecionar(X,Y)`, com X para a linha escolhida, e Y para a coluna escolhida.
3) Escolha um campo para marcar com a possibilidade de bomba com `marcar(X,Y)`, com X para a linha escolhida, Y para a coluna escolhida.
4) Após selecionar todos os campos permitidos o jogo termina com vitoria. Ou a derrota se houver alguma exploração de bomba com a consulta `selecionar(X,Y)`.