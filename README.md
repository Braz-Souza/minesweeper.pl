# Documentação do Agente Explorador – Jogo Campo Minado em Prolog

## 1. Introdução

Este documento descreve o agente explorador desenvolvido em Prolog, cuja finalidade é interagir com um ambiente representado por um campo minado. O agente utiliza um conjunto de regras e fatos dinâmicos para gerenciar e explorar o ambiente, identificando bombas, marcando posições e selecionando locais seguros. A base de conhecimento foi estruturada utilizando os recursos dinâmicos do Prolog, e as operações do agente são realizadas por meio de consultas (queries) interativas.

## 2. Definição do Ambiente
O ambiente é representado como uma matriz (grid) de tamanho fixo que simula um campo minado. Essa matriz é construída dinamicamente a partir de fatos e regras, os quais definem tanto a estrutura do campo quanto o estado de cada célula.

#### 1. Fatos e Dinamismo

- **Fatos estáticos:**  
  São definidos alguns parâmetros que estabelecem o estado inicial do ambiente. Por exemplo:  
  - `quantidade_bombas(9).`  
    Define que o campo terá 9 bombas.  
  - `tamanho_grid(9).`  
    Define que o grid é 9x9.  
  - `qtd_descobertos(0).`  
    Inicia o contador de células descobertas com valor zero.  
  - `finalizado(0).`  
    Indica que o jogo ainda não foi finalizado.

- **Fatos dinâmicos:**  
  São declarados com a diretiva `:- dynamic ...` para permitir modificações em tempo de execução. Por exemplo, os predicados `bomba/2`, `descoberto/3` e `marcado/2` podem ser atualizados conforme o agente interage com o ambiente.

#### 2. Construção do Grid

O grid é construído através de uma matriz gerada por regras que criam e organizam as linhas e colunas:

- **Regra para criar uma linha:**  
  A cláusula `cria_linha/4` constrói cada linha da matriz. Ela verifica se uma célula está marcada, descoberta ou oculta, definindo seu valor de exibição:  
  ```prolog
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
  ```
  Essa cláusula exemplifica como uma célula descoberta com valor especial (por exemplo, 9, que indica bomba) é tratada.

- **Construção completa da matriz:**  
  A regra `matriz/2` utiliza `cria_linha/4` para formar cada linha, gerando a matriz completa que representa o campo:
  ```prolog
  matriz(L, N) :- matriz(L, 1, N).
  matriz([], S, N) :- S is N+1, !.
  matriz([H|T], AUX, N) :- 
      S is AUX+1, 
      matriz(T, S, N),
      cria_linha(L, #, N, AUX),
      rev(L, H).
  ```
  Aqui, a função `rev/2` (que inverte listas) é utilizada para ajustar a ordem dos elementos na linha.

- **Fato que representa o campo:**  
  O predicado `campo/1` integra a construção do grid, vinculando o tamanho do grid definido pelo fato `tamanho_grid/1`:
  ```prolog
  campo(L) :- tamanho_grid(N), matriz(L, N).
  ```

#### 3. Manipulação do Estado do Ambiente

Várias cláusulas permitem a alteração do ambiente em resposta às ações do agente:

- **Reset do ambiente:**  
  A regra `reset_campo/0` redefine os parâmetros do jogo, zerando o contador de descobertas e removendo todos os fatos dinâmicos de bombas, células descobertas e marcadas:
  ```prolog
  reset_campo :- 
      retract(qtd_descobertos(_)),
      assertz(qtd_descobertos(0)),
      retract(finalizado(_)),
      assertz(finalizado(0)),
      retractall(descoberto(_,_,_)),
      retractall(bomba(_,_)),
      retractall(marcado(_,_)).
  ```

- **Atualização de descobertas:**  
  A cláusula `add_descoberto/0` incrementa o contador de células descobertas:
  ```prolog
  add_descoberto :- 
      retract(qtd_descobertos(N)),
      S is N+1,
      assertz(qtd_descobertos(S)).
  ```

#### 4. Distribuição de Bombas

A inserção das bombas é realizada de forma aleatória:

- **Geração de posição aleatória:**  
  A regra `random_pos/2` utiliza a biblioteca `random` para determinar uma posição dentro do grid:
  ```prolog
  random_pos(R, N) :-
      random_between(1, N, R).
  ```

- **Inserção recursiva de bombas:**  
  A regra `gerar_campo_bombas/1` itera a inserção de bombas conforme a quantidade definida, garantindo que cada nova bomba seja inserida em uma posição ainda não ocupada:
  ```prolog
  gerar_campo_bombas(0) :- !.
  gerar_campo_bombas(N) :-
      tamanho_grid(GRID),
      gerar_bomba(GRID),
      S is N-1,
      gerar_campo_bombas(S).
  ```

### 2.1. Resumo

A definição do ambiente é técnica e modular, permitindo que cada aspecto do jogo – desde a criação do grid, passando pela gestão dos estados das células (descobertas, marcadas ou ocultas) até a distribuição aleatória das bombas – seja definido por meio de cláusulas precisas e inter-relacionadas. Essa abordagem possibilita a execução dinâmica de operações, onde as ações do agente são refletidas em alterações do estado do ambiente, fundamental para a construção de um agente explorador robusto em PROLOG.

Esta estruturação por meio de fatos e regras, com o uso de cláusulas dinâmicas e recursivas, exemplifica a aplicação prática dos conceitos de agentes e ambientes interativos conforme a definição proposta na atividade.

## 3. Definição das Tarefas do Agente
O agente explorador no jogo **Campo Minado** executa tarefas que simulam as ações de um jogador humano, como **explorar células**, **marcar bombas**, **tomar decisões baseadas em lógica** e **verificar condições de vitória/derrota**. Cada tarefa é implementada por meio de regras em Prolog que interagem com a base de conhecimento dinâmica.

---

### **3.1. Tarefas Principais**
#### **1. `iniciar_jogo/0`**  
- **Descrição:** Prepara o ambiente para um novo jogo.  
- **Ações:**  
  - Reseta o estado do campo (`reset_campo/0`).  
  - Gera bombas aleatoriamente (`gerar_campo_bombas/1`).  
  - Exibe o grid inicial (`print_campo/0`).  
- **Exemplo de Consulta:**  
  ```prolog
  ?- iniciar_jogo.
  ```

#### **2. `selecionar/2` (Explorar Célula)**  
- **Descrição:** Revela o conteúdo de uma célula (segura ou bomba).  
- **Lógica:**  
  - Se a célula contém uma bomba, o jogo termina (`set_fim/0`).  
  - Se for segura, calcula bombas adjacentes (`olhar_ao_redor/3`) e:  
    - Se não há bombas próximas (`V=0`), explora células vizinhas recursivamente (`limpar_ao_redor/2`).  
    - Se há bombas, exibe o número (`descoberto/3`).  
- **Exemplo:**  
  ```prolog
  ?- selecionar(5, 5).  % Revela a célula central.
  ```

#### **3. `marcar/2` (Marcar/Desmarcar Bandeira)**  
- **Descrição:** Marca uma célula como possível bomba (⚑) ou remove a marca.  
- **Regras:**  
  - Não marca células já reveladas (`descoberto/3`).  
  - Alterna entre marcado e desmarcado (`assertz/retract`).  
- **Exemplo:**  
  ```prolog
  ?- marcar(3, 4).  % Coloca/remove bandeira na posição (3,4).
  ```

#### **4. `explorar_sistema/0` (Modo Autônomo)**  
- **Descrição:** O agente toma decisões autônomas para explorar o campo.  
- **Estratégias:**  
  - **Seleção Aleatória:** `selecionar_aleatorio/0` para células não marcadas.  
  - **Lógica Dedutiva:**  
    - Se uma célula revelada tem `V` bombas vizinhas e `V` células não reveladas ao redor, marca todas como bombas (`marcar_ao_redor_de/2`).  
    - Se todas as bombas vizinhas já estão marcadas, revela as células restantes (`limpar_ao_redor/2`).  
- **Exemplo:**  
  ```prolog
  ?- explorar_sistema.  % O agente joga sozinho.
  ```

---

### **3.2. Tarefas Auxiliares**
#### **1. `olhar_ao_redor/3` (Detectar Bombas Vizinhasa)**  
- **Descrição:** Calcula quantas bombas existem nas 8 células adjacentes.  
- **Uso:** Base para decisões de exploração e marcação.  

#### **2. `verify_end_game/0` (Verificar Fim de Jogo)**  
- **Condições de Vitória:**  
  - Todas as células seguras foram reveladas (`qtd_descobertos/1` = `GridSize² - Bombas`).  
- **Condições de Derrota:**  
  - Célula com bomba é selecionada (`select/2` chama `mostrar_bombas/0`).  

#### **3. `status_do_jogo/0` (Relatório de Progresso)**  
- **Exibe:**  
  - Tempo decorrido, bombas restantes, células descobertas e bandeiras corretas.  

---

### **3.4. Resumo**
As tarefas do agente combinam **interação direta** (via consultas) e **lógica autônoma** (exploração inteligente), refletindo as capacidades de um agente explorador. A base de conhecimento é atualizada dinamicamente para representar o estado do ambiente.  

## 4. Especificação do Conhecimento

Esta seção descreve como as **tarefas do agente** modificam dinamicamente a **base de conhecimento** (fatos em Prolog), permitindo que o agente acumule informações, tome decisões e atue sobre o ambiente.  

### **4.1. Acúmulo de Conhecimento**  
As seguintes tarefas **adicionam fatos** à base de conhecimento:  

#### **1. `selecionar/2` (Explorar Célula)**  
- **Fatos modificados:** `descoberto/3`, `qtd_descobertos/1`.  
- **Lógica:**  
  - Se a célula é segura, adiciona `descoberto(X, Y, V)`, onde `V` é o número de bombas vizinhas (0-8).  
  - Incrementa `qtd_descobertos/1` para rastrear progresso.  
- **Exemplo:**  
  ```prolog
  % Antes: descoberto(_, _, _) não existe para (5,5).  
  ?- selecionar(5, 5).  
  % Depois: assertz(descoberto(5, 5, 3)).  % Se houver 3 bombas próximas.
  ```

#### **2. `marcar/2` (Marcar Bandeira)**  
- **Fatos modificados:** `marcado/2`.  
- **Lógica:**  
  - Adiciona `marcado(X, Y)` se a célula não estiver revelada.  
- **Exemplo:**  
  ```prolog
  % Antes: marcado(_, _) não existe para (2,2).  
  ?- marcar(2, 2).  
  % Depois: assertz(marcado(2, 2)).
  ```

#### **3. `gerar_campo_bombas/1` (Inicialização)**  
- **Fatos modificados:** `bomba/2`.  
- **Lógica:**  
  - Adiciona `bomba(X, Y)` para cada bomba gerada aleatoriamente.  

---

### **4.2. Remoção de Conhecimento**  
As seguintes tarefas **removem fatos** da base:  

#### **1. `reset_campo/0` (Reiniciar Jogo)**  
- **Fatos removidos:** Todos os dinâmicos (`bomba/2`, `descoberto/3`, `marcado/2`).  
- **Lógica:**  
  - Usa `retractall/1` para limpar o estado anterior.  
  - Reinicia contadores (`qtd_descobertos/1`, `finalizado/1`).  

#### **2. `marcar/2` (Desmarcar Bandeira)**  
- **Fatos removidos:** `marcado/2`.  
- **Lógica:**  
  - Se a célula já está marcada, `retract(marcado(X, Y))` é executado.  

---

### **4.3. Consultas que Atualizam Conhecimento**  
| **Tarefa**            | **Fatos Adicionados**       | **Fatos Removidos**       | **Efeito**                                                                 |  
|-----------------------|----------------------------|---------------------------|----------------------------------------------------------------------------|  
| `selecionar(X,Y)`     | `descoberto(X,Y,V)`        | –                         | Revela células seguras ou bombas.                                          |  
| `marcar(X,Y)`         | `marcado(X,Y)`             | `marcado(X,Y)` (se existir)| Alterna entre marcar/desmarcar bandeiras.                                  |  
| `gerar_campo_bombas`  | `bomba(X,Y)`               | –                         | Preenche o campo com bombas aleatórias.                                    |  
| `reset_campo`         | –                          | Todos os fatos dinâmicos   | Prepara um novo jogo.                                                      |  

---

### **4.4. Exemplo de Fluxo de Conhecimento**  
```prolog
% Inicia jogo: gera bombas e fatos iniciais.
?- iniciar_jogo.  
% Base: [bomba(1,2), bomba(3,4), ..., qtd_descobertos(0)].

% Agente marca uma célula como possível bomba.
?- marcar(1, 1).  
% Base: [..., marcado(1,1)].

% Agente explora célula segura (sem bombas próximas).
?- selecionar(5, 5).  
% Base: [..., descoberto(5,5,0), qtd_descobertos(1)].

% Reinicia o jogo: remove todos os fatos dinâmicos.
?- reset_campo.  
% Base: [quantidade_bombas(9), tamanho_grid(9), qtd_descobertos(0)].
```

---

### **4.5. Conclusão**  
O agente **atualiza sua base de conhecimento** de forma dinâmica conforme interage com o ambiente, refletindo:  
1. **Exploração** (células reveladas).  
2. **Marcação** (hipóteses de bombas).  
3. **Reinicialização** (limpeza para novo jogo).  

Essa dinâmica é essencial para que o agente **tome decisões inteligentes** (ex.: evitar células com bombas) e **avalie o estado do jogo** (vitória/derrota).  


## 5. Atuação sobre o Ambiente – Pipeline de Consultas
Nesta seção, é detalhado o **pipeline de consultas** que exemplifica como o agente interage com o ambiente do Campo Minado. As consultas em Prolog representam as ações do agente, modificando o estado do jogo e acumulando conhecimento.  

---

### **5.1. Pipeline de Consultas**  
O pipeline abaixo simula um jogo completo, desde a inicialização até a vitória/derrota, passando por ações manuais e autônomas:  

#### **1. Inicialização do Ambiente**  
```prolog
?- iniciar_jogo.  
```
**Efeitos:**  
- Reseta o campo (`reset_campo/0`).  
- Gera 9 bombas aleatórias (`gerar_campo_bombas/1`).  
- Exibe o grid inicial (todas células ocultas como `#`).  

**Saída:**  
```plaintext
X -  1 2 3 4 5 6 7 8 9
1 - [#, #, #, #, #, #, #, #, #]
2 - [#, #, #, #, #, #, #, #, #]
...
9 - [#, #, #, #, #, #, #, #, #]
```

---

#### **2. Exploração Manual (Jogador Humano)**  
```prolog
?- selecionar(5, 5).  % Revela a célula central.
```
**Efeitos:**  
- Se for segura, exibe o número de bombas vizinhas (ex.: `3`) e revela células adjacentes se `V=0`.  
- Se for bomba, termina o jogo (`mostrar_bombas/0`).  

**Saída (caso seguro):**  
```plaintext
X -  1 2 3 4 5 6 7 8 9
...
5 - [#, #, #, #, 3, #, #, #, #]
```

---

#### **3. Marcação de Bandeiras**  
```prolog
?- marcar(4, 4).  % Marca possível bomba.
```
**Efeitos:**  
- Adiciona `marcado(4, 4)` e exibe `⚑` no grid.  

**Saída:**  
```plaintext
4 - [#, #, #, ⚑, #, #, #, #, #]
```

---

#### **4. Modo Autônomo (Agente Inteligente)**  
```prolog
?- explorar_sistema.  % Agente toma decisões.
```
**Efeitos:**  
- **Seleção Aleatória:** Escolhe células não marcadas (`selecionar_aleatorio/0`).  
- **Lógica Dedutiva:**  
  - Se uma célula revelada tem `V` bombas vizinhas e `V` células não reveladas ao redor, marca todas como bombas.  
  - Se todas as bombas vizinhas já estão marcadas, revela as células restantes.  

**Exemplo de Saída:**  
```plaintext
3 - [1, ⚑, 1, #, #, #, #, #, #]  % Agente marcou bombas e revelou células seguras.
```

---

#### **5. Verificação de Fim de Jogo**  
**Caso de Vitória:**  
- Quando todas as células seguras são reveladas (`verify_end_game/0`):  
  ```plaintext
  ===== VITÓRIA =====
  === STATUS DO JOGO ===
  Tempo decorrido: 02:30
  Campos descobertos: 72  % 9x9 - 9 bombas.
  ```

**Caso de Derrota:**  
- Ao selecionar uma bomba:  
  ```plaintext
  ===== DERROTA =====
  X -  1 2 3 4 5 6 7 8 9
  1 - [9, 1, 0, 0, 0, #, #, #, #]  % Bombas reveladas (9).
  ```

---

### **5.2. Exemplo Completo de Pipeline**  
```prolog
% 1. Inicia o jogo.
?- iniciar_jogo.

% 2. Jogador revela células.
?- selecionar(1, 1).  % Revela canto superior esquerdo.
?- marcar(2, 2).      % Marca possível bomba.

% 3. Agente autônomo continua.
?- explorar_sistema.

% 4. Verifica status.
?- status_do_jogo.
```

**Saída Final (Exemplo):**  
```plaintext
=== VITÓRIA ====

=== STATUS DO JOGO ===
Tempo decorrido: 01:45
Quantidade de bombas: 9
...

X -  1 2 3 4 5 6 7 8 9
1 - [0, 1, ⚑, 1, 0, 0, 0, 0, 0]
2 - [0, 1, 1, 1, 0, 0, 0, 0, 0]
...
```

## 6. Conclusão
O agente explorador em Prolog desenvolvido para o jogo **Campo Minado** demonstra com eficiência os princípios de **percepção, processamento e atuação** sobre um ambiente dinâmico. Através de uma base de conhecimento modular e regras bem definidas, o agente é capaz de:  

- **Interagir com o ambiente** por meio de ações como explorar células e marcar bombas.  
- **Acumular conhecimento** dinamicamente, atualizando fatos como `descoberto/3` e `marcado/2`.  
- **Tomar decisões autônomas** usando lógica dedutiva no modo `explorar_sistema`.  
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

% Regra que retorna true se o jogo acabou
has_game_ended :-
	quantidade_bombas(N),
	tamanho_grid(L),
	qtd_descobertos(X),	
	S is L*L - X,
       	N == S.


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
```