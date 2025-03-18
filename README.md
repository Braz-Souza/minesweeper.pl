# minesweeper.pl

Um agente exploratório de um campo minado em prolog.

## Como jogar?

1) Inicie o agente explorador do campo minado com `iniciar_jogo.`. Por padrão a quantidade de bombas é 10 e o tamanho do grid é 8x8.
2) Escolha um campo para explorar com `selecionar(X,Y)`, com X para a linha escolhida, e Y para a coluna escolhida.
3) Escolha um campo para marcar com a possibilidade de bomba com `marcar(X,Y)`, com X para a linha escolhida, Y para a coluna escolhida.
4) Após selecionar todos os campos permitidos o jogo termina com vitoria. Ou a derrota se houver alguma exploração de bomba com a consulta `selecionar(X,Y)`.

