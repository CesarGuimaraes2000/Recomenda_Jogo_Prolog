% ARQUIVO DE REGRAS (LÓGICA DO SISTEMA)

:- use_module(library(lists)).
:- use_module(library(random)).

% 1. Configurações Iniciais

:- dynamic possui/2.

% 2. Carregamento das Bases de Dados

:- ensure_loaded('jogos_populares.pl').
:- exists_file('biblioteca.pl') -> ensure_loaded('biblioteca.pl') ; true.

% REGRA BÁSICA: DISPONIBILIDADE

jogo_disponivel(ID, Nome, Preco, Modo, Tags) :-
    jogo(ID, Nome, Preco, Modo, Tags),
    \+ possui(ID, _).


% REGRA PRINCIPAL: RECOMENDAÇÃO POR PONTUAÇÃO (SCORE)

% 1. Filtra jogos por preço.
% 2. Calcula Score = Quantas categorias o jogo tem em comum com o desejado.
% 3. Ordena e retorna os Top 5.

recomendar_top_5(CatsDesejadas, PrecoMin, PrecoMax, Top5Jogos) :-
    findall(
        par(Score, item(Nome, Preco, Modo, Tags)),
        (
            % Busca candidatos válidos
            jogo_disponivel(_, Nome, Preco, Modo, Tags),
            Preco >= PrecoMin, Preco =< PrecoMax,
            
            % Calcula pontuação (intersecção de categorias)
            calcular_interseccao(CatsDesejadas, Tags, Score),
            
            % Garante que tenha pelo menos 1 categoria em comum
            Score > 0
        ),
        TodosCandidatos
    ),
    
    % Ordena a lista de pares baseando-se no Score (1º argumento do par)
    sort(1, @>=, TodosCandidatos, CandidatosOrdenados),
    
    % Extrai apenas os dados do jogo, descartando o score auxiliar
    extrair_itens(CandidatosOrdenados, ItensLimpos),
    
    % Pega os 5 primeiros
    pegar_primeiros(5, ItensLimpos, Top5Jogos).

% AUXILIARES

% Calcula quantos elementos de Lista1 estão presentes em Lista2
calcular_interseccao(Lista1, Lista2, Tamanho) :-
    intersection(Lista1, Lista2, Interseccao),
    length(Interseccao, Tamanho).

% Transforma lista de [par(Score, Item), ...] em [Item, ...]
extrair_itens([], []).
extrair_itens([par(_, Item)|Resto], [Item|ItensResto]) :-
    extrair_itens(Resto, ItensResto).

% Pega os N primeiros elementos de uma lista
pegar_primeiros(_, [], []).
pegar_primeiros(0, _, []).
pegar_primeiros(N, [H|T], [H|R]) :-
    N > 0,
    N1 is N - 1,
    pegar_primeiros(N1, T, R).