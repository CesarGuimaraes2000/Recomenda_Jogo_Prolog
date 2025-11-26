:- use_module(library(lists)).
:- use_module(library(random)).

:- dynamic possui/2.

:- ensure_loaded('jogos_populares.pl').
:- exists_file('biblioteca.pl') -> ensure_loaded('biblioteca.pl') ; true.

jogo_disponivel(ID, Nome, Preco, Modo, Tags) :-
    jogo(ID, Nome, Preco, Modo, Tags),
    \+ possui(ID, _).


jogo_valido(ID, Nome, Preco, Modo, Tags, CatsDesejadas, PrecoMin, PrecoMax) :-
    jogo_disponivel(ID, Nome, Preco, Modo, Tags),
    
    % Filtro de Preço
    Preco >= PrecoMin,
    Preco =< PrecoMax,
    
    % Filtro de Categorias (Rigoroso: deve ter TODAS)
    contem_todas_categorias(CatsDesejadas, Tags).

recomendar_top_5(CatsDesejadas, PrecoMin, PrecoMax, Top5Jogos) :-
    % 1. Encontra TODOS os jogos que satisfazem a regra 'jogo_valido'
    % O resultado é uma lista de termos: item(Nome, Preco, Modo, Tags)
    findall(
        item(Nome, Preco, Modo, Tags),
        jogo_valido(_, Nome, Preco, Modo, Tags, CatsDesejadas, PrecoMin, PrecoMax),
        TodosJogos
    ),
    
   
    list_to_set(TodosJogos, JogosUnicos),
    
    
    random_permutation(JogosUnicos, JogosEmbaralhados),
    
    
    pegar_primeiros(5, JogosEmbaralhados, Top5Jogos).


contem_todas_categorias([], _).
contem_todas_categorias([Cat|Resto], TagsJogo) :-
    member(Cat, TagsJogo),
    contem_todas_categorias(Resto, TagsJogo).


pegar_primeiros(_, [], []).
pegar_primeiros(0, _, []).
pegar_primeiros(N, [H|T], [H|R]) :-
    N > 0,
    N1 is N - 1,
    pegar_primeiros(N1, T, R).