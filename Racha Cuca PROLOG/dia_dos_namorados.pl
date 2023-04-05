%Implementando os fatos:

	vestido(amarelo).
	vestido(azul).
	vestido(branco).
	vestido(verde).
	vestido(vermelho).

	homem(antonio).
	homem(caio).
	homem(igor).
	homem(mauricio).
	homem(renan).

	mulher(ana).
	mulher(claudia).
	mulher(denise).
	mulher(laura).
	mulher(monica).

	conheceram(churrasco).
	conheceram(faculdade).
	conheceram(festa).
	conheceram(praia).
	conheceram(trabalho).

	duracao(1).
	duracao(2).
	duracao(3).
	duracao(4).
	duracao(5).

	presente(anel).
	presente(bolsa).
	presente(perfume).
	presente(sandalia).
	presente(vestido).



%funcoes que serao utilizadas na descricao das regras:

	%X está à ao lado de Y
	aoLado(X,Y,Lista) :- nextto(X,Y,Lista);nextto(Y,X,Lista).

	%X está EXATAMENTE a esquerda de Y
	xaEsquerda(X,Y,Lista) :- nextto(X,Y,Lista).
	                       
	%X está à esquerda de Y (em qualquer posição à esquerda)
	aEsquerda(X,Y,Lista) :- nth0(IndexX,Lista,X), 
	                        nth0(IndexY,Lista,Y), 
	                        IndexX < IndexY.

	%X está EXATAMENTE a direita de Y
	xaDireita(X,Y,Lista) :- nextto(Y,X,Lista).
	                        
	%X está à direita de Y (em qualquer posição à direita)
	aDireita(X,Y,Lista) :- aEsquerda(Y,X,Lista). 

	%X está no canto se ele é o primeiro ou o último da lista
	noCanto(X,Lista) :- last(Lista,X).
	noCanto(X,[X|_]).

	%Checa se todos os elementos sao diferentes.
	todosDiferentes([]).
	todosDiferentes([H|T]) :- not(member(H,T)), todosDiferentes(T).

	%X esta em algum lugar entre Y e Z, nessa ordem
	entre(X,Y,Z,Lista) :- aEsquerda(Y,X,Lista), aDireita(Z,X,Lista).

solucao(ListaSolucao) :- 

    ListaSolucao = [
        mesa(Vestido1, Homem1, Mulher1, Conheceram1, Duracao1, Presente1),
        mesa(Vestido2, Homem2, Mulher2, Conheceram2, Duracao2, Presente2),
        mesa(Vestido3, Homem3, Mulher3, Conheceram3, Duracao3, Presente3),
        mesa(Vestido4, Homem4, Mulher4, Conheceram4, Duracao4, Presente4),
        mesa(Vestido5, Homem5, Mulher5, Conheceram5, Duracao5, Presente5)
    ],

    %O casal que namora há um ano está ao lado da mulher que ganhou uma Bolsa.
    aoLado(mesa(_, _, _, _, 1, _), mesa(_, _, _, _, _, bolsa), ListaSolucao),

    %A mulher de Branco está exatamente à direita da de Amarelo.
    xaDireita(mesa(branco, _, _, _, _, _), mesa(amarelo, _, _, _, _, _), ListaSolucao),

    %Na primeira posição está a mulher que ganhou um Perfume.
    Presente1 = perfume,

    %O casal que se conheceu na Praia está em uma das pontas.
    noCanto(mesa(_, _, _, praia, _, _), ListaSolucao),

    %Mônica está em algum lugar à direita da mulher de Vermelho.
    aDireita(mesa(_, _, monica, _, _, _), mesa(vermelho, _, _, _, _, _), ListaSolucao),

    %A mulher do vestido Branco está exatamente à esquerda de Maurício.
    xaEsquerda(mesa(branco, _, _, _, _, _), mesa(_, mauricio, _, _, _, _), ListaSolucao),

    %A mulher que ganhou um Anel está na terceira posição.
    Presente3 = anel,

    %Laura está ao lado da mulher de Verde.
    aoLado(mesa(_, _, laura, _, _, _), mesa(verde, _, _, _, _, _), ListaSolucao),

    %Renan namora Denise.
    member(mesa(_, renan, denise, _, _, _), ListaSolucao),

    %Quem ganhou um Perfume está exatamente à esquerda do casal que se conheceu num Churrasco.
    xaEsquerda(mesa(_, _, _, _, _, perfume), mesa(_, _, _, churrasco, _, _), ListaSolucao),

    %A mulher que ganhou uma Bolsa está em algum lugar entre o Igor e a mulher de Vermelho, nessa ordem.
    entre(mesa(_, _, _, _, _, bolsa), mesa(_, igor, _, _, _, _), mesa(vermelho, _, _, _, _, _), ListaSolucao),

    %Antônio namora Mônica.
    member(mesa(_, antonio, monica, _, _, _), ListaSolucao),

    %Na quarta posição está o casal que namora há 3 anos.
    Duracao4 = 3,

    %O casal que namora há 2 anos está ao lado de Maurício.
    aoLado(mesa(_, _, _, _, 2, _), mesa(_, mauricio, _, _, _, _), ListaSolucao),

    %O casal do namoro mais longo é aquele que se conheceu numa Festa.
    member(mesa(_, _, _, festa, 5, _), ListaSolucao),

    %Renan está exatamente à esquerda do casal que namora há 4 anos.
    xaEsquerda(mesa(_, renan, _, _, _, _), mesa(_, _, _, _, 4, _), ListaSolucao),

    %Claudia está na segunda posição.
    Mulher2 = claudia,

    %A mulher que ganhou um Vestido está exatamente à esquerda do casal que se conheceu na Faculdade.
    xaEsquerda(mesa(_, _, _, _, _, vestido), mesa(_, _, _, faculdade, _, _), ListaSolucao),

    %Testa todas as possibilidades...
    vestido(Vestido1), vestido(Vestido2), vestido(Vestido3), vestido(Vestido4), vestido(Vestido5),
    todosDiferentes([Vestido1, Vestido2, Vestido3, Vestido4, Vestido5]),

    homem(Homem1), homem(Homem2), homem(Homem3), homem(Homem4), homem(Homem5),
    todosDiferentes([Homem1,Homem2, Homem3, Homem4, Homem5]),

    mulher(Mulher1), mulher(Mulher2), mulher(Mulher3), mulher(Mulher4), mulher(Mulher5),
    todosDiferentes([Mulher1, Mulher2, Mulher3, Mulher4, Mulher5]),

    conheceram(Conheceram1), conheceram(Conheceram2), conheceram(Conheceram3), conheceram(Conheceram4), conheceram(Conheceram5),
    todosDiferentes([Conheceram1, Conheceram2, Conheceram3, Conheceram4, Conheceram5]),

    duracao(Duracao1), duracao(Duracao2), duracao(Duracao3), duracao(Duracao4), duracao(Duracao5),
    todosDiferentes([Duracao1, Duracao2, Duracao3, Duracao4, Duracao5]),

    presente(Presente1), presente(Presente2), presente(Presente3), presente(Presente4), presente(Presente5),
    todosDiferentes([Presente1, Presente2, Presente3, Presente4, Presente5]).   