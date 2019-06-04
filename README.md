# Trabalho CP 2017-2018

Este projeto diz respeito á utilização de catamorfismos, anamorfismos e hilomorfismos.

Dentro encontram-se as seguintes definições:


Definição de uma Blockchain e de catamorfismos, anamorfismos e hilomorfismos para Blockchains.

Definição de uma QuadTree e de catamorfismos, anamorfismos e hilomorfismos para QuadTrees.

Definição de uma FTree e de catamorfismos, anamorfismos e hilomorfismos para FTrees.


//---------------------------------------------------------------------------------------------------------//


Os exercicios seguintes encontram-se resolvidos da seguinte forma:

Exercicio 1(BlockChain):

a) Catamorfismo aplicado a blockchain para criar uma lista com todas as transações da blockchain fornecida.

b) Catamorfismo aplicado ás transações de uma blockchain para calcular o saldo dos individuos nesta.

c) Catamorfismo aplicado a uma blockchain para verificar a existencia de elementos repetidos(transações repetidas).


//---------------------------------------------------------------------------------------------------------//


Exercicio 2(QuadTree):

a) Troca a ordem dos ramos de uma QTree.

b) Catamorfismo aplicado a uma QTree recebida para a rodar 90 graus para a direita.

c) Catamorfismo aplicada a uma QTree para aumentar ou diminuir o tamanho desta em função de um inteiro recebido, alterando o tamanho definido pelas celulas.

d) Fmap aplicado a uma QTree para inverter as cores desta subtraindo o valor das componentes da cor a 255.

e) Anamorfismo aplicado a uma QTree para comprimir todos os nodos que estejam a niveis inferiores do novo nivel máximo da QTree pedido.

f) Catamorfismo aplicado a uma QTree para contornar uma malha poligonal contida na imagem fornecida, tirando partido de uma função que deteta o fundo da imagem.


//---------------------------------------------------------------------------------------------------------//


Exercicio 3(Demonstração teórica):

a) Utilização da lei da recursividade múltipla para <f k, l k> e para <g,s> e, combinando os resultados com a lei da banana-split, derivação das funções base k e loop que são usadas como auxiliares.


//---------------------------------------------------------------------------------------------------------//


Exercicio 4(FTree):

a) Criação de uma árvore de Pitágoras através de um anamorfismo cuja profundidade é passada como argumento pelo utilizador começando o primeiro quadrado com tamanho 100. Os quadrados no nivel seguinte seguem a formula de definição de lado {tamanho do nivel atual/raiz quadrada do lado atual do quadrado}.

b) Utilização de um catamorfismo para desenhar uma FTree, neste caso desenha a árvore e Pitágoras(PTree) gerado em cima.


//---------------------------------------------------------------------------------------------------------//


Exercicio 5(Bags):

a) map aplicado a um Bag para calcular a probabilidade de cada cor, num dado Bag.


//---------------------------------------------------------------------------------------------------------//


Nota: Para as funções definidas em baixo consultar Cp.hs, BMP.hs, List.hs, ListUtils.hs, Nat.hs e Show.hs para definições auxiliares.

Definições auxiliares para utilização em Blockchains:

inBlockchain = either Bc Bcs

outBlockchain (Bc a) = i1 a

outBlockchain (Bcs (a,b))= i2 (a,b)

recBlockchain f = id -|- id >< f  



definições auxiliares para utilização em QuadTrees:

myUncurry0 f (a,(b,(c,d))) = f a b c d

myUncurry1 g (e,(r,s)) = g e r s 

inQTree = either (myUncurry1 Cell) (myUncurry0 Block) 

outQTree (Cell a b c) = i1 (a,(b,c))
outQTree (Block t1 t2 t3 t4) = i2 (t1,(t2,(t3,t4)))

baseQTree f g = (f >< id) -|- (g >< (g >< (g >< g)))

recQTree g = baseQTree id g

fmap f = cataQTree(inQTree . (baseQTree f id))



definições auxiliares para utilização em FTrees:

myUncurry f (a,(b,c)) = f a b c

inFTree = either (Unit) (myUncurry Comp)  

outFTree (Unit b) = i1 b
outFTree (Comp a t1 t2) = i2 (a,(t1,t2)) 

baseFTree f g h = g -|- (f >< (h >< h))

recFTree g = baseFTree id id g

bimap f g = cataFTree(inFTree . (baseFTree f g id))



definições auxiliares para utilização em Bags:


singletonbag a = B [(a, 1)]

fmapf =B·map (f ×id)·unB

