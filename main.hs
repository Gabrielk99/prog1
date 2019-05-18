import Text.Printf
import System.IO
import Data.List

main = do 
    k'<- readFile "k.txt"
    dt<- readFile "entrada.txt"
    let aux = lines dt
    let aux1 = map (words) aux 
    let pontos = [map (read::String->Double) xs|xs<-aux1]
    let k = (read k' ::Int)
    let grupos = startKmeans pontos k 0
    let indices = gerarSaida' pontos grupos
    let saida = gerandoSaida indices
    let sSE = _SSE grupos
    writeFile "saida.txt" saida 
    writeFile "result.txt" (printaSSE sSE)
    

----------------------- FUNÇÕES AUXILIARES PARA SALVAR A SAIDA -------------------------- 
--printaSSE retorna a soma SSE formatada para a precisão das 4 casas decimais--
printaSSE ::Double->String
printaSSE sSE = printf "%.4f" sSE

--transforma os indices que precisam ser gerados em uma string-- 
gerandoSaida :: Show a => [[a]] -> [Char]
gerandoSaida [] = []
gerandoSaida (xs:xss) = ( concat(intersperse ", " saida' ) ++"\n\n")++gerandoSaida xss
                    where 
                        saida' = map show xs
----------------------------------------------------------------------------------------

------------ FUNÇÕES AUXILIARES PARA ORDENAR COM O SORTBY ------------- 
ordena'::Ord a=> a->a->Ordering
ordena' ys xs |ys<xs = LT
              |otherwise = GT
ordena::(Foldable t,Num a,Ord a,Ord(t a))=> t a->t a->Ordering
ordena ys xs |sum ys < sum xs = LT
             |sum ys == sum xs = ordena' ys xs
             |otherwise = GT
ordenaD'::Ord a=> a->a->Ordering
ordenaD' ys xs |ys<xs = LT
               |otherwise = GT
ordenaD :: [Double]->[Double]->[Double]->Ordering
ordenaD ct ys xs|distEuc ys ct > distEuc xs ct = LT
                |distEuc ys ct == distEuc xs ct = ordenaD' ys xs
                |otherwise = GT
ordenaM' :: Ord a=>a->a->Ordering
ordenaM' ct1 ct2 | ct1<ct2=LT
                 | otherwise=GT
ordenaM :: [Double]->[Double]->[Double]->Ordering
ordenaM pt ct1 ct2|distEuc ct1 pt < distEuc ct2 pt = LT
                  |distEuc ct1 pt == distEuc ct2 pt = ordenaM' ct1 ct2
                  |otherwise = GT 
--------------------------------------------------------------------------

{---------------------------------------------------- COMEÇANDO O ALGORITIMO -----------------------------------------------------------------}

--Ordeno os pontos com sortBy utilizando como parametro ordena, onde ordena é uma função que compara as somas das coordenadas dos pontos--
--Crescentemente de acordo com a soma, dessa forma head xss retorna o ponto de menor soma das coordenadas--
menorSomaCord :: (Foldable t,Num a,Ord a, Ord (t a))=>[t a]->t a
menorSomaCord xss = head (sortBy ordena xss)

-------------------DISTANCIA EUCLIDEANA----------------------
listdistEuc :: [Double]->[Double]->[Double]
listdistEuc [] [] = []
listdistEuc (x:xs) (y:ys) = [(x-y)^2] ++ listdistEuc xs ys

distEuc :: [Double]->[Double]->Double
distEuc xs ys = sqrt (sum (listdistEuc xs ys))
-------------------------------------------------------------

-- Ordeno os pontos a partir da distancia de forma que os que tiverem maior distancia ao ponto ys fique no começo --
--quando os pontos possuem a mesma distancia ao ponto ys, avalio o que tem as menores coordenadas no começo -- 
maiordistEuc :: [[Double]] -> [Double] -> [Double]
maiordistEuc xss ys =  head(sortBy (ordenaD ys) xss)

---------------------------------------------------- CALCULO DO CENTROIDE-----------------------------------------------------------------
--Aqui estou achando nosso centroide
--centroide' é responsável por somar coordenadas por coordenadas
--centroide'' é por mandar sempre um ponto estatico e uma lista de pontos, dai 
--vamos sempre chamar centroide'' sendo xs trocado pelo centroide' de xs e ys
--centroide3 é a função que vai chamar centroide'' dizendo que o ponto estatico é a cabeça e a lista de pontos é a cauda
--centroide4 recebe a lista de pontos e retorna o centroide depois de todas as outras operações
centroide' :: [Double]->[Double]->[Double]
centroide' xs [] = []
centroide' [] ys = []
centroide' (x:xs) (y:ys) = [x+y]++centroide' xs ys

centroide'' :: [Double]->[[Double]] -> [Double]
centroide'' xs [] = xs
centroide'' xs (ys:yss) = centroide'' (centroide' xs ys) yss  

centroide3 :: [[Double]] -> [Double]
centroide3 xss = centroide'' (head xss) (tail xss)

centroide4 ::[[Double]] -> [Double]
centroide4 xss = [x/(fromIntegral tam)| x<-xs]
                where xs = centroide3 xss
                      tam = length xss

--------------------------------------------------------------------------------------------------------------------------------------------

---------------------------------------MONTANDO OS K CENTROIDES---------------------------------------------

--verifico se k é maior que 0, caso seja chamo a função novamente
-- com o novo valor de k = k-1 e mando a lista de pontos com menos o ponto mais distante do centroide 
-- e mando a lista de centroides atualizada
iniciarGrupos ::  (Ord t,Num t) => t->[[Double]]->[[Double]]->[[Double]]
iniciarGrupos k [] yss = yss
iniciarGrupos k xss yss | k > 0 = iniciarGrupos (k-1) (removeEle ptMDist xss ) (yss++[ptMDist])
                        | otherwise = yss
                        where 
                            ptMDist = maiordistEuc xss (centroide4 yss)
        
-- função geradora dos k centroides
--chamo a função auxiliar com k-1 pois o  primeiro ponto ja está sendo passado
--e mando a lista atualizada com menos um ponto
-- K tem que ser necessáriamente maior que zero, se não retorna lista vazia
kGruposForm :: (Ord t, Num t) => t->[[Double]] -> [[Double]]
kGruposForm 0 _ = []
kGruposForm k xss = iniciarGrupos (k-1) (removeEle (menorSomaCord xss) xss) yss
                  where 
                  yss = [menorSomaCord xss]

-- Remove o ponto mais distante do centroide da lista de pontos que estamos trabalhando --
removeEle :: Eq t => t->[t]->[t]
removeEle ys [] = [] 
removeEle ys (xs:xss) | ys == xs = xss
                      | otherwise = [xs]++removeEle ys xss
-------------------------------------------------------------------------------------------------------------


---------------------------------------------CRIANDO OS GRUPOS KMEANS--------------------------------------------------------

--Crio os grupos a partir dos centroides e tuplas
--onde as tuplas são (chave,conteudo)
--em que a chave é a que centroide o conteudo pertence
criandoOsGrupos :: Eq a=> [(a,a)]->[a]->[[a]]
criandoOsGrupos tuplas [] = []
criandoOsGrupos tuplas (cs:centroides) = [[cs]++[snd tp| tp<-tuplas, (fst tp) == cs]] ++ criandoOsGrupos tuplas centroides

--Aqui ocorre a atualização dos centroides--
newCentroides :: [[[Double]]]->[[Double]]
newCentroides grupos = [centroide4 xss | xss<-grupos ]

--Onde eu crio as tuplas de (chave,conteudo)
--pego o ponto, acho a menor distancia aos centroides
--identifico de qual centroide é, esse é a chave
--dai crio uma lista dessas tuplas
comparePtCent :: [[Double]]->[[Double]]->[([Double],[Double])]
comparePtCent [] _= []
comparePtCent (pt:pontos) centroides = [(chave,pt)]++comparePtCent pontos centroides
                                where 
                                    chave = head(sortBy (ordenaM pt) centroides)

--Aqui é onde a magica acontece, as operações anteriores de criar tuplas, separar os grupos e recalcular centroides--
--como estabelecido, comparamos se houve mudanças no grupo e se o lim é menor igual a 100
sepOsGrupos :: (Ord t1,Num t1)=>[[Double]]->t1->[[Double]]->[[[Double]]]->[[[Double]]]
sepOsGrupos xss lim centroidesAtt gruposAnt | lim <=100 && grupos /= gruposAnt = sepOsGrupos xss (lim+1) centroidesN grupos
                                            | otherwise = grupos

                                            where
                                        centroidesN = newCentroides grupos
                                        grupos = criandoOsGrupos listDeTuplas centroidesAtt
                                        listDeTuplas = comparePtCent xss centroidesAtt

--Função para chamar a sepOsGrupos, auxiliar para ter um primeiro grupo de partida para poder comparar
--Ela pega o primeiro grupo, calcula os novos centroides e manda para a função sepOsGrupos
startKmeans :: (Ord t2,Ord t3,Num t2,Num t3)=>[[Double]]->t3->t2->[[[Double]]]
startKmeans xss k lim  = sepOsGrupos xss (lim+1) centroidesN grupos
                        where 
                            centroidesN = newCentroides grupos
                            grupos = criandoOsGrupos listDeTuplas centroides
                            listDeTuplas = comparePtCent xss centroides
                            centroides = kGruposForm k xss

---------------------------------------------------------------------------------------------------------------------------------

---------------------------------------------------CALCULA O SSE----------------------------------------------------------
_SSE ::[[[Double]]]->Double
_SSE grupos = sum[(distEuc xs (centroide4 xss))^2|xss<-grupos,xs<-xss]
--------------------------------------------------------------------------------------------------------------------------


--------------------------------------------GERANDO LISTA DE INDICES---------------------------------------------------------

--retorna o indice que um ponto do grupo é representado pelos indices dos pontos iniciais--
retornaIndice :: (Eq t1,Num t2)=>t1->[t1]->t2->t2
retornaIndice xs (ys:yss) id| xs == ys = id
                            | otherwise = retornaIndice xs yss (id+1)
                           

--verifica o ponto que ja foi indexado e o configura como uma lista vazia para que na proxima chamada
--esse mesmo ponto não seja localizado nesse indixe ja utilizado
atualizaPts :: Eq a => [[a]]->[a]->[[a]]   
atualizaPts [] [] = []
atualizaPts xss [] = xss
atualizaPts (xs:xss) gs | xs == gs = [[]] ++ xss
                        | otherwise = [xs]++atualizaPts xss gs

--retorno uma lista de indices a partir de um conjunto de pontos, 
--Atualizo os pontos antes de gerar um novo indice, pois pode haver pontos repetidos em indices diferentes--
gerarSaida'' :: (Eq a1,Num a2)=>[[a1]]->[[a1]]->[a2]
gerarSaida'' xss [] = []
gerarSaida'' xss (gs:gss) =  [id] ++ gerarSaida'' pontosAtt gss
                            where 
                                id = retornaIndice gs xss 1
                                pontosAtt = atualizaPts xss gs

--gera uma lista de lista dos indices--
gerarSaida' :: (Eq a1,Num a2)=>[[a1]]->[[[a1]]]->[[a2]]
gerarSaida' xss [] = []
gerarSaida' xss (gss:grupos) = [gerarSaida'' xss (tail gss)]++gerarSaida' xss grupos

--------------------------------------------------------------------------------------------------------------------------
