import Text.Printf
import System.IO
main = do 
    k'<- readFile "k.txt"
    dt<- readFile "entrada.txt"
    let aux = lines dt
    let aux1 = map (words) aux 
    let pontos = [map (read::String->Double) xs|xs<-aux1]
    let k = (read k' ::Int)
    let grupos = kGruposForm k pontos
    let saida = gerarSaida pontos k 0
    gerandoSaida saida
    gerandoSaida grupos 
    writeFile "result.txt" (printaSSE pontos k 0)
    



-- FUNÇÕES AUXILIARES PARA SALVAR A SAIDA -- 
printaSSE ::[[Double]] -> Int -> Int ->String
printaSSE pontos k lim = printf "%.4f" (_SSE pontos k lim)


geraList [] = []
geraList (x:xs) | x>0 = [x]++geraList xs
                |otherwise = geraList xs
geraList' [] = []
geraList' (xs:xss) = [geraList xs] ++ geraList' xss 

gerandoSaida [] = return ()
gerandoSaida (xs:xss) = do
                let saida1 = map (show) xs
                let saida'=map (++ ", ") saida1
                let saida = unwords saida'
                appendFile "saida.txt" (saida ++ "\n\n")
                gerandoSaida xss

--MERGE SORT PARA ORDENAR OS PONTOS-- 
mergeSort :: Ord a => [a] -> [a]
mergeSort [] = []
mergeSort [x]=[x]
mergeSort xss =  merge (mergeSort (take ((length xss) `div`2) xss)) (mergeSort (drop ((length xss) `div` 2) xss))

merge:: Ord a=> [a]->[a]->[a]
merge [] ys = ys
merge xs [] = xs
merge (xs:xss) (ys:yss) | xs<ys = [xs] ++ merge xss (ys:yss)
                    | otherwise = [ys] ++ merge (xs:xss) yss 

--PEGANDO O PONTO DE MENOR SOMA DAS CORDENADAS--
-- a ideia é ordenar crescentemente garantindo que o ponto de menor soma das cordenadas seja o primeiro -- 
menorSomaCord :: Ord a=> [a]-> a
menorSomaCord xss = head (mergeSort xss)

--DISTANCIA EUCLIDEANA--
listdistEuc :: [Double]->[Double]->[Double]
listdistEuc [] [] = []
listdistEuc (x:xs) (y:ys) = [(x-y)^2] ++ listdistEuc xs ys

distEuc :: [Double]->[Double]->Double
distEuc xs ys = sqrt (sum (listdistEuc xs ys))

-- ordeno a lista e pego o ultimo, esse me garante que é a maior distancia possível do ponto que quero comparar --
maiordistEuc :: [[Double]] -> [Double] -> Double
maiordistEuc xss ys = distEuc (last(mergeSort xss)) ys

-- Aqui vamos receber a lista ordenada de forma decrescente, 
-- sabemos que o ponto mais distante ao centroide será o ponto com as maiores coordenadas
-- Por que fazer isso? Quando encontrarmos um que já é diferente da maior distancia quer dizer que não tem mais nenhum
pontosMaisDist :: [[Double]]->Double->[Double]->[[Double]]
pontosMaisDist [] maiorD ys = []
pontosMaisDist (xs:xss) maiorD ys | maiorD == distEuc xs ys = [xs]++ pontosMaisDist xss maiorD ys
                                  | otherwise = []

-- pego os pontos mais distantes tendo em consideração que estou mandando em ordem decrescente
-- ordeno os pontos que recebi e retiro a cabeça, como ta em ordem crescente esse é com certeza o ponto com maior dist
-- e menores primeiras cord
pontoMaisDistMenorCord ::  [[Double]]->[Double]->[Double]                           
pontoMaisDistMenorCord xss ys = head(mergeSort(pontosMaisDist  (reverse (mergeSort xss)) (maiordistEuc xss ys) ys))
--Aqui estou achando nosso centroide
--centroide' é responsável por somar cordenadas por cordenadas
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


--MONTANDO OS K CENTROIDES--

--verifico se k é maior que 0, caso seja chamo a função novamente
-- com o novo valor de k = k-1 e mando a lista de pontos com menos o ponto mais distante do centroide 
-- e mando a lista de centroides atualizada
iniciarGrupos ::  (Ord t,Num t) => t->[[Double]]->[[Double]]->[[Double]]
iniciarGrupos k [] yss = yss
iniciarGrupos k xss yss | k > 0 = iniciarGrupos (k-1) (removeEle ptMDist xss ) (yss++[ptMDist])
                        | otherwise = yss
                        where 
                            ptMDist = pontoMaisDistMenorCord xss (centroide4 yss)
        
-- função geradora dos k centroides
--chamo a função auxiliar com k-2 pois os dois primeiros pontos ja estão sendo passados
--e mando a lista atualizada com menos os dois primeiros pontos
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

--CRIANDO OS GRUPOS KMEANS--

--Acha a menor distancia do ponto aos centroides--
minDistCent::[Double]->Double->[[Double]]->Double
minDistCent _ min [] = min
minDistCent xs min (ys:yss) | min > dist = minDistCent xs dist yss
                            | otherwise  = minDistCent xs min yss
                            where 
                                dist = distEuc xs ys

--mandar os centroides ordenados em ordem crescente--
--como os centroides tão em ordem crescente o primeiro que tiver a dist igual a minima é o centroide 
--de menores primeiras cordenadas
acharPtdeMenordist ::  [Double]->Double ->[[Double]]->[Double]
acharPtdeMenordist _ _ [] = []
acharPtdeMenordist xs min (ys:yss)| distEuc xs ys == min = ys 
                                  | otherwise = acharPtdeMenordist xs min yss

--Crio os grupos a partir dos centroides e tuplas
--onde as tuplas são (chave,conteudo)
--em que a chave é a que centroide o conteudo pertence
criandoOsGrupos :: Eq a=> [(a,a)]->[a]->[[a]]
criandoOsGrupos xss [] = []
criandoOsGrupos xss (cs:centroides) = [[cs]++[snd xs| xs<-xss, (fst xs) == cs]] ++ criandoOsGrupos xss centroides

--Aqui ocorre a atualização dos centroides--
newCentroides :: [[[Double]]]->[[Double]]
newCentroides grupos = [centroide4 xss | xss<-grupos ]

--Onde eu crio as tuplas de (chave,conteudo)
--pego o ponto, acho a menor distancia aos centroides
--identifico de qual centroide é, esse é a chave
--dai crio uma lista dessas tuplas
comparePtCent :: [[Double]]->[[Double]]->[([Double],[Double])]
comparePtCent [] _= []
comparePtCent (xs:xss) centroides = [(chave,xs)]++comparePtCent xss centroides
                                where 
                                    min = minDistCent xs (distEuc xs (head centroides)) (tail centroides)
                                    chave = acharPtdeMenordist xs min (mergeSort centroides)

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
--Ela pega o primeirou grupo, calcula os novos centroides e manda para a função sepOsGrupos
startKmeans :: (Ord t2,Ord t3,Num t2,Num t3)=>[[Double]]->t3->t2->[[[Double]]]
startKmeans xss k lim  = sepOsGrupos xss (lim+1) centroidesN grupos
                        where 
                            centroidesN = newCentroides grupos
                            grupos = criandoOsGrupos listDeTuplas centroides
                            listDeTuplas = comparePtCent xss centroides
                            centroides = kGruposForm k xss

--CALCULA O SSE--
_SSE :: (Ord t1,Ord t2,Num t1,Num t2)=> [[Double]]->t2->t1->Double
_SSE xss k lim = sum[(distEuc xs (centroide4 xss))^2|xss<-grupos,xs<-xss]
    where 
        grupos = startKmeans xss k lim

--gero tuplas com indices onde cada ponto é representado por um indice--
geraTuplasIndice :: Num t=> t-> [b] -> [(t,b)]
geraTuplasIndice  _ [] = []
geraTuplasIndice indice (xs:xss) = [(indice,xs)]++geraTuplasIndice (indice+1) xss

--retorna o indice que um ponto do grupo é representado pelos indices dos pontos iniciais--
retornaIndice :: (Num p, Eq t)=> t->[(p,t)]->p
retornaIndice xs []  = -1
retornaIndice xs (tp:tuplas)| xs == snd tp = fst tp
                            | otherwise = retornaIndice xs tuplas

removeTup id (tp:tps) | id == fst tp = tps
                      | otherwise = [tp]++removeTup id tps
--retorno uma lista de indices a partir de um conjunto de pontos, 
--verifico se o ponto ja foi indexado, pois é possível que o centroide final permaneça igual a um ponto inicial
gerarSaida'' :: (Num a1, Eq a2,Eq a1)=>[(a1,a2)]->[a2]->[a1]
gerarSaida'' tuplas [] = []
gerarSaida'' tuplas (gs:gss) = [id] ++ gerarSaida'' newTup gss
                            where 
                                id = retornaIndice gs tuplas 
                                newTup = removeTup id tuplas
--gera uma lista de lista dos indices--
gerarSaida' :: (Eq a2,Eq a1,Num a1)=>[a2]->[[a2]]->[[a1]]
gerarSaida' xss [] = []
gerarSaida' xss (gss:grupos) = [gerarSaida'' tuplas gss]++gerarSaida' xss grupos
                    where 
                        tuplas = geraTuplasIndice 1 xss

--retorna todo o processo feito por gerarSaida' e gerarSaida''--
gerarSaida :: (Ord a,Ord t2,Ord t3,Num t2,Num t3,Num a)=> [[Double]]->t3->t2->[[a]]
gerarSaida xss k lim = geraList' saida
                    where 
                        grupos = startKmeans xss k lim
                        saida = gerarSaida' xss grupos
