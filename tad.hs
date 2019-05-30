import Data.List
type Ponto = (Float,Float)

origin :: Ponto

origin = (0,0)

esquerda :: Ponto-> Ponto

esquerda (x,y) = (x-1,y)

type Pair a = (a,a)

mult :: Pair Int -> Int

mult (m,n) = m*n

distancia :: Ponto ->Ponto-> Float 

distancia (m,n) (x,y) = sqrt ((m-x)^2 + (n-y)^2)

type Triplo a = (a,a,a)

areaP :: Triplo Float -> Float
areaP (a,b,c) = a*b*c

data Resposta = Sim | Nao | CAGUEIPORA deriving (Eq,Show)
chaveia :: Resposta->Resposta
chaveia Sim = Nao
chaveia Nao = Sim
chaveia _ = CAGUEIPORA

respostas::[Resposta]
respostas = [Sim,Nao,CAGUEIPORA]

comparaR :: Resposta->Resposta->String
comparaR x k | x==k = "Igual"
             | otherwise = "NOTIGUAL"

data Figura = Circle Ponto Float| Retangle Ponto Ponto | Triangulo Ponto Ponto Ponto deriving (Show)


area :: Figura -> Float

area (Circle (x,y) r)  = pi*r^2 
area (Retangle (x,y) (x1,y1)) = abs((y1-y))*abs((x1-x))
area (Triangulo (x,y) (x1,y1) (x2,y2)) = ((x*y1 + y*x2 + x1*y2)-(y1*x2+x*y2+y*x1))/2 

mover:: Figura -> Ponto -> Figura

mover (Circle (x,y) r) (x1,y1) = Circle (x1+x,y1+y) r

mover (Retangle (x,y) (x1,y1)) (c1,c2) = Retangle (x+c1,y+c2) (x1+c1,y1+c2)

data Talvez a = Nada | Apenas a deriving (Eq,Show)

cabeça :: [a] -> Talvez a
cabeça [] = Nada
cabeça (x:xs) = Apenas x


div1 :: Integral a=> a->a ->Talvez a
div1 a 0 = Nada
div1 a b = Apenas (a`div`b)

main = do
    content<- readFile "entrada.txt"
    let x = map words (lines content)
    let y = transform x []
    let z = figure y
    let ord = sortBy (ordena) z
    let sd = saida (disFigura ord)
    writeFile "saida.txt" sd

transform [] listAtt = listAtt
transform (xs:xss) listAtt = transform xss (listAtt++[((head xs),convert)])
                        where
                            convert = map (read::String->Float) (tail xs)

figure [] = []
figure (tp:tps) | fst tp == "Circulo" = [Circle (pontos!! 0, pontos !! 1) (pontos!!2)]++figure tps
                | fst tp == "Retangulo" = [Retangle (pontos!!0,pontos!!1) (pontos!!2,pontos!!3)]++figure tps
                | fst tp == "Triangulo"= [Triangulo (pontos!!0,pontos!!1) (pontos!!2,pontos!! 3) (pontos!!4,pontos!!5)]++figure tps
                |otherwise =figure tps
                where 
                    pontos = snd tp

ordena  xs ys | area xs <= area ys = LT
              |otherwise = GT

saida [] = []
saida (xs:xss)=(concat (intersperse " " xs)++"\n")++saida xss

disFigura' (Circle (x,y) r ) = [["Circulo",(show x),(show y),(show r)]]
disFigura' (Retangle (x,y) (x1,y1)) = [["Retangulo",(show x),(show y),(show x1),(show y1)]]
disFigura' (Triangulo (x,y) (x1,y1) (x2,y2)) = [["Triangulo",(show x),(show y),(show x1),(show y1),(show x2),(show y2)]]
disFigura []=[]
disFigura (x:xs) = disFigura' x ++ disFigura xs




 ------------------------------------------------------------------               
data Nat = Zero | Succ Nat deriving (Show)

 
convert1 (Zero) n = n
convert1 (Succ x) n = convert1 x (n+1)
convert a = convert1 a 0
convert' 0 = Zero
convert' n = Succ (convert' (n-1))  
somarB a b = convert' ( convert a+convert b)


somarE (Zero) (Zero) = Zero
somarE (Succ x) (Zero) = Succ (somarE x (Zero))
somarE (Zero) (Succ x) = Succ (somarE (Zero) x)
somarE (Succ x) (Succ v) = Succ(Succ (somarE x v))

mult1 Zero n =  n
mult1 (Succ m) n = somarE n (mult1 (Succ m) n)

data Expr = Val Int | Add Expr Expr | Mul Expr Expr deriving (Show)

instance Eq Expr where
    (Val a)== (Val b) = a == b 
    (Add a b) == (Add c d) = (( c ==  a|| c == b)&&( d== b || d== a)) 
    (Mul a b) == (Mul c d) = ((c== a|| c == b)&&( d== b || d== a))
-------------------------------------------
avalia (Val a) = a
avalia (Add a b) = (avalia a)+(avalia b)
avalia (Mul a b) = (avalia a)*(avalia b)
-------------------------------------------
tamanho (Val a) = 1
tamanho (Mul b a) = 1 + tamanho b + tamanho a
tamanho (Add b a)= 1+tamanho a + tamanho b


data Arv a = Null | No (Arv a) a (Arv a) deriving Show

pertence a Null = False
pertence a (No esq b dir) | a == b = True
                           |otherwise = pertence a esq || pertence a dir

arvore = (No (No (No Null 2 Null) 4 (No Null 5 Null))  8 (No (No Null 10 Null) 12 (No Null 16 Null)))

vazia Null = True
vazia _ = False

pertence' a Null = Nada
pertence' a (No esq b dir)  | a == b = Apenas a
                            |pertence' a esq /= Nada = Apenas a
                            |otherwise = pertence' a dir

pertenceB x Null = False
pertenceB x (No esq b dir) | x==b = True
                           |x>b = pertenceB x dir
                           |otherwise = pertenceB x esq
                        
pertenceB' x (No esq b dir) | pertenceB x (No esq b dir) = Apenas x
                            |otherwise = Nada