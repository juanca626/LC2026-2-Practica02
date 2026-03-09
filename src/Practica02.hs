--Probado en la version 4.18.2.1 de cabal
module Practica02 where

--Sintaxis de la logica proposicional
data Prop = Var String | Cons Bool | Not Prop
            | And Prop Prop | Or Prop Prop
            | Impl Prop Prop | Syss Prop Prop
            deriving (Eq)

instance Show Prop where 
                    show (Cons True) = "⊤"
                    show (Cons False) = "⊥"
                    show (Var p) = p
                    show (Not p) = "¬" ++ show p
                    show (Or p q) = "(" ++ show p ++ " ∨ " ++ show q ++ ")"
                    show (And p q) = "(" ++ show p ++ " ∧ " ++ show q ++ ")"
                    show (Impl p q) = "(" ++ show p ++ " → " ++ show q ++ ")"
                    show (Syss p q) = "(" ++ show p ++ " ↔ " ++ show q ++ ")"


p, q, r, s, t, u :: Prop
p = Var "p"
q = Var "q"
r = Var "r"
s = Var "s"
t = Var "t"
u = Var "u"

type Estado = [String]

--EJERCICIOS

--Ejercicio 1
variables :: Prop -> [String]
variables f = eliminarDuplicados (vars f)
    where
        vars (Cons _) = []
        vars (Var p) = [p]
        vars (Not p) = vars p 
        vars (And p q) = vars p ++ vars q
        vars (Or p q) = vars p ++ vars q
        vars (Impl p q) = vars p ++ vars q
        vars (Syss p q) = vars p ++ vars q

--Ejercicio 2
interpretacion :: Prop -> Estado -> Bool
interpretacion (Cons b) _ = b
interpretacion (Var var) e = contiene var e
interpretacion (Not prop) e = not (interpretacion prop e)
interpretacion (And p1 p2) e = interpretacion p1 e && interpretacion p2 e
interpretacion (Or p1 p2) e = interpretacion p1 e || interpretacion p2 e
interpretacion (Impl p1 p2) e = not (interpretacion p1 e) || interpretacion p2 e
interpretacion (Syss p1 p2) e = interpretacion p1 e == interpretacion p2 e

--Ejercicio 3
estadosPosibles :: Prop -> [Estado]
estadosPosibles f = conjuntoPotencia (variables f)

--Ejercicio 4
modelos :: Prop -> [Estado]
modelos f = [e | e <- estadosPosibles f, interpretacion f e]

--Ejercicio 5
sonEquivalentes :: Prop -> Prop -> Bool
sonEquivalentes f1 f2 = 
    let variablesUnidas = eliminarDuplicados (variables f1 ++ variables f2)
        estados = conjuntoPotencia variablesUnidas
    in todos (\e -> interpretacion f1 e == interpretacion f2 e) estados

--Ejercicio 6 
tautologia :: Prop -> Bool
tautologia f = todos (\e -> interpretacion f e) (estadosPosibles f)

--Ejercicio 7
contradiccion :: Prop -> Bool
contradiccion f = not (alguno (\e -> interpretacion f e) (estadosPosibles f))

--Ejercicio 8
consecuenciaLogica :: [Prop] -> Prop -> Bool
consecuenciaLogica [] conclusion = tautologia conclusion
consecuenciaLogica premisas conclusion =
    tautologia (Impl (foldr1 And premisas) conclusion)


--Funciones auxiliar

--Checa si hay un elemento en la lista
contiene :: Eq a => a -> [a] -> Bool
contiene _ [] = False
contiene x (y:ys) = x == y || contiene x ys

--Pos elimina duplicados :v
eliminarDuplicados :: Eq a => [a] -> [a]
eliminarDuplicados [] = []
eliminarDuplicados (x:xs) = 
    if contiene x xs 
    then eliminarDuplicados xs 
    else x : eliminarDuplicados xs

--Ayuda a conseguir los posibles estados de las variables
conjuntoPotencia :: [a] -> [[a]]
conjuntoPotencia [] = [[]]
conjuntoPotencia (x:xs) = [(x:ys) | ys <- conjuntoPotencia xs] ++ conjuntoPotencia xs

--Verifica si los elementos de una lista cumplen con la proposicion
todos :: (a -> Bool) -> [a] -> Bool
todos _ [] = True
todos proposicion (x:xs) = proposicion x && todos proposicion xs

--Lo mismo que arriba pero solo con uno
alguno :: (a -> Bool) -> [a] -> Bool
alguno _ [] = False
alguno proposicion (x:xs) = proposicion x || alguno proposicion xs