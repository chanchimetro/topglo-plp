module TP1 where

data Caja = Bombilla Bool | Nada
              deriving Eq
instance Show Caja where
    show = showDeCaja

showDeCaja :: Caja -> String 
showDeCaja (Bombilla True) = "💡"
showDeCaja (Bombilla False) = "⚪️"
showDeCaja (Nada) = "🛑"

data Circuito = Caja     Caja
              | Serie    Circuito Circuito
              | Paralelo Caja Circuito Circuito Caja
                  deriving Eq
instance Show Circuito where
    show = showDeCircuito

showDeCircuito :: Circuito -> String
showDeCircuito (Caja caja) = showDeCaja caja
showDeCircuito (Serie circuitoInicial circuitoFinal) =
  (showDeCircuito circuitoInicial) ++ "-" ++ (showDeCircuito circuitoFinal)
showDeCircuito (Paralelo cajaEntrada circuitoIzquierdo circuitoDerecho cajaSalida) =
  (showDeCaja cajaEntrada) ++
  "{" ++ (showDeCircuito circuitoIzquierdo) ++ "}" ++
  "{" ++ (showDeCircuito circuitoDerecho) ++ "}" ++
  (showDeCaja cajaSalida)

showDeCircuitoConEstructura :: Circuito -> String
showDeCircuitoConEstructura (Caja caja) = showDeCaja caja
showDeCircuitoConEstructura (Serie circuitoInicial circuitoFinal) = "(" ++
  (showDeCircuitoConEstructura circuitoInicial) ++
    "-" ++
  (showDeCircuitoConEstructura circuitoFinal) ++ ")"
showDeCircuitoConEstructura (Paralelo cajaEntrada circuitoIzquierdo circuitoDerecho cajaSalida) =
  (showDeCaja cajaEntrada) ++
  "{" ++ (showDeCircuitoConEstructura circuitoIzquierdo) ++ "}" ++
  "{" ++ (showDeCircuitoConEstructura circuitoDerecho) ++ "}" ++
  (showDeCaja cajaSalida)

on  = Bombilla True
off = Bombilla False

cajaOn   = Caja on
cajaOff  = Caja off
cajaNada = Caja Nada

-- 1: recCircuito

recCircuito:: (Caja -> a) -> (Circuito -> Circuito -> a -> a -> a) -> (Caja -> Circuito -> Circuito -> Caja -> a -> a -> a -> a -> a) -> Circuito -> a
recCircuito fCaja fSerie fParalelo circuito = case circuito of
  (Caja caja) -> fCaja caja
  (Serie circ1 circ2) -> fSerie circ1 circ2 (rec circ1) (rec circ2)
  (Paralelo caja1 circ1 circ2 caja2) -> fParalelo caja1 circ1 circ2 caja2 (fCaja caja1) (rec circ1) (rec circ2) (fCaja caja2)
  where
    rec = recCircuito fCaja fSerie fParalelo

-- 2: foldCircuito

foldCircuito:: (Caja -> a) -> (a -> a -> a) -> (a -> a -> a -> a -> a) -> Circuito -> a
foldCircuito fCaja fSerie fParalelo = recCircuito
  fCaja
  (\_ _ recCirc1 recCirc2 -> fSerie recCirc1 recCirc2)
  (\_ _ _ _ recCaja1 recCirc1 recCirc2 recCaja2 -> fParalelo recCaja1 recCirc1 recCirc2 recCaja2)

-- 3 invertido

invertido:: Circuito -> Circuito
invertido = foldCircuito
  Caja
  (flip Serie)
  (\(Caja caja1) circ1 circ2 (Caja caja2) -> Paralelo caja2 circ2 circ1 caja1)

-- 4: hayCaminoIluminado

hayCaminoIluminado:: Circuito -> Bool
hayCaminoIluminado = foldCircuito
  (==on)
  (&&)
  (\caja1 circ1 circ2 caja2 -> caja1&&(circ1||circ2)&&caja2)

-- 5: cantidadPrendidas

cantidadPrendidas:: Circuito -> Int
cantidadPrendidas = foldCircuito
  (\caja -> if caja==on then 1 else 0)
  (+)
  (\caja1 circ1 circ2 caja2 -> caja1+circ1+circ2+caja2)

-- 6: cajasDeCircuito

cajasDeCircuito:: Circuito -> [Caja]
cajasDeCircuito = foldCircuito
  (: [])
  (++)
  (\caja1 circ1 circ2 caja2 -> caja1++circ1++circ2++caja2)

-- 7: esCircuitoProlijo

esCircuitoProlijo:: Circuito -> Bool
esCircuitoProlijo = recCircuito
  (const True)
  (\_ circ2 recCirc1 recCirc2 -> case circ2 of
    (Caja _) -> recCirc1
    (Serie _ _) -> False
    (Paralelo _ _ _ _) -> recCirc1&&recCirc2
  )
  (\_ _ _ _ _ _ _ _ -> True)

-- 8: circuitoEmprolijado

circuitoEmprolijado:: Circuito -> Circuito
circuitoEmprolijado = foldCircuito
  Caja
  (\recCirc1 recCirc2 -> case recCirc2 of
    Caja caja -> Serie recCirc1 recCirc2
    Serie c1 c2 -> Serie (Serie recCirc1 c1) c2
    Paralelo _ _ _ _ -> Serie recCirc1 recCirc2
  )
  (\(Caja recCaja1) recCirc1 recCirc2 (Caja recCaja2) -> Paralelo recCaja1 recCirc1 recCirc2 recCaja2)

-- 9: tienenLaMismaEstructura 

tienenLaMismaEstructura:: Circuito -> Circuito -> Bool
tienenLaMismaEstructura = foldCircuito
  (\cajaCirc1 circ -> case circ of
    Caja _ -> True
    Serie _ _ -> False
    Paralelo _ _ _ _ -> False
  )
  (\recCircuito1 recCircuito2 circ -> case circ of
    Caja _ -> False
    Serie c1 c2 -> recCircuito1 c1 && recCircuito2 c2
    Paralelo _ _ _ _ -> False
  )
  (\_ recCirc1 recCirc2 _ circ -> case circ of 
    Caja _ -> False
    Serie _ _ -> False
    Paralelo _ c1 c2 _ -> recCirc1 c1 && recCirc2 c2
  )

-- 10: subCircuitoMásResistente

resistenciaCircuito:: Circuito -> Float
resistenciaCircuito circ = 1

subCircuitoMásResistente:: Circuito -> Circuito
subCircuitoMásResistente = recCircuito
  Caja
  --(\caja -> if resistenciaCircuito (Caja caja) > (resistenciaCircuito cajaNada) then Caja caja else cajaNada)
  (\circ1 circ2 recCirc1 recCirc2 -> if resistenciaCircuito recCirc1 > resistenciaCircuito recCirc2
    then if resistenciaCircuito (Serie circ1 circ2) > resistenciaCircuito recCirc1
      then Serie circ1 circ2
      else recCirc1
    else if resistenciaCircuito (Serie circ1 circ2) > resistenciaCircuito recCirc2
      then Serie circ1 circ2
      else recCirc2
  )
  (\caja1 circ1 circ2 caja2 _ recCirc1 recCirc2 _ -> if resistenciaCircuito recCirc1 > resistenciaCircuito recCirc2
    then if resistenciaCircuito (Paralelo caja1 circ1 circ2 caja2) > resistenciaCircuito recCirc1
      then Paralelo caja1 circ1 circ2 caja2
      else recCirc1
    else if resistenciaCircuito (Paralelo caja1 circ1 circ2 caja2) > resistenciaCircuito recCirc2
      then Paralelo caja1 circ1 circ2 caja2
      else recCirc2
  )

{-- 11: Demostrar: alternado . alternado = id

alternado :: Circuito -> Circuito
{AC} alternado (Caja caja) = Caja (cajaAlternada caja)
{AS} alternado (Serie ci cf) = Serie (alternado ci) (alternado cf)
{AP} alternado (Paralelo ce ci cd cs) =
       Paralelo (cajaAlternada ce) (alternado ci) (alternado cd) (cajaAlternada cs)

cajaAlternada :: Caja -> Caja
{CAN} cajaAlternada Nada = Nada
{CAB} cajaAlternada Bombilla booleano = Bombilla not booleano

(.) :: (b -> c) -> (a -> b) -> a -> c
{C} (f . f) x = f (f x)

id :: a -> a
{I} id x = x

not :: Bool -> Bool
{NT} not True = False
{NF} not False = True

-- TODO: COMPLETAR

--}