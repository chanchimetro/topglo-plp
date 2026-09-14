import TP1
import Test.HUnit

-- TESTS

testsInvertido :: Test
testsInvertido =
  TestList -- TODO: AGREGAR
    [ "Caja invertida (1)"
        ~: invertido cajaOn
        ~?= cajaOn,
      "Caja invertida (2)"
        ~: invertido cajaOff
        ~?= cajaOff,
      "Caja invertida (3)"
        ~: invertido cajaNada
        ~?= cajaNada,
      "Caja invertida (4)"
        ~: invertido
          ( Serie
              ( Paralelo
                  on
                  (Paralelo off cajaNada cajaOn on)
                  (Paralelo Nada cajaOn cajaOff Nada)
                  on
              )
              cajaOn
          )
        ~?= Serie
          cajaOn
          ( Paralelo
              on
              (Paralelo Nada cajaOff cajaOn Nada)
              (Paralelo on cajaOn cajaNada off)
              on
          )
    ]

testsHayCaminoIluminado :: Test
testsHayCaminoIluminado =
  TestList -- TODO: AGREGAR
    [ "En una caja con bombilla encendida hay camino iluminado"
        ~: hayCaminoIluminado cajaOn
        ~?= True,
      "Hay Camino Iluminado (2)"
        ~: hayCaminoIluminado
          ( Serie
              ( Paralelo
                  on
                  (Paralelo off cajaNada cajaOn on)
                  (Paralelo Nada cajaOn cajaOff Nada)
                  on
              )
              cajaOn
          )
        ~?= False,
      "Hay Camino Iluminado (3)"
        ~: hayCaminoIluminado
          ( Serie
              ( Paralelo
                  on
                  (Paralelo on cajaNada cajaOn on)
                  (Paralelo Nada cajaOn cajaOff Nada)
                  on
              )
              cajaOn
          )
        ~?= True
    ]

testsCantidadPrendidas :: Test
testsCantidadPrendidas =
  TestList -- TODO: AGREGAR
    [ "Cantidad prendidas en caja prendida es 1"
        ~: cantidadPrendidas cajaOn
        ~?= 1,
      "Cantidad Prendidas (2)"
        ~: cantidadPrendidas
          ( Serie
              ( Paralelo
                  on
                  (Paralelo off cajaNada cajaOn on)
                  (Paralelo Nada cajaOn cajaOff Nada)
                  on
              )
              cajaOn
          )
        ~?= 6
    ]

testsCajasDeCircuito :: Test
testsCajasDeCircuito =
  TestList -- TODO: AGREGAR
    [ "La lista de cajas de un circuito con una única caja es la lista con esa caja"
        ~: cajasDeCircuito cajaOn
        ~?= [on],
      "Cajas de Circuito (2)"
        ~: cajasDeCircuito
          ( Serie
              ( Paralelo
                  on
                  (Paralelo off cajaNada cajaOn on)
                  (Paralelo Nada cajaOn cajaOff Nada)
                  on
              )
              cajaOn
          )
        ~?= [on, off, Nada, on, on, Nada, on, off, Nada, on, on]
    ]

testsEsCircuitoProlijo :: Test
testsEsCircuitoProlijo =
  TestList -- TODO: AGREGAR
    [ "Una caja es prolija"
        ~: esCircuitoProlijo cajaOn
        ~?= True,
      "Es Circuito Prolijo (2)"
        ~: esCircuitoProlijo (Serie (Serie cajaOn cajaOn) cajaOff)
        ~?= True,
      "Es Circuito Prolijo (3)"
        ~: esCircuitoProlijo (Serie cajaOn (Serie cajaOn cajaOff))
        ~?= False
    ]

-- NOTA: para correr este test, cambiar la línea 18 del archivo tp1.hs de "show = showDeCircuito" a
-- "show = showDeCircuitoConEstructura".
-- De esa forma, podrán distinguir la estructura de los circuitos en serie.
testsCircuitoEmprolijado :: Test
testsCircuitoEmprolijado =
  TestList -- TODO: AGREGAR
    [ "La versión emprolijada de una caja es la misma caja"
        ~: circuitoEmprolijado cajaOn
        ~?= cajaOn,
      "Circuito Emprolijado (2)"
        ~: circuitoEmprolijado (Serie (Serie cajaOn cajaOn) cajaOff)
        ~?= Serie (Serie cajaOn cajaOn) cajaOff,
      "Circuito Emprolijado (3)"
        ~: circuitoEmprolijado (Serie cajaOn (Serie cajaOff cajaOn))
        ~?= Serie (Serie cajaOn cajaOff) cajaOn
    ]

testsTienenLaMismaEstructura :: Test
testsTienenLaMismaEstructura =
  TestList
    [ "Test Tienen La Misma Estructura (1)"
        ~: tienenLaMismaEstructura
          ( Serie
              cajaOn
              ( Paralelo
                  on
                  (Paralelo Nada cajaOff cajaOn Nada)
                  (Paralelo on cajaOn cajaNada off)
                  on
              )
          )
          ( Serie
              cajaOff
              ( Paralelo
                  on
                  (Paralelo off cajaOff cajaOn on)
                  (Paralelo Nada cajaOff cajaNada Nada)
                  off
              )
          )
        ~?= True,
      "Test Tienen La Misma Estructura (2)"
        ~: tienenLaMismaEstructura
          ( Serie
              cajaOn
              ( Paralelo
                  on
                  (Paralelo Nada cajaOff cajaOn Nada)
                  (Paralelo on cajaOn cajaNada off)
                  on
              )
          )
          ( Serie
              cajaOff
              ( Paralelo
                  on
                  (Serie cajaOff cajaOn)
                  (Paralelo Nada cajaOff cajaNada Nada)
                  off
              )
          )
        ~?= False
    ]

-- En particular aca cajaOn y cajaOff tienen la misma resistencia (1), por lo tanto, es indistinto usar uno u otro (por como implementamos "resistenciaCircuito")
testsSubCircuitoMásResistente :: Test
testsSubCircuitoMásResistente =
  TestList
    [ "Una caja simple es su propio subcircuito más resistente"
        ~: subCircuitoMásResistente cajaOn
        ~?= cajaOn,
      "En una serie, la serie completa es más resistente que las cajas individuales (1+1=2)"
        ~: subCircuitoMásResistente (Serie cajaOn cajaOn)
        ~?= Serie cajaOn cajaOn,
      "En un paralelo chico, el paralelo completo suma más (1 + 0.5 + 1 = 2.5 > 1)"
        ~: subCircuitoMásResistente (Paralelo cajaOn cajaOn cajaOn cajaOn)
        ~?= Paralelo cajaOn cajaOn cajaOn cajaOn,
      "Paralelo donde una rama es más resistente que el circuito total"
        ~: let ramaIzq = Serie (Serie cajaOn cajaOn) (Serie cajaOn (Serie cajaOn (Serie cajaOn cajaOn))) -- Resistencia 6
               ramaDer = Serie (Serie cajaOn cajaOn) (Serie cajaOn (Serie cajaOn cajaOn)) -- Resistencia 5
            in subCircuitoMásResistente (Paralelo cajaOn ramaIzq ramaDer cajaOn)
                 -- Resistencia Total = 1 + (6*5)/(6+5) + 1 = 1 + 2.7 + 1 = 4.7 aprox
                 -- Como 6 > 4.7, el subcircuito más resistente de todo el conjunto es la rama "ramaIzq"
                 ~?= ramaIzq
    ]

tests :: Test
tests =
  TestList
    [ TestLabel "invertido" testsInvertido,
      TestLabel "hayCaminoIluminado" testsHayCaminoIluminado,
      TestLabel "cantidadPrendidas" testsCantidadPrendidas,
      TestLabel "cajasDeCircuito" testsCajasDeCircuito,
      TestLabel "esCircuitoProlijo" testsEsCircuitoProlijo,
      TestLabel "circuitoEmprolijado" testsCircuitoEmprolijado,
      TestLabel "tienenLaMismaEstructura" testsTienenLaMismaEstructura,
      TestLabel "subCircuitoMásResistente" testsSubCircuitoMásResistente
    ]

main :: IO ()
main = runTestTT tests >>= print