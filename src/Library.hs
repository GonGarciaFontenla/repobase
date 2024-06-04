module Library where
import PdePreludat
import GHC.Num (Num)

doble :: Number -> Number
doble numero = numero + numero

{-
En el universo local de Harry Postre, para hacer postres se utilizan hechizos que se van usando sobre
los mismos para irlos preparando.
-}

--Modelar los postres. Un mismo postre puede tener muchos sabores, tiene un peso y se sirve a cierta temperatura.

data Postre = Postre {
    nombre :: String,
    sabores :: [String],
    peso :: Number,
    temperatura :: Number
}deriving Show

bizcochoBorracho :: Postre
bizcochoBorracho = Postre "Bizcocho borracho" ["fruta" , "crema"] 100 25

tarta :: Postre
tarta = Postre "Tarta de melaza" ["melaza"] 50 1

--Modelar los hechizos, sabiendo que deberían poderse agregar más sin modificar el código existente.
type Hechizo = Postre -> Postre

incendio :: Hechizo
incendio = modificarTemperatura 1 . perderPorcentajePeso 5

inmobulus :: Hechizo
inmobulus postre = modificarTemperatura (-(temperatura postre)) postre

wingardiumLeviosa :: Hechizo
wingardiumLeviosa = agregarSabor "concentrado" . perderPorcentajePeso 10
 
diffindo ::Number -> Hechizo
diffindo = perderPorcentajePeso

riddikulus :: String -> Hechizo
riddikulus sabor = agregarSabor (reverse sabor)

avadaKedvra :: Hechizo
avadaKedvra = inmobulus . quitarSabores 

modificarTemperatura :: Number -> Postre -> Postre
modificarTemperatura temp postre = postre {temperatura = temperatura postre + temp}

perderPorcentajePeso :: Number -> Postre -> Postre
perderPorcentajePeso porcentaje postre = postre {peso = peso postre - ((peso postre * porcentaje)/100)}

agregarSabor :: String -> Postre -> Postre
agregarSabor saborNuevo postre = postre {sabores = saborNuevo : sabores postre}

quitarSabores :: Postre -> Postre
quitarSabores postre = postre {sabores = []}

{-
Dado un conjunto de postres en la mesa, saber si hacerles un determinado hechizo los dejará 
listos (un postre está listo cuando pesa algo más que cero, tiene algún sabor y además no está congelado).
-}
--Forma Recursiva
hechizoDejaListo :: Hechizo -> [Postre] -> Bool
hechizoDejaListo hechizo [x] = estaListo (hechizo x)
hechizoDejaListo hechizo (x:xs) = estaListo (hechizo x) && hechizoDejaListo hechizo xs

--Forma no recursiva 
hechizoDejaListo' :: Hechizo -> [Postre] -> Bool
hechizoDejaListo' hechizo = all (estaListo . hechizo)

estaListo :: Postre -> Bool
estaListo postre = peso postre > 0 && tieneSabores postre && not (estaCongelado postre)

tieneSabores :: Postre -> Bool
tieneSabores = not . null .sabores
--tieneSabores = (>0) . length . sabores

estaCongelado :: Postre -> Bool
estaCongelado = (<=0) . temperatura 

--Agregar consigna
promedioPeso :: [Postre] -> Number
promedioPeso = promedio . obtenerPesos 

promedio :: [Number] -> Number
promedio pesos = (/length pesos) (sum pesos)

postresListos :: [Postre] -> [Postre] 
postresListos = filter estaListo

obtenerPesos :: [Postre] -> [Number]
obtenerPesos postres = map peso (postresListos postres)
