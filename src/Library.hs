{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use foldr" #-}
module Library where
import PdePreludat
import System.IO.Error (permissionErrorType)

doble :: Number -> Number
doble numero = numero + numero

-- -- 1ra Opcion de Modelización 
-- 1. a.  Tenemos personas, de las que se conocen sus habilidades (strings) y si son buenas o 
-- no. Dar un sinónimo de tipo y una constante de ejemplo

type Persona = ([Habilidad],TipoDePersona)

type TipoDePersona = Bool
type Habilidad = String

data Color = Amarillo | Azul | Verde | Rojo | Negro | Rosa deriving(Show ,Eq)

type NivelDePelea = Number

-- 1. b.  Tenemos tambien Power Rangers, de los que conocemos su color, sus habilidades y 
-- su nivel de pelea. Nuevamente, dar un sinónimo de tipo y una constante de ejemplo.

type PowerRanger = (Color, [Habilidad] , NivelDePelea)

habilidadesPersona :: Persona -> [Habilidad]
habilidadesPersona (listaDeHabilidades, _ ) = listaDeHabilidades

esBuenaPersona :: Persona -> TipoDePersona
esBuenaPersona (_ , bondad ) = bondad

-- 2. convertirEnPowerRanger: dado un color y una persona, convierte a la persona en un 
-- ranger de ese color.  
-- Al hacer esto, le traspasa todas sus habilidades originales, potenciadas (por ejemplo: si su 
-- habilidad era “bailar”, ahora es ”superBailar” ), y le da tanto nivel de pelea como la cantidad 2
-- de letras de todas sus habilidades originales.
-- darNivelDePelea :: Persona -> Number

potenciarHabilidades :: Persona -> [Habilidad]

potenciarHabilidades' persona =  map ("Super"++) $habilidadesPersona persona
potenciarHabilidades  =  map ("Super"++) . habilidadesPersona

contarLetras :: [Char] -> Number
contarLetras [] = 0
contarLetras (x:xs) = 1 + contarLetras xs

darNivelDePelea  :: Persona -> Number
darNivelDePelea =  sum . map contarLetras . habilidadesPersona

tomy :: Persona
tomy = (["Galan","Poderoso"], True )

jason :: Persona
jason = (["Travieso","Debil"], True )

kimberly :: Persona
kimberly  =  (["Carisma","Solidaridad"], True )

skull :: Persona
skull =  (["Violento","Lider"], False )

bulk ::  Persona
bulk =  (["BarraBrava","Ladron"], False )


convertirEnPowerRanger :: Color -> Persona -> PowerRanger
convertirEnPowerRanger color persona = (color, potenciarHabilidades persona , darNivelDePelea persona )


-- 3. formarEquipoRanger: dada una lista de colores y una lista de personas, genera un 
-- equipo (una lista de Power Rangers) de los colores dados.
coloresLista = [Rojo,Rosa,Azul]
personasLista :: [Persona]
personasLista = [jason,skull,kimberly,bulk]

-- Funcion ZIP?
-- formarEquipoRanger :: [Color]->[Persona]->[Persona]
-- formarEquipoRanger colores personas = zip.convertirEnPowerRanger colores .  filter (esBuenaPersona ) personas

-- 4. a. findOrElse: Dada una condición, un valor y una lista, devuelve el primer elemento 
-- que cumpla la condición, o el valor dado si ninguno la cumple.

findOrElse :: (a -> Bool) -> a -> [a] -> a
findOrElse condicion valor lista
        | any condicion lista = head $ filter condicion lista
        | otherwise = valor

-- 4. b. rangerLider: Dado un equipo de rangers, devuelve aquel que debe liderar el equipo: 
-- este es siempre el ranger rojo, o en su defecto, la cabeza del equipo. 

listaDeRangers = [(Verde,["Patadura","Olvidadizo"],20),(Rosa,["Carisma","Mandona"],5),(Rojo,["Horrible","Necio"],10)]
listaDeRangers2 = [(Amarillo,["Horrible","Necio"],10),(Verde,["Patadura","Olvidadizo"],20),(Rosa,["Carisma","Mandona"],5)]

rangerLider ::  [PowerRanger] -> PowerRanger
rangerLider equipoRanger = findOrElse buscarAlRangerRojo (head equipoRanger) equipoRanger 

buscarAlRangerRojo :: PowerRanger -> Bool
buscarAlRangerRojo (color,_,_) = color == Rojo

-- 5. a. maximumBy: dada una lista, y una función que tome un elemento y devuelva un valor 
-- ordenable, encuentra el máximo de la misma.

