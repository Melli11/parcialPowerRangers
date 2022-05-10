module Spec where
import PdePreludat
import Library
import Test.Hspec

correrTests :: IO ()
correrTests = hspec $ do
  suiteDeTestsDeParte1
  suiteDeTestsDeParte2
  suiteDeTestsDeParte4
  -- suiteDeTestsDeParte5
suiteDeTestsDeParte1 = describe "Parte 1: Personas y PowerRangers" $ do
    let tomy = (["Galan","Poderoso"], True )
    let tomyMalo = (["Galan","Poderoso"], False )
    let rangerHabilidoso = (Verde, ["Galan","Poderoso","Confiable","Habil","Metodico","Nocturno"], 10 )

    describe "Es buena persona" $ do
      it "Dada una persona con bondad entonces se cumple que ES BUENA persona" $ do
        esBuenaPersona  tomy `shouldBe` True
      it "Dada una persona con maldad entonces se cumple que NO ES BUENA persona" $ do
        esBuenaPersona  tomyMalo `shouldBe` False
    describe "Nivel de Pelea de un Ranger" $ do
      it "Dado un ranger con nivel de pelea 10 entonces se cumple su poder es 10" $ do
        nivelDePelea rangerHabilidoso `shouldBe` 10
suiteDeTestsDeParte2 = describe "Parte 2: de Persona a Ranger " $ do
    let jony = (["Espiritu libre","Vago"], True )
    let casuality = ([], False )
    describe "convertirEnPowerRanger" $ do
      it "Dada una persona y un color, entonces convierto a la persona en un Ranger de ese color" $ do
        convertirEnPowerRanger Verde jony `shouldBe` (Verde,["SuperEspiritu libre","SuperVago"],18)
        convertirEnPowerRanger Rojo casuality `shouldBe` (Rojo,[],0)
      it "Dada una lista de habilidades las puedo potenciar  " $ do
        potenciarHabilidades jony  `shouldBe` ["SuperEspiritu libre","SuperVago"]
        potenciarHabilidades casuality `shouldBe` []
      it "Dar nivel de pelea" $ do
        darNivelDePelea jony `shouldBe` 18
        darNivelDePelea casuality `shouldBe` 0

suiteDeTestsDeParte4 = describe "Parte 4: Buscando al lider " $ do
    let jony = (["Espiritu libre","Vago"], True )
    let casuality = ([], False )
    let listaDeRangers = [(Verde,["Patadura","Olvidadizo"],20),(Rosa,["Carisma","Mandona"],5),(Rojo,["Horrible","Necio"],10)]
    let listaDeRangers2 = [(Amarillo,["Horrible","Necio"],10),(Verde,["Patadura","Olvidadizo"],20),(Rosa,["Carisma","Mandona"],5)]
    describe "findOrElse" $ do
      it "Dada una condición, un valor y una lista, devuelve el primer elemento  que cumpla la condición, o el valor dado si ninguno la cumple" $ do
        findOrElse even 3 [1,7,11] `shouldBe` 3
        findOrElse even 3 [1,7,2] `shouldBe` 2
        findOrElse ("a"==) "b" ["a","c","d"] `shouldBe` "a"
        findOrElse ("a"==) "b" ["x","c","d"] `shouldBe` "b"
      it "Dada una lista de Rangers que tiene en su equipo al Ranger Rojo entonces él será el lider" $ do
        rangerLider listaDeRangers  `shouldBe` (Rojo,["Horrible","Necio"],10)
      it "Dada una lista de Rangers que NO tiene en su equipo al Ranger Rojo entonces  el lider será el primero de la lista , en este caso el Amarillo" $ do
        rangerLider listaDeRangers2 `shouldBe` (Amarillo,["Horrible","Necio"],10)

