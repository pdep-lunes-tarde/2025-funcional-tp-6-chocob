module Spec where
import PdePreludat
import Library
import Test.Hspec
import Control.Exception (evaluate)


correrTests :: IO ()
correrTests = hspec $ do
    suiteDeTestsParte1
    suiteDeTestsParte2
    suiteDeTestsParte3

suiteDeTestsParte1 = describe "Hamburguesas" $ do
    let mcPollo = Hamburguesa {precioBase = 20, ingredientes = [Pan,Pollo,QuesoDeAlmendras,Pan]}
    
    describe "precioFinal" $ do
        it "El precio final de una hamburguesa es la sumatoria de los precios de los ingredientes + el precio base." $ do
            precioFinal cuartoDeLibra `shouldBe` 54
    describe "agrandar" $ do
        it "Dada una hamburguesa de carne retorna la hamburguesa con una carne adicional en su lista de ingredientes" $ do
            agrandar cuartoDeLibra `shouldBe` cuartoDeLibra {ingredientes = [Pan,Carne,Carne,Cheddar,Pan]}
        it "Dada una hamburguesa de pollo retorna la hamburguesa con un pollo adicional en su lista de ingredientes" $ do
            agrandar mcPollo `shouldBe` mcPollo {ingredientes = [Pan,Pollo, Pollo,QuesoDeAlmendras,Pan]}
    describe "agregarIngrediente" $ do
        it "Dado un ingrediente y una hamburguesa retorna la hamburguesa con el ingrediente agregado" $ do
            agregarIngrediente Panceta cuartoDeLibra `shouldBe` cuartoDeLibra {ingredientes = [Pan,Panceta,Carne,Cheddar,Pan]}
    describe "descuento" $ do
        it "Dada una hamburguesa y un portentaje retorna la hamburguesa con el precio base actualizado" $ do
            descuento 10 cuartoDeLibra `shouldBe` cuartoDeLibra {precioBase = 18}
    describe "pdepBurger" $ do
        it "El precio final de la pdepBurger deberia ser 110 " $ do
            precioFinal pdepBurger `shouldBe` 110
        it "Dado un cuarto de libra y un descuento del 20% retorna un cuarto de libra con descuento aplicado, agrandada dos veces y con panceta y cheddar agregados" $ do
            pdepBurger `shouldBe` cuartoDeLibra {precioBase = 16, ingredientes = [Pan, Cheddar, Carne, Panceta, Carne, Carne, Cheddar, Pan]}
   
suiteDeTestsParte2 = describe "Algunas hamburguesas mas" $ do  
    describe "dobleCuarto" $ do
        it "El precio final de la Doble Cuarto deberia ser 84 " $ do
            precioFinal dobleCuarto `shouldBe` 84
        it "Un Boble cuarto un Cuarto de Libra con carne y cheddar" $ do
            dobleCuarto `shouldBe` cuartoDeLibra {precioBase = 20, ingredientes = [Pan,Cheddar, Carne, Carne, Cheddar, Pan]}
    describe "bigPdep" $ do
        it "El precio final de la Big Pdep deberia ser 89 " $ do
            precioFinal bigPdep `shouldBe` 89
        it "Una Big Pdep es un Doble Cuarto con curry agregado" $ do
            bigPdep `shouldBe` cuartoDeLibra {precioBase = 20, ingredientes = [Pan, Curry, Cheddar, Carne, Carne, Cheddar, Pan]}
    describe "delDia" $ do
        it "Dada una hamburguesa en promocion, retorna la hamburguesa con papas y con un descuento del 30% al precio base" $ do
            delDia bigPdep `shouldBe` bigPdep {precioBase = 14, ingredientes = [Papas, Pan, Curry, Cheddar, Carne, Carne, Cheddar, Pan]}
        it "una Doble Cuarto en promocion vale 88" $ do
            precioFinal (delDia dobleCuarto) `shouldBe` 88

suiteDeTestsParte3 = describe "Algunos cambios mas" $ do  
    describe "hacerVeggie" $ do
        it "Dada una hamburguesa con carne y cheddar retorna la hamburguesa en formato vegetariano" $ do
            hacerVeggie cuartoDeLibra `shouldBe` cuartoDeLibra {ingredientes = [Pan, PatiVegano, QuesoDeAlmendras, Pan]}
        it "Dada una hamburguesa de carne convertida a vegetariana retorna una hamburguesa con un precio actualizado a los nuevos productos" $ do
            precioFinal (hacerVeggie cuartoDeLibra) `shouldBe` 49
    describe "cambiarPanDePati" $ do
        it "Dada una hamburguesa con pan retorna la hamburguesa con el pan cambiado por pan integral" $ do
            cambiarPanDePati cuartoDeLibra `shouldBe` cuartoDeLibra {ingredientes = [PanIntegral, Carne, Cheddar, PanIntegral]}
        it "Dada una hamburguesa con pan integral retorna la hamburguesa con el precio actualizado" $ do
            precioFinal (cambiarPanDePati cuartoDeLibra) `shouldBe` 56
    describe "dobleCuartoVegano" $ do
        it "Un Doble Cuarto Vegano es una doble cuarto con pan integral y con pati vegano" $ do
            dobleCuartoVegano `shouldBe` dobleCuarto {ingredientes = [PanIntegral, QuesoDeAlmendras, PatiVegano, PatiVegano, QuesoDeAlmendras, PanIntegral]}
        it "Un Doble Cuarto Vegano tiene su precio actualizado por sus ingredientes" $ do
            precioFinal dobleCuartoVegano `shouldBe` 76