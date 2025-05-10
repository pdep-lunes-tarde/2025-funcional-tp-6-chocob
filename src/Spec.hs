module Spec where
import PdePreludat
import Library
import Test.Hspec
import Control.Exception (evaluate)


correrTests :: IO ()
correrTests = hspec $ do
    suiteDeTestsParte1
    -- suiteDeTestsParte2
    -- suiteDeTestsParte3

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
    describe "la pdpBurger" $ do
        it "Dado un cuarto de libra y un descuento del 20% retorna un cuarto de libra con descuento aplicado, agrandada dos veces y con panceta y cheddar agregados" $ do
            pdpBurger `shouldBe` cuartoDeLibra {precioBase = 16, ingredientes = [Pan, Cheddar, Carne, Panceta, Carne, Carne, Cheddar, Pan]}
