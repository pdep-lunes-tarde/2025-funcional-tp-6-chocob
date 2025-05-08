module Spec where
import PdePreludat
import Library
import Test.Hspec
import Control.Exception (evaluate)

correrTests :: IO ()
correrTests = hspec $ do
    describe "precioFinal" $ do
        it "El precio final de una hamburguesa es la sumatoria de los precios de los ingredientes + el precio base." $ do
            precioFinal cuartoDeLibra `shouldBe` 54
    describe "agrandar" $ do
        it "Agregar carne a la hamburguesa" $ do
            agrandar cuartoDeLibra Carne `shouldBe` Hamburguesa {precioBase = 20, ingredientes = [Carne,Carne,Pan,Cheddar,Pan]}