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
