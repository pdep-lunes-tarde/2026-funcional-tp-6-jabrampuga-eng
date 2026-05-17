module Spec where
import PdePreludat
import Library
import Test.Hspec

correrTests :: IO ()
correrTests = hspec $ do
    describe "TP 5" $ do

        describe "precioFinal" $ do
            it "el precio final de un cuarto de libra es 54" $ do
                precioFinal cuartoDeLibra `shouldBe` 54

        describe "agregarIngrediente" $ do
            it "agrega un ingrediente a la hamburguesa" $ do
                ingredientes
                    (agregarIngrediente Panceta cuartoDeLibra)
                    `shouldBe`
                    [Panceta, Pan, Carne, Cheddar, Pan]

        describe "descuento" $ do
            it "aplica descuento sobre el precio base" $ do
                precioBase
                    (descuento 20 cuartoDeLibra)
                    `shouldBe`
                    16

        describe "agrandar" $ do
            it "si tiene carne agrega otra carne" $ do
                ingredientes
                    (agrandar cuartoDeLibra)
                    `shouldBe`
                    [Carne, Pan, Carne, Cheddar, Pan]

            it "si tiene pollo agrega pollo" $ do
                ingredientes
                    (agrandar (Hamburguesa 10 [Pan, Pollo]))
                    `shouldBe`
                    [Pollo, Pan, Pollo]

        describe "pdepBurger" $ do
            it "vale 110" $ do
                precioFinal pdepBurger `shouldBe` 110

        describe "dobleCuarto" $ do
            it "vale 84" $ do
                precioFinal dobleCuarto `shouldBe` 84

        describe "bigPdep" $ do
            it "vale 89" $ do
                precioFinal bigPdep `shouldBe` 89

        describe "delDia" $ do
            it "una doble cuarto del dia vale 88" $ do
                precioFinal (delDia dobleCuarto)
                    `shouldBe`
                    88

        describe "hacerVeggie" $ do
            it "reemplaza carne por pati vegano" $ do
                ingredientes
                    (hacerVeggie cuartoDeLibra)
                    `shouldBe`
                    [Pan, PatiVegano, QuesoDeAlmendras, Pan]

        describe "cambiarPanDePati" $ do
            it "cambia pan por pan integral" $ do
                ingredientes
                    (cambiarPanDePati cuartoDeLibra)
                    `shouldBe`
                    [PanIntegral, Carne, Cheddar, PanIntegral]

        describe "dobleCuartoVegano" $ do
            it "es veggie y con pan integral" $ do
                ingredientes dobleCuartoVegano
                    `shouldBe`
                    [QuesoDeAlmendras,
                     PatiVegano,
                     PanIntegral,
                     PatiVegano,
                     QuesoDeAlmendras,
                     PanIntegral]