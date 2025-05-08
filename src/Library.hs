module Library where
import PdePreludat

data Ingrediente =
    Carne | Pan | Panceta | Cheddar | Pollo | Curry | QuesoDeAlmendras
    deriving (Eq, Show)

precioIngrediente :: Ingrediente -> Number
precioIngrediente Carne = 20
precioIngrediente Pan = 2
precioIngrediente Panceta = 10
precioIngrediente Cheddar = 10
precioIngrediente Pollo =  10
precioIngrediente Curry = 5
precioIngrediente QuesoDeAlmendras = 15

data Hamburguesa = Hamburguesa {
    precioBase :: Number,
    ingredientes :: [Ingrediente]
} deriving (Eq, Show)

cuartoDeLibra :: Hamburguesa
cuartoDeLibra = Hamburguesa { precioBase = 20, ingredientes = [Carne,Pan,Cheddar,Pan] }

precioFinal :: Hamburguesa -> Number
precioFinal hamburguesa = sumatoriaIngredientes hamburguesa + precioBase hamburguesa

sumatoriaIngredientes :: Hamburguesa -> Number
sumatoriaIngredientes hamburguesa = sum . map precioIngrediente $ ingredientes hamburguesa 