module Library where
import PdePreludat
import GHC.IO.Handle.Types (Handle__(haBufferMode))

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
cuartoDeLibra = Hamburguesa { precioBase = 20, ingredientes = [Pan,Carne,Cheddar,Pan] }

precioFinal :: Hamburguesa -> Number
precioFinal hamburguesa = sumatoriaIngredientes hamburguesa + precioBase hamburguesa

sumatoriaIngredientes :: Hamburguesa -> Number
sumatoriaIngredientes hamburguesa = sum . map precioIngrediente $ ingredientes hamburguesa 

agrandar :: Hamburguesa -> Hamburguesa
agrandar hamburguesa
    | Carne `elem` ingredientes hamburguesa = 
        agregarIngrediente Carne hamburguesa
    | otherwise = 
        agregarIngrediente Pollo hamburguesa

agregarIngrediente :: Ingrediente -> Hamburguesa -> Hamburguesa
agregarIngrediente ingrediente hamburguesa = hamburguesa {ingredientes = agregarSegundo ingrediente (ingredientes hamburguesa)}

agregarSegundo :: Ingrediente -> [Ingrediente] -> [Ingrediente]
agregarSegundo ingrediente (cabeza:cola) = cabeza : ingrediente : cola 

-- Entiendo que esto quedo completamente declarativo en la funcion agrandar, yo hubiese trabajado directamente con la funcion agregarSegundo
-- sin embargo cree agregarIngrediente porque no correspondia con el pedido del enciado 
-- (recibir un ingrediente y una hamburguesa y devolver una hamburguesa)

-- Version previa de agrandar
-- agrandar :: Hamburguesa -> Ingrediente -> Hamburguesa
-- agrandar hamburguesa ingredienteBase
--     | ingredienteBase == Carne || ingredienteBase == Pollo = 
--         hamburguesa {ingredientes = agregarIngrediente ingredienteBase (ingredientes hamburguesa)}
--     | otherwise = hamburguesa

-- agregarIngrediente :: Ingrediente -> [Ingrediente] -> [Ingrediente]
-- agregarIngrediente ingrediente listaIngredientes = ingrediente : listaIngredientes

descuento :: Number -> Hamburguesa -> Hamburguesa
descuento porcentaje hamburguesa = hamburguesa {precioBase = precioBase hamburguesa - (precioBase hamburguesa * porcentaje / 100)}

