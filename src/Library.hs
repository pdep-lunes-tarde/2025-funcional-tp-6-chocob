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
cuartoDeLibra = Hamburguesa { precioBase = 20, ingredientes = [Carne,Pan,Cheddar,Pan] }

precioFinal :: Hamburguesa -> Number
precioFinal hamburguesa = sumatoriaIngredientes hamburguesa + precioBase hamburguesa

sumatoriaIngredientes :: Hamburguesa -> Number
sumatoriaIngredientes hamburguesa = sum . map precioIngrediente $ ingredientes hamburguesa 

-- agrandar: cada vez que se agranda una hamburguesa se agrega otro ingrediente base
-- (por ahora, son Carne o Pollo), se elige el ingrediente base a agregar según lo que ya haya en la hamburguesa 
-- (si había carne se agrega carne, si había pollo se agrega pollo, si había ambos da igual cuál se agregue).
agrandar :: Hamburguesa -> Ingrediente -> Hamburguesa
agrandar hamburguesa ingredienteBase
    | ingredienteBase == Carne || ingredienteBase == Pollo = 
        hamburguesa {ingredientes = agregarIngrediente ingredienteBase (ingredientes hamburguesa)}
    | otherwise = hamburguesa

agregarIngrediente :: Ingrediente -> [Ingrediente] -> [Ingrediente]
agregarIngrediente ingrediente listaIngredientes = ingrediente : listaIngredientes

descuento :: Number -> Hamburguesa -> Hamburguesa
descuento porcentaje hamburguesa = hamburguesa {precioBase = precioBase hamburguesa - (precioBase hamburguesa * porcentaje / 100)}
