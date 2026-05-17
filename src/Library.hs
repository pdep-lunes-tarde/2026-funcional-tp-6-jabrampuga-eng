module Library where
import PdePreludat

data Ingrediente =
    Carne | Pan | Panceta | Cheddar | Pollo | Curry
    | QuesoDeAlmendras | BaconDeTofu
    | PatiVegano | Papas | PanIntegral
    deriving (Eq, Show)

precioIngrediente :: Ingrediente -> Number
precioIngrediente Carne = 20
precioIngrediente Pan = 2
precioIngrediente Panceta = 10
precioIngrediente Cheddar = 10
precioIngrediente Pollo = 10
precioIngrediente Curry = 5
precioIngrediente QuesoDeAlmendras = 15
precioIngrediente BaconDeTofu = 12
precioIngrediente PatiVegano = 10
precioIngrediente Papas = 10
precioIngrediente PanIntegral = 3

data Hamburguesa = Hamburguesa {
    precioBase :: Number,
    ingredientes :: [Ingrediente]
} deriving (Eq, Show)


precioFinal :: Hamburguesa -> Number
precioFinal hamburguesa =
    precioBase hamburguesa +
    sum (map precioIngrediente (ingredientes hamburguesa))


cuartoDeLibra :: Hamburguesa
cuartoDeLibra =
    Hamburguesa 20 [Pan, Carne, Cheddar, Pan]


contiene :: Ingrediente -> Hamburguesa -> Bool
contiene ingrediente =
    any (== ingrediente) . ingredientes

agregarIngrediente :: Ingrediente -> Hamburguesa -> Hamburguesa
agregarIngrediente ingrediente hamburguesa =
    hamburguesa {
        ingredientes =
            ingrediente : ingredientes hamburguesa
    }

descuento :: Number -> Hamburguesa -> Hamburguesa
descuento porcentaje hamburguesa =
    hamburguesa {
        precioBase =
            precioBase hamburguesa *
            (1 - porcentaje / 100)
    }

agrandar :: Hamburguesa -> Hamburguesa
agrandar hamburguesa
    | contiene Carne hamburguesa =
        agregarIngrediente Carne hamburguesa

    | contiene Pollo hamburguesa =
        agregarIngrediente Pollo hamburguesa

    | otherwise =
        agregarIngrediente PatiVegano hamburguesa

pdepBurger :: Hamburguesa
pdepBurger =
    (descuento 20 . (agregarIngrediente Cheddar .(agregarIngrediente Panceta .(agrandar . agrandar))))
    cuartoDeLibra

dobleCuarto :: Hamburguesa
dobleCuarto =
    (agregarIngrediente Cheddar . agregarIngrediente Carne)
    cuartoDeLibra

bigPdep :: Hamburguesa
bigPdep =
    agregarIngrediente Curry dobleCuarto

delDia :: Hamburguesa -> Hamburguesa
delDia =
    descuento 30 .
    agregarIngrediente Papas

hacerVeggie :: Hamburguesa -> Hamburguesa
hacerVeggie hamburguesa =
    hamburguesa {
        ingredientes =
            map reemplazarIngrediente
            (ingredientes hamburguesa)
    }

reemplazarIngrediente :: Ingrediente -> Ingrediente
reemplazarIngrediente Carne = PatiVegano
reemplazarIngrediente Pollo = PatiVegano
reemplazarIngrediente Cheddar = QuesoDeAlmendras
reemplazarIngrediente Panceta = BaconDeTofu
reemplazarIngrediente ingrediente = ingrediente

cambiarPanDePati :: Hamburguesa -> Hamburguesa
cambiarPanDePati hamburguesa =
    hamburguesa {
        ingredientes =
            map cambiarPan
            (ingredientes hamburguesa)
    }

cambiarPan :: Ingrediente -> Ingrediente
cambiarPan Pan = PanIntegral
cambiarPan ingrediente = ingrediente

dobleCuartoVegano :: Hamburguesa
dobleCuartoVegano = (cambiarPanDePati . hacerVeggie) dobleCuarto