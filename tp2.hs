
data Auto = UnAuto {
    marca ::String,
    modelo :: Int,
    kilometraje :: Float } deriving (Show, Eq)

--Defino estructura de personas con la info correspondiente
data Persona = UnaPersona {
    nombre :: String,
    impuesto :: Bool,
    auto :: [Auto]
    } deriving (Show, Eq)

--Defino las marcas de autos importadas
importadas :: [String]
importadas = ["ferrari"]

ferrari, fitito, reno :: Auto
ferrari = UnAuto "ferrari" 1990 100
fitito = UnAuto "fiat" 1960 1000000
reno = UnAuto "renault" 2023 0

--Defino las 3 Personas Diferentes con su nombre, impuestos y el conjunto de autos 
ricardoFort, snoop, moria :: Persona
ricardoFort = UnaPersona "RicardoFort" True [ferrari, reno, fitito]
snoop = UnaPersona "SnoopDogg" False [reno]
moria = UnaPersona "MoriaCasan" False [reno, fitito]

--Formula de valor de c/auto 
valorAuto :: Auto -> Float
valorAuto (UnAuto marca modelo km)
    | marca `elem` importadas = fromIntegral modelo * km * 10
    | otherwise = fromIntegral modelo + km + 420 

--Calcula el valor Total de los Autos 
valorTotalAutos :: Persona -> Float
valorTotalAutos (UnaPersona _ _ auto) = sum (map valorAuto auto) 

--Verifica si la persona paga impuestos y si el valor total de sus autos es mayor a 1000000.
esMillonarioHonesto :: Persona -> Bool
esMillonarioHonesto personas = impuesto personas && valorTotalAutos personas > 1000000
