

-- Definimos El TipoCliente
data TipoCliente = Cliente {
  nombreCliente :: String,
  resistencia :: Int,
  listaAmigos :: [TipoCliente],
  bebidas :: [TipoBebida]
} deriving (Show,Eq)


data TipoItinerario = Itinerario { 
  nombreItinerario :: String,
  duracion :: Float, 
  detalle :: [TipoCliente -> TipoCliente]
} 






--mezclaExplosiva:: TipoItinerario 
--mezclaExplosiva  = Itinerario "Mezcla explosiva" 2.5 [tomarBebida GrogXD cliente,tomarBebida GrogXD cliente,
-- tomarBebida (Klusener "huevo") cliente, tomarBebida (Klusener "frutilla") cliente ] 
 
-- Definimos los clientes que se solicitaron
rodri :: TipoCliente
rodri = Cliente "Rodri" 55 [] [Tintico]  -- Rodri tomó un tintico

marcos :: TipoCliente
marcos = Cliente "Marcos" 40 [rodri] [Klusener "guinda"]  -- Marcos tomó un Klusener de guinda

cristian :: TipoCliente
cristian = Cliente "Cristian" 2 [] [GrogXD, JarraLoca]  -- Cristian tomó un grog XD y una jarraLoca

ana :: TipoCliente
ana = Cliente "Ana" 120 [marcos, rodri] []  -- Ana no tomó nada

robertoCarlos :: TipoCliente
robertoCarlos = Cliente "Roberto Carlos" 165 [] []


-- Definimos el tipo de bebida
data TipoBebida = GrogXD | JarraLoca | Klusener { sabor :: String } | Tintico | Soda { fuerza :: Int }  deriving Eq





-- Derivamos la instancia de Show para TipoBebida
instance Show TipoBebida where
  show GrogXD = "GrogXD"
  show JarraLoca = "JarraLoca"
  show (Klusener sabor) = "Klusener " ++ sabor
  show Tintico = "Tintico"
  show (Soda fuerza) = "Soda " ++ show fuerza

-- La Funcion Como Esta devuelve cuán ebrio está un cliente: fresco, piola o duro
comoEsta :: TipoCliente -> String
comoEsta cliente
  | resistencia cliente > 50 = "fresco"
  | resistencia cliente < 50 && length (listaAmigos cliente) > 1 = "piola"
  | otherwise = "duro"

-- Permite agregar a un cliente a la lista de amigos de otro cliente
agregarAmigo :: TipoCliente -> TipoCliente -> TipoCliente
agregarAmigo cliente amigo
  | nombreCliente cliente == nombreCliente amigo = cliente -- No se puede agregar a sí mismo como amigo
  | amigo `elem` listaAmigos cliente = cliente -- No se puede agregar más de una vez al mismo amigo
  | otherwise = cliente { listaAmigos = amigo : listaAmigos cliente }

-- Efecto de la bebida en el cliente
tomarBebida :: TipoBebida -> TipoCliente -> TipoCliente
tomarBebida GrogXD cliente = cliente { resistencia = 0, bebidas = GrogXD : bebidas cliente} 
tomarBebida JarraLoca cliente = cliente { resistencia = resistencia cliente - 10, bebidas = JarraLoca : bebidas cliente, listaAmigos = map afectarAmigo (listaAmigos cliente)}
  where
    afectarAmigo amigo = amigo { resistencia = resistencia amigo - 10}
tomarBebida (Klusener gusto) cliente = cliente { resistencia = (resistencia cliente - length gusto), bebidas = (Klusener gusto) : bebidas cliente }
tomarBebida Tintico cliente = cliente { resistencia = resistencia cliente + (length $ listaAmigos cliente) * 5, bebidas = Tintico : bebidas cliente}
tomarBebida (Soda fuerza) cliente = cliente { nombreCliente = "e" ++ replicate fuerza 'r' ++ "p" ++ nombreCliente cliente, bebidas = (Soda fuerza) : bebidas cliente}


-- Función para rescatarse
rescatarse :: Int -> TipoCliente -> TipoCliente
rescatarse horas cliente
  | horas > 3 = cliente { resistencia = resistencia cliente + 200 }
  | horas <= 0 = cliente
  | otherwise = cliente { resistencia = resistencia cliente + 100 }


--Muestra La Resistencia de un cliente
mostrarResistencia :: TipoCliente -> String
mostrarResistencia cliente = "Resistencia: " ++ show (resistencia cliente)


--funcion para  mostrar el nombre del cliente luego de la soda
mostrarNombre :: TipoCliente -> String
mostrarNombre cliente = "Nombre: " ++ nombreCliente cliente

--Dada una lista de bebidas y un cliente devuelve al cliente luego de haaber tomado las bebidas
tomarTragos :: TipoCliente -> [TipoBebida] -> TipoCliente
tomarTragos cliente [] = cliente -- Caso base: no hay más tragos para tomar
tomarTragos cliente (trago:restoTragos) = tomarTragos (tomarBebida trago cliente) restoTragos -- Hay más tragos por tomar
 

dameOtro :: TipoCliente -> TipoCliente
dameOtro cliente  
  | length ( bebidas cliente) == 0 = cliente
  | otherwise = tomarBebida (head(bebidas cliente)) cliente


cualesPuedeTomar :: TipoCliente -> [TipoBebida] -> [TipoBebida]
cualesPuedeTomar cliente [] = []




main :: IO ()
main = do
  putStrLn "La Noche De  Rodri Cristian y Marcos:"
  putStrLn ""
  putStrLn $ "¿Cómo está Rodri antes de arrancar la noche ? " ++ comoEsta rodri
  putStrLn ""
  putStrLn $ "¿Cómo está Cristian antes de arrancar la noche ? " ++ comoEsta cristian
  putStrLn ""
  putStrLn $ "¿Cómo está Marcos antes de arrancar la noche ? " ++ comoEsta marcos
  putStrLn ""
  putStrLn $ "¿Como esta Ana antes de arrancar la noche? " ++ comoEsta ana
  putStrLn ""
  putStrLn "Marcos Se  Siente Solo y quiere hacer amigos:"
  putStrLn ""
  -- Agregar a Ana y Rodri como amigos de Marcos
  let marcosConAmigos = agregarAmigo (agregarAmigo marcos ana) rodri

  putStrLn "Agregar a Ana y Rodri como amigos de Marcos:"

  -- Verificar el estado de Marcos después de agregar amigos
  putStrLn $ "¿Cómo está Marcos Después de Hacer Amigos? " ++ comoEsta marcosConAmigos
  putStrLn ""
  putStrLn $ "¿Cómo está Ana Antes De Arrancar La Noche " ++ comoEsta ana
  putStrLn ""
  putStrLn $ "¿Cómo estan los amigos de ana Antes De Arrancar la Nocehe"   
  putStrLn ""
  putStrLn $ "Resistencia de sus amigos: " ++ show (map resistencia (listaAmigos ana))
  putStrLn ""
  let anaAux = ((tomarBebida (Klusener "huevo")) . (rescatarse 2) . (tomarBebida (Klusener "chocolate")) . (tomarBebida JarraLoca)) ana
  putStrLn $ "Resistencia De Ana Despues De Las Bebidas:\n" ++ mostrarResistencia anaAux
  putStrLn $ "Ana despues de tomar:" ++ show anaAux
  putStrLn $ "¿Cómo estan los amigos de ana Despues de las Bebidas:\n"++ show (map resistencia (listaAmigos anaAux))
  putStrLn ""
  putStrLn ""
  let ana500 = dameOtro anaAux 
  putStrLn $ "Estado de Ana despues de pedir otro:" ++ show ana500
  -- Intentar agregar a Rodri como amigo de Rodri
  putStrLn ""
  let rodriConAmigo = agregarAmigo rodri rodri
  putStrLn $ "Estado de Rodri después de intentar agregarlo como amigo de sí mismo: " ++ show rodriConAmigo
  putStrLn ""
  -- Intentar que Marcos reconozca a Rodri como amigo
  let marcosConAmigoReconocido = agregarAmigo marcos rodri
  putStrLn $ "Estado de Marcos después de intentar reconocer a Rodri como amigo: " ++ show marcosConAmigoReconocido
  putStrLn ""
  -- Hacer que Rodri reconozca a Marcos como amigo
  let rodriConMarcosAmigo = agregarAmigo rodri marcos
  putStrLn $ "Estado de Rodri después de reconocer a Marcos como amigo: " ++ show rodriConMarcosAmigo
  putStrLn ""
  putStrLn "---------------------------------------------------------------------------------"
  putStrLn $ "Resistencia De Ana Antes De Tomar un GrogXD:\n" ++ mostrarResistencia ana
  putStrLn ""
  let ana1 = tomarBebida GrogXD ana
  putStrLn $ "Resistencia De Ana Despues De Tomar Un GrogXD:\n" 
  putStrLn $ mostrarResistencia ana1
  putStrLn ""
  putStrLn "---------------------------------------------------------------------------------"
  putStrLn $ "Resistencia De Ana Antes De Tomar Una Jarra Loca:\n" ++ mostrarResistencia ana
  putStrLn ""
  let ana2 = tomarBebida JarraLoca ana
  putStrLn $ "Resistencia De Ana Despues De Tomar Un Jarra Loca:\n"
  putStrLn $ mostrarResistencia ana2
  putStrLn ""
  putStrLn $ "Resistencia de sus amigos: " ++ show (map resistencia (listaAmigos ana2))
  putStrLn ""
  putStrLn "---------------------------------------------------------------------------------"
  putStrLn $ "Resistencia De Ana Antes De Tomar Un Klusener de huevo:\n" ++ mostrarResistencia ana
  let ana3 =  tomarBebida (Klusener "Huevo") ana
  putStrLn $ "Resistencia De Ana Despues de Tomar UnKlusener de huevo:\n"
  putStrLn $ mostrarResistencia ana3
  putStrLn ""
  putStrLn "---------------------------------------------------------------------------------"
  putStrLn $ "Resistencia De Crisitian antes de tomar un Tintico :\n" ++ mostrarResistencia cristian
  putStrLn ""
  let cristian1= tomarBebida Tintico cristian
  putStrLn $ "Resistencia De Cristian Despues de Tomar Un Tintico:\n"
  putStrLn $ mostrarResistencia cristian1
  putStrLn ""
  putStrLn "---------------------------------------------------------------------------------"
  putStrLn $ "Resistencia De Ana antes de tomar un Tintico :\n" ++ mostrarResistencia ana
  putStrLn ""
  let ana4= tomarBebida Tintico ana
  putStrLn $ "Resistencia De Ana Despues de Tomar Un Tintico:\n"
  putStrLn $ mostrarResistencia ana4
  putStrLn ""
  putStrLn "---------------------------------------------------------------------------------"
  putStrLn $ "Nombre De Rodri antes de tomar una soda de fuerza 2  :\n" ++ mostrarNombre rodri
  putStrLn ""
  let rodri1 = tomarBebida (Soda 2) rodri
  putStrLn $ "Nombre De Rodri despues de tomar una soda de fuerza 2  :\n"
  putStrLn $ mostrarNombre rodri1
  putStrLn ""
  putStrLn "---------------------------------------------------------------------------------"
  putStrLn $ "Nombre De Rodri antes de tomar una soda de fuerza 10  :\n" ++ mostrarNombre rodri
  putStrLn ""
  let rodri2 = tomarBebida (Soda 10) rodri
  let rodrierp= tomarBebida(Soda 2) rodri2
  putStrLn $ "Nombre De Rodri despues de tomar una soda de fuerza 10  :\n"
  putStrLn $ mostrarNombre rodrierp
  putStrLn ""
  putStrLn "---------------------------------------------------------------------------------"
  putStrLn $ "Nombre De Ana antes de tomar una soda de fuerza 0  :\n" ++ mostrarNombre ana
  putStrLn ""
  let ana5 = tomarBebida (Soda 0) ana
  let ana = ana5
  putStrLn $ "Nombre De Ana despues de tomar una soda de fuerza 0  :\n"
  putStrLn $ mostrarNombre ana

  putStrLn ""
  putStrLn "---------------------------------------------------------------------------------"
  putStrLn $ "Resistencia De Rodri antes de rescatarse :\n" ++ mostrarResistencia rodri
  putStrLn ""
  let rodri3 = rescatarse 5 rodri
  putStrLn $ "Resistencia de Rodri después de rescatarse 5 horas:\n" ++ mostrarResistencia rodri3
  putStrLn ""
  putStrLn "---------------------------------------------------------------------------------"
  putStrLn $ "Resistencia De Rodri antes de rescatarse :\n" ++ mostrarResistencia rodri
  putStrLn ""
  let rodri4 = rescatarse 1 rodri
  putStrLn $ "Resistencia de Rodri después de rescatarse 1 hora:\n" ++ mostrarResistencia rodri4
  putStrLn ""
  putStrLn "---------------------------------------------------------------------------------"









