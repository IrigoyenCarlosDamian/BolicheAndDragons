

-- Definimos El TipoCliente
data TipoCliente = Cliente {
  nombreCliente :: String,
  resistencia :: Int,
  listaAmigos :: [TipoCliente]
} deriving (Show,Eq)

-- Definimos los clientes que se solicitaron
rodri :: TipoCliente
rodri = Cliente "Rodri" 55 []

marcos :: TipoCliente
marcos = Cliente "Marcos" 40 [rodri]

cristian :: TipoCliente
cristian = Cliente "Cristian" 2 []

ana :: TipoCliente
ana = Cliente "Ana" 120 [marcos, rodri]

-- Definimos el tipo de bebida
data TipoBebida = GrogXD | JarraLoca | Klusener { sabor :: String } | Tintico | Soda { fuerza :: Int }

-- La Funcion Como Esta devuelve cuán ebrio está un cliente: fresco, piola o duro
comoEsta :: TipoCliente -> String
comoEsta cliente
  | resistencia cliente > 50 = "fresco"
  | resistencia cliente < 50 && length (listaAmigos cliente) > 1 = "piola"
  | otherwise = "duro"

-- Permite agregar a un cliente a la lista de amigos de otro cliente
agregarAmigo :: TipoCliente -> TipoCliente -> TipoCliente
agregarAmigo cliente amigo
  | cliente == amigo = cliente -- No se puede agregar a sí mismo como amigo
  | amigo `elem` listaAmigos cliente = cliente -- No se puede agregar más de una vez al mismo amigo
  | otherwise = cliente { listaAmigos = amigo : listaAmigos cliente }

-- Efecto de la bebida en el cliente
efectoBebida :: TipoBebida -> TipoCliente -> TipoCliente
efectoBebida GrogXD cliente = cliente { resistencia = 0 }
efectoBebida JarraLoca cliente = cliente { resistencia = resistencia cliente - 10, listaAmigos = map afectarAmigo (listaAmigos cliente) }
  where
    afectarAmigo amigo = amigo { resistencia = resistencia amigo - 10 }
efectoBebida (Klusener gusto) cliente = cliente { resistencia = resistencia cliente - length gusto }
efectoBebida Tintico cliente = cliente { resistencia = resistencia cliente + (length $ listaAmigos cliente) * 5 }
efectoBebida (Soda fuerza) cliente = cliente { nombreCliente = "e" ++ replicate fuerza 'r' ++ "p" ++ nombreCliente cliente }

-- Función para rescatarse
rescatarse :: Int -> TipoCliente -> TipoCliente
rescatarse horas cliente
  | horas > 3 = cliente { resistencia = resistencia cliente + 200 }
  | otherwise = cliente { resistencia = resistencia cliente + 100 }


mostrarResistencia :: TipoCliente -> String
mostrarResistencia cliente = "Resistencia: " ++ show (resistencia cliente)

mostrarAmigos :: TipoCliente -> String
mostrarAmigos cliente = "Amigos: " ++ show (length (listaAmigos cliente))

mostrarNombre :: TipoCliente -> String
mostrarNombre cliente = "Nombre: " ++ nombreCliente cliente

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
  putStrLn "Marcos Se  Siente Solo y quiere hacer amigos:"
  putStrLn ""
  -- Agregar a Ana y Rodri como amigos de Marcos
  let marcosConAmigos = agregarAmigo (agregarAmigo marcos ana) rodri

  putStrLn "Agregar a Ana y Rodri como amigos de Marcos:"

  -- Verificar el estado de Marcos después de agregar amigos
  putStrLn $ "¿Cómo está Marcos Después de Hacer Amigos? " ++ comoEsta marcosConAmigos
  putStrLn ""
  putStrLn "La Noche De Ana:\n"
  putStrLn ""
  putStrLn $ "¿Cómo está Ana antes de arrancar la noche ? " ++ comoEsta ana
  putStrLn $ "Resistencia de Ana:\n" ++ mostrarResistencia ana 
  putStrLn $ "Resistencia de sus amigos: " ++ show (map resistencia (listaAmigos ana))
  putStrLn ""
  let anaJarraLoca = efectoBebida JarraLoca ana
  putStrLn $ "Resistencia de Ana después de tomar una jarra loca:\n" ++ mostrarResistencia anaJarraLoca
  putStrLn $ "Resistencia de sus amigos después de tomar una jarra loca: " ++ show (map resistencia (listaAmigos anaJarraLoca))
  putStrLn ""

  let anaKlusnerChocolate = efectoBebida (Klusener "Chocolate") anaJarraLoca
  putStrLn $ "Resistencia de Ana después de tomar un Klusener de chocolate:\n" ++ mostrarResistencia anaKlusnerChocolate
  putStrLn ""
  putStrLn $ "El De Seguridad Se Puso La Gorra ,Te Rescatas o te tengo que sacar: "
  putStrLn ""
  let anaRescatada = rescatarse 2 anaKlusnerChocolate
  putStrLn $ "Resistencia de Ana después de rescatarse:\n"  ++ mostrarResistencia anaRescatada
  putStrLn ""

  let anaKlusnerHuevo = efectoBebida (Klusener "Huevo") anaRescatada
  putStrLn $ "Resistencia de Ana después de tomar un Klusener de huevo:\n" ++ mostrarResistencia anaKlusnerHuevo
  putStrLn ""
  putStrLn $ "Resistencia de sus amigos después de tomar una jarra loca: " ++ show (map resistencia (listaAmigos anaKlusnerHuevo))
  putStrLn ""
  putStrLn $ "Fue Una Noche Agitda ¿Como Terimno Ana Despues Del Boliche?"
  putStrLn ""
  putStrLn $ "¿Cómo está Ana? " ++ comoEsta anaKlusnerHuevo
  putStrLn ""
  putStrLn ""
  putStrLn "¿Como Terminaron Los Amigos De Ana La Noche De Boliche?:"
  mapM_ (\amigo -> putStrLn $ nombreCliente amigo ++ ": " ++ comoEsta amigo) (listaAmigos anaKlusnerHuevo)

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
  let ana1 = efectoBebida GrogXD ana
  putStrLn $ "Resistencia De Ana Despues De Tomar Un GrogXD:\n" 
  putStrLn $ mostrarResistencia ana1
  putStrLn ""
  putStrLn "---------------------------------------------------------------------------------"
  putStrLn $ "Resistencia De Ana Antes De Tomar Una Jarra Loca:\n" ++ mostrarResistencia ana
  putStrLn ""
  let ana2 = efectoBebida JarraLoca ana
  putStrLn $ "Resistencia De Ana Despues De Tomar Un Jarra Loca:\n"
  putStrLn $ mostrarResistencia ana2
  putStrLn ""
  putStrLn $ "Resistencia de sus amigos: " ++ show (map resistencia (listaAmigos ana2))
  putStrLn ""
  putStrLn "---------------------------------------------------------------------------------"
  putStrLn $ "Resistencia De Ana Antes De Tomar Un Klusener de huevo:\n" ++ mostrarResistencia ana
  let ana3 =  efectoBebida (Klusener "Huevo") ana
  putStrLn $ "Resistencia De Ana Despues de Tomar UnKlusener de huevo:\n"
  putStrLn $ mostrarResistencia ana3
  putStrLn ""
  putStrLn "---------------------------------------------------------------------------------"
  putStrLn $ "Resistencia De Crisitian antes de tomar un Tintico :\n" ++ mostrarResistencia cristian
  putStrLn ""
  let cristian1= efectoBebida Tintico cristian
  putStrLn $ "Resistencia De Cristian Despues de Tomar Un Tintico:\n"
  putStrLn $ mostrarResistencia cristian1
  putStrLn ""
  putStrLn "---------------------------------------------------------------------------------"
  putStrLn $ "Resistencia De Ana antes de tomar un Tintico :\n" ++ mostrarResistencia ana
  putStrLn ""
  let ana4= efectoBebida Tintico ana
  putStrLn $ "Resistencia De Ana Despues de Tomar Un Tintico:\n"
  putStrLn $ mostrarResistencia ana4
  putStrLn ""
  putStrLn "---------------------------------------------------------------------------------"
  putStrLn $ "Nombre De Rodri antes de tomar una soda de fuerza 2  :\n" ++ mostrarNombre rodri
  putStrLn ""
  let rodri1 = efectoBebida (Soda 2) rodri
  putStrLn $ "Nombre De Rodri despues de tomar una soda de fuerza 2  :\n"
  putStrLn $ mostrarNombre rodri1
  putStrLn ""
  putStrLn "---------------------------------------------------------------------------------"
  putStrLn $ "Nombre De Rodri antes de tomar una soda de fuerza 10  :\n" ++ mostrarNombre rodri
  putStrLn ""
  let rodri2 = efectoBebida (Soda 10) rodri
  putStrLn $ "Nombre De Rodri despues de tomar una soda de fuerza 10  :\n"
  putStrLn $ mostrarNombre rodri2
  putStrLn ""
  putStrLn "---------------------------------------------------------------------------------"
  putStrLn $ "Nombre De Rodri antes de tomar una soda de fuerza 0  :\n" ++ mostrarNombre ana
  putStrLn ""
  let ana5 = efectoBebida (Soda 0) ana
  putStrLn $ "Nombre De Rodri despues de tomar una soda de fuerza 0  :\n"
  putStrLn $ mostrarNombre ana5
  putStrLn ""
  putStrLn "---------------------------------------------------------------------------------"
  putStrLn $ "Resistencia De Rodri antes de rescatarse :\n" ++ mostrarResistencia rodri
  putStrLn ""
  let rodri3 = rescatarse 5 rodri
  putStrLn $ "Resistencia de Rodri después de rescatarse:\n" ++ mostrarResistencia rodri3
  putStrLn ""
  putStrLn "---------------------------------------------------------------------------------"
  putStrLn $ "Resistencia De Rodri antes de rescatarse :\n" ++ mostrarResistencia rodri
  putStrLn ""
  let rodri4 = rescatarse 1 rodri
  putStrLn $ "Resistencia de Rodri después de rescatarse:\n" ++ mostrarResistencia rodri4
  putStrLn ""
  putStrLn "---------------------------------------------------------------------------------"









