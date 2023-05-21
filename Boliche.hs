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
efectoBebida (Soda fuerza) cliente = cliente { nombreCliente = replicate fuerza 'r' ++ nombreCliente cliente }

-- Función para rescatarse
rescatarse :: Int -> TipoCliente -> TipoCliente
rescatarse horas cliente
  | horas > 3 = cliente { resistencia = resistencia cliente + 200 }
  | otherwise = cliente { resistencia = resistencia cliente + 100 }

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
  putStrLn $ "Estado de Marcos después de agregar a Ana y Rodri: " ++ show marcosConAmigos
  putStrLn ""

  -- Verificar el estado de Marcos después de agregar amigos
  putStrLn $ "¿Cómo está Marcos Después de Hacer Amigos? " ++ comoEsta marcosConAmigos
  putStrLn ""
  putStrLn "La Noche De Ana:"
  putStrLn ""
  putStrLn $ "¿Cómo está Ana antes de arrancar la noche ? " ++ comoEsta ana
  putStrLn $ "Resistencia de Ana: " ++ show (resistencia ana)
  putStrLn $ "Resistencia de sus amigos: " ++ show (map resistencia (listaAmigos ana))
  putStrLn ""

  let resultado1 = efectoBebida JarraLoca ana
  putStrLn $ "Estado después de tomar una jarra loca:\n" ++ show resultado1
  putStrLn $ "Resistencia de Ana después de tomar una jarra loca: " ++ show (resistencia resultado1)
  putStrLn $ "Resistencia de sus amigos después de tomar una jarra loca: " ++ show (map resistencia (listaAmigos resultado1))
  putStrLn ""

  let resultado2 = efectoBebida (Klusener "Chocolate") resultado1
  putStrLn $ "Estado después de tomar un Klusener de chocolate:\n" ++ show resultado2
  putStrLn $ "Resistencia de Ana después de tomar un Klusener de chocolate:\n" ++ show (resistencia resultado2)
  putStrLn ""
  putStrLn $ "El De Seguridad Se Puso La Gorra ,Te Rescatas o te tengo que sacar: "
  putStrLn ""
  let resultado3 = rescatarse 2 resultado2
  putStrLn $ "Estado después de rescatarse:\n" ++ show resultado3
  putStrLn $ "Resistencia de Ana después de rescatarse:\n" ++ show (resistencia resultado3)
  putStrLn ""

  let resultado4 = efectoBebida (Klusener "Huevo") resultado3
  
  putStrLn $ "Resistencia de Ana después de tomar un Klusener de huevo:\n" ++ show (resistencia resultado4)
  putStrLn ""
  putStrLn $ "Fue Una Noche Agitda ¿Como Terimno Ana Despues Del Boliche?"
  putStrLn ""
  putStrLn $ "¿Cómo está Ana? " ++ comoEsta resultado4
  putStrLn ""
  putStrLn $ "Estado final de Ana: \n" ++ show resultado4
  putStrLn ""
  putStrLn "¿Como Terminaron Los Amigos De Ana La Noche De Boliche?:"
  mapM_ (\amigo -> putStrLn $ nombreCliente amigo ++ ": " ++ comoEsta amigo) (listaAmigos resultado4)

