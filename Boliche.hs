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

-- Itinerario de Ana
itinerarioAna :: TipoCliente -> TipoCliente
itinerarioAna cliente =
  let paso1 = efectoBebida JarraLoca cliente
      paso2 = efectoBebida (Klusener "Chocolate") paso1
      paso3 = rescatarse 2 paso2
      paso4 = efectoBebida (Klusener "Huevo") paso3
  in paso4

-- Ejecución de la consulta del itinerario de Ana
main :: IO ()
main = do
  let resultado = itinerarioAna ana
  putStrLn $ "Estado final de Ana: " ++ show resultado
  putStrLn $ "¿Cómo está Ana? " ++ comoEsta resultado