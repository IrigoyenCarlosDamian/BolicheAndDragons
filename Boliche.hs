import System.Process
import System.IO
import Data.List (intercalate)
import Data.List
{----------------------------------------------------------------------------------------------------}
-- Definimos El TipoCliente
--Objetivo 1 Parte 1
data TipoCliente = Cliente {
  nombreCliente :: String,
  resistencia :: Int,
  listaAmigos :: [TipoCliente],
  bebidas :: [TipoBebida] 
} deriving (Eq)

{-OBSERVACION  PARA EL OBJETIVO 1 DE PARTE 2 SE LE AGREGO AL DATO DE TIPO CLIENTE  el dato bebidas que representa el historial de bebidas que va tomando un cliente  -}

--Funcion que sirve para listar  los amigos de un cliente Recibe un Cliente y devuelve un string que representa los nombres de los amigos del cliente,
--Por el efecto de la Jarra Loca tambien optamos por mostrar la resistencia de los maigos del cliente 
listarAmigos :: TipoCliente -> String
listarAmigos cliente =
  if null (listaAmigos cliente)
    then "No tiene amigos"
    else intercalate ", " (map mostrarAmigo (listaAmigos cliente))
  where
    mostrarAmigo amigo = nombreCliente amigo ++ " (Resistencia: " ++ show (resistencia amigo) ++ ")"


-- Derivamos la instancia de Show para TipoCliente
--Permite visualizar los campos de del tipo de dato cliente de una forma mas clara
instance Show TipoCliente where
  show (Cliente nombre resistencia listaAmigos bebidas) =
    "Nombre: " ++ nombre
    ++ "\nResistencia: " ++ show resistencia ++ "\nAmigos:"
     ++ listarAmigos (Cliente nombre resistencia listaAmigos bebidas)
    ++ "\nBebidas: " ++ mostrarBebidas bebidas
    where
      mostrarBebidas [] = "No ha tomado ninguna bebida"
      mostrarBebidas bebidas = intercalate ", " (map show bebidas)

--OBJETIVO 3 PARTE 2 
--Definimos el tipo de dato itineraro 
data TipoItinerario = Itinerario { 
  nombreItinerario :: String,
  duracion :: Float, 
  detalle :: [TipoCliente -> TipoCliente]
} 

instance Show TipoItinerario where 
    show i = nombreItinerario i

--Definicion de los itinerarios
mezclaExplosiva:: TipoItinerario 
mezclaExplosiva  = Itinerario "Mezcla explosiva" 2.5 [tomarBebida (GrogXD) ,tomarBebida (GrogXD) ,tomarBebida (Klusener "huevo") , tomarBebida (Klusener "frutilla") ] 
  

itinerarioBasico:: TipoItinerario 
itinerarioBasico = Itinerario "Itinerario basico" 5.0 [ tomarBebida (JarraLoca), tomarBebida (Klusener "Chocolate"), rescatarse (2), tomarBebida (Klusener "huevo")]

salidaDeAmigos:: TipoItinerario 
salidaDeAmigos = Itinerario "Salida de amigos" 1.0 [tomarBebida (Soda 1), tomarBebida (Tintico), agregarARC (robertoCarlos), tomarBebida (JarraLoca)]


--Objetivo 3b 
hacerItinerario:: [TipoCliente -> TipoCliente] -> TipoCliente -> TipoCliente
hacerItinerario [] cliente = cliente
hacerItinerario (primero: resto) cliente = hacerItinerario resto (primero cliente)

--Objetivo 4a
intensidadItinerario:: TipoItinerario -> Float
intensidadItinerario itinerario = (genericLength (detalle itinerario)) / duracion itinerario



--Objetivo 4b
obtenerItinerarioMasIntenso:: [TipoItinerario] -> TipoItinerario
obtenerItinerarioMasIntenso[itinerario] = itinerario 
obtenerItinerarioMasIntenso(primero:resto) = mayorItinerario primero resto 
                        where 
                        mayorItinerario itinerario1 [] = itinerario1 
                        mayorItinerario itinerario1 (itinerario2:restoAux)
                                |   (intensidadItinerario itinerario1) <= (intensidadItinerario itinerario2) = mayorItinerario itinerario2 restoAux
                                |   otherwise = mayorItinerario itinerario1 restoAux



hacerItinerarioIntenso:: [TipoItinerario] -> TipoCliente -> TipoCliente
hacerItinerarioIntenso [] cliente = cliente 
hacerItinerarioIntenso [itinerario] cliente = hacerItinerario (detalle itinerario) cliente
hacerItinerarioIntenso itinerarios cliente = (hacerItinerario(detalle (obtenerItinerarioMasIntenso itinerarios)) cliente) 








-- Definimos los clientes que se solicitaron
--Objetevio 2  Parte 1 
-- Para el objetivo 1 de la parte dos se le agrego las bebidas que se solicitaba a cada cliente 
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
data TipoBebida = GrogXD | JarraLoca | Klusener { sabor :: String } | Tintico | Soda { fuerza :: Int } | JarraPopular { espirituosidad :: Int}  deriving Eq





-- Derivamos la instancia de Show para TipoBebida
instance Show TipoBebida where
  show GrogXD = "GrogXD"
  show JarraLoca = "JarraLoca"
  show (Klusener sabor) = "Klusener " ++ sabor
  show Tintico = "Tintico"
  show (Soda fuerza) = "Soda De Fuerza:" ++ show fuerza
  show (JarraPopular espirituosidad) = "Jarra popular de espirituosidad: " ++ show espirituosidad

-- La Funcion Como Esta devuelve cuán ebrio está un cliente: fresco, piola o duro se basa en la resistencia del del cliente y en la longitud de la lista de amigos del cliente
--Recibe un  cliente y devuelve un String que  representa su estado de ebriedad
--Objetivo 3 Parte 1

comoEsta :: TipoCliente -> String
comoEsta cliente
  | resistencia cliente > 50 = "fresco"
  | resistencia cliente < 50 && length (listaAmigos cliente) > 1 = "piola"
  | otherwise = "duro"

-- Permite agregar a un cliente a la lista de amigos de otro cliente
---Recibe  dos cliente (cliente 1,cliente 2) y hace que el cliente 1 agrege al cliente 2 a su lista de amigs 
-- Objetico 4 Parte 1
agregarAmigo :: TipoCliente -> TipoCliente -> TipoCliente
agregarAmigo cliente amigo
  | nombreCliente cliente == nombreCliente amigo = cliente -- No se puede agregar a sí mismo como amigo
  | amigo `elem` listaAmigos cliente = cliente -- No se puede agregar más de una vez al mismo amigo
  | otherwise = cliente { listaAmigos = amigo : listaAmigos cliente }



--Permite agregar a Roberto Carlos como amigo
--Dado a que roberto carlos es un dato de tipo cliente se hizo esta funcion auxiliar para el caso en el que se tiene que agregar a otro cliente a la lista de amigos de roberto carlos 
agregarARC :: TipoCliente -> TipoCliente -> TipoCliente
agregarARC amigo cliente
  | nombreCliente cliente == nombreCliente amigo = cliente -- No se puede agregar a sí mismo como amigo
  | amigo `elem` listaAmigos cliente = cliente -- No se puede agregar más de una vez al mismo amigo
  | otherwise = cliente { listaAmigos = amigo : listaAmigos cliente }

--Objetivo 5

-- Efecto de la bebida en el cliente  
--Dada una bebida y un cliente devuelve al cliente afectado por una bebida dada 
tomarBebida :: TipoBebida -> TipoCliente -> TipoCliente
tomarBebida GrogXD cliente = cliente { resistencia = 0, bebidas = GrogXD : bebidas cliente} 
--Se Afecta al cliente con map lo que hacemos es aplicar la funcion afectarAmigo que le resta 1o de resistencia a todos los amigos de un cliente dado 
tomarBebida JarraLoca cliente = cliente { resistencia = resistencia cliente - 10, bebidas = JarraLoca : bebidas cliente, listaAmigos = map afectarAmigo (listaAmigos cliente)}
  where
    afectarAmigo amigo = amigo { resistencia = resistencia amigo - 10}
tomarBebida (Klusener gusto) cliente = cliente { resistencia = (resistencia cliente - length gusto), bebidas = (Klusener gusto) : bebidas cliente }
tomarBebida Tintico cliente = cliente { resistencia = resistencia cliente + (length $ listaAmigos cliente) * 5, bebidas = Tintico : bebidas cliente}
--con replicate fuerza lo que hacemos e replicar tantas veces la r como lo indique la fuera de la soda
tomarBebida (Soda fuerza) cliente = cliente { nombreCliente = "e" ++ replicate fuerza 'r' ++ "p" ++ nombreCliente cliente, bebidas = (Soda fuerza) : bebidas cliente}
--Objetivo 5 Parte 2 
tomarBebida (JarraPopular espirituosidad) cliente = agregarAmigosEnCascada espirituosidad (listaAmigos cliente) cliente { bebidas = JarraPopular espirituosidad : bebidas cliente }
  where
    agregarAmigosEnCascada 0 _ cliente = cliente
    agregarAmigosEnCascada _ [] cliente = cliente
    agregarAmigosEnCascada n (amigo:restoAmigos) cliente = agregarAmigosEnCascada (n - 1) (listaAmigos amigo ++ restoAmigos) (agregarAmigo cliente amigo) 

    {-Observacion  la funcion agregar amigo en  cascada perimte que un cliente pueda hacerse amigos de sus amigos 
    
    la funcion recibe  como parametros el nivel de espirtusidad de la jarrra popular la cual nos indica cuantos son los niveles de indireccion a la hora de aplicar la funcion agregar amigo
    un cliente y su lista de amigos 
    Recursion:  
      - 1er caso Base si el nivel de indireccion es 0 se devulve el cliente 
      - 2do cado base si la lista de amigos de un cliente es vacia se devuelve al cliente 
      - Caso Recursivo se llama a agregar amigos en cascada con un nivbel menos de indirecion , un cliente y la lista de amgigos 
      -}

--Objetico 6 Parte 1 
-- Función para rescatarse
-- Dado un  entero que representa una cantidad de horas  y un cliente devuelve al cliente con la reistencia aumentada 
rescatarse :: Int -> TipoCliente -> TipoCliente
rescatarse horas cliente
  | horas > 3 = cliente { resistencia = resistencia cliente + 200 }
  | horas <= 0 = cliente
  | otherwise = cliente { resistencia = resistencia cliente + 100 }



--Dada una lista de bebidas y un cliente devuelve al cliente afectado por las bebidas que tomo estas bebidas  se  registran en el historial de bebidas del cliente
--OBJETIVO 1 PARTE 2
tomarTragos :: TipoCliente -> [TipoBebida] -> TipoCliente
tomarTragos cliente [] = cliente -- Caso base: no hay más tragos para tomar
tomarTragos cliente (trago:restoTragos) = tomarTragos (tomarBebida trago cliente) restoTragos -- Hay más tragos por tomar
 
--Dado el historial de bebidas de un cliente hace que el cliente tome la ultima bebida de su historial de bebida 
--Dado un cliente devuelve al cliente afectado por la ultima bebida en su historial de bebidas 
--OBJETIVO 1 PARTE 2 
dameOtro :: TipoCliente -> TipoCliente
dameOtro cliente  
  | length ( bebidas cliente) == 0 = cliente
  | otherwise = tomarBebida (head(bebidas cliente)) cliente

--OBJETIVO 2 PARTE 2 
-- Dada una lista de bebidas y un cliente  , devuelve cuales bebidas puede tomar dicho cliente  que lo dejen con un una resistencia mayor a 0
cualesPuedeTomar :: TipoCliente -> [TipoBebida] -> [TipoBebida]
cualesPuedeTomar _ [] = []
cualesPuedeTomar cliente (bebida:restoBebidas)
  | resistencia (tomarBebida bebida cliente) > 0 = bebida : cualesPuedeTomar cliente restoBebidas
  | otherwise = cualesPuedeTomar cliente restoBebidas

--OBJETIVO 2 PARTE 2
--Hace uso de la funcion anterior y dada la lista de bebidas que el cliente puede tomar que lo dejan  con una resistencia mayor a 0  devuelve la longitud de dicha lista
cuantasPuedeTomar :: TipoCliente -> [TipoBebida] -> Int
cuantasPuedeTomar cliente bebidas = length (cualesPuedeTomar cliente bebidas)

