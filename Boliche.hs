
--Definimos El TipoCliente
data TipoCliente = Cliente { 
	nombreCliente :: String, 
	resistencia :: Int, 
	listaAmigos :: [TipoCliente]
} deriving(Show)



--Definimos la bebida como un nombre y su efecto 
data TipoBebida = Bebida {
	nombreBebida :: String,
	efecto :: (TipoCliente -> TipoCliente)
} 


{-Definicion Del Efecto De Cada Bebida-}

cambiarResistencia :: (Int -> Int) -> TipoCliente -> TipoCliente
cambiarResistencia funcion cliente = cliente { resistencia = (funcion (resistencia  cliente)) }


{-VER SI SE DEBE AGREGAR ALGO MAS-}
grog_xd_efecto :: TipoCliente -> TipoCliente
grog_xd_efecto cliente = cliente { resistencia = 0 }

jarra_loca_efecto :: TipoCliente -> TipoCliente
jarra_loca_efecto cliente = cambiarResistencia (10 -) cliente

klusener_efecto :: TipoCliente -> TipoCliente
klusener_efecto cliente = cliente

tintico_efecto :: TipoCliente -> TipoCliente
tintico_efecto cliente = cliente

soda_efecto :: TipoCliente -> TipoCliente
soda_efecto cliente = cliente

{-Definicion de la bebida y su efecto-}
--para el klusner ver de como poner el sabor y en la jarra  popular la esprituosidad 
grog_xd = Bebida "Grog XD" grog_xd_efecto
jarra_loca = Bebida "Jarra Loca" jarra_loca_efecto
klusener = Bebida "klusener" klusener_efecto
tintico = Bebida "tintico" tintico_efecto
soda = Bebida "soda" soda_efecto

--Definimos A Los CLientes Solicitados 
rodri = Cliente "Rodri"     55 []
marcos = Cliente "Marcos"    40 [rodri]
cristian = Cliente "Cristian"  2  []
ana = Cliente "Ana"       120 [marcos, rodri]
--se Agrego el cliente Roberto Carlos
robertoCarlos= Clinete "Roberto Carlos" 165 []               



--La Funcion Como Esta Devuelve cuan  ebrio  esta un cliente fresco piola o duro 
comoEsta :: TipoCliente -> String
comoEsta cliente 
	| resistencia cliente > 50 = "fresco"
	| resistencia cliente < 50 &&  length (listaAmigos cliente) > 1 = "piola"
	| otherwise = "duro"

--Permite a un argegar un clineta la lista de amigos de otro cliente 
agregarAmigo :: TipoCliente -> TipoCliente -> TipoCliente
agregarAmigo cliente amigo 
	| ((nombreCliente cliente) == (nombreCliente amigo)) || (any (((==) (nombreCliente amigo)).nombreCliente) (listaAmigos cliente)) = cliente
	| otherwise = cliente { listaAmigos = amigo : (listaAmigos cliente) } 



--Funcion que permite 'rescatarse' a  un cliente
rescatarse :: TipoCliente -> Int -> TipoCliente
rescatarse cliente horas | horas > 3 = cliente { resistencia = (resistencia cliente) + 200 }
						 | horas > 0 = cliente { resistencia = (resistencia cliente) + 100 }
						 | otherwise = cliente 
