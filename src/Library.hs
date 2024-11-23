module Library where
import PdePreludat

doble :: Number -> Number
doble numero = numero + numero

-- Modelo inicial
data Jugador = UnJugador {
  nombre :: String,
  padre :: String,
  habilidad :: Habilidad
} deriving (Eq, Show)

data Habilidad = Habilidad {
  fuerzaJugador :: Number,
  precisionJugador :: Number
} deriving (Eq, Show)

-- Jugadores de ejemplo
bart = UnJugador "Bart" "Homero" (Habilidad 25 60)
todd = UnJugador "Todd" "Ned" (Habilidad 15 80)
rafa = UnJugador "Rafa" "Gorgory" (Habilidad 10 1)

data Tiro = UnTiro {
  velocidad :: Number,
  precision :: Number,
  altura :: Number
} deriving (Eq, Show)

type Puntos = Number

-- Funciones útiles
between :: Number -> Number -> Number -> Bool
between n m x = elem x [n .. m]

-- Punto 1

type Palo = Habilidad -> Tiro

putter :: Palo
putter habilidad = UnTiro 10 ((precisionJugador habilidad) * 2) 0

madera :: Palo
madera habilidad = UnTiro 100 ((precisionJugador habilidad) / 2) 5

hierro :: Number -> Palo
hierro n habilidad
    | 1 <= n && n <= 10 = UnTiro ((fuerzaJugador habilidad) * n) ((precisionJugador habilidad) / n) (max 0 (n - 3))

palos :: [Palo]
palos = [putter, madera] ++ (map hierro [1 .. 10])

-- Punto 2

golpe :: Jugador -> Palo -> Tiro
golpe persona palo = palo (habilidad persona)

-- Punto 3

type Requisito = Tiro -> Bool

type Modificacion = Tiro -> Tiro

cumpleRequisito :: Tiro -> Requisito -> Bool
cumpleRequisito tiro requisito = requisito tiro

supera :: [Requisito] -> Tiro -> Bool
supera requisitos tiro = all (cumpleRequisito tiro) requisitos

aplicarModificacion :: Tiro -> Modificacion -> Tiro
aplicarModificacion tiro modificacion = modificacion tiro

modificar :: [Modificacion] -> Tiro -> Tiro
modificar modificaciones tiro = foldl aplicarModificacion tiro modificaciones

nuevaVel :: Number -> Modificacion
nuevaVel v tiro = tiro {velocidad = v}

nuevaPre :: Number -> Modificacion
nuevaPre p tiro = tiro {precision = p}

nuevaAl :: Number -> Modificacion
nuevaAl a tiro = tiro {altura = a}

detener :: Tiro -> Tiro
detener tiro = tiro {velocidad = 0, precision = 0, altura = 0}

duplicarVel :: Tiro -> Tiro
duplicarVel tiro = tiro {velocidad = (velocidad tiro) * 2}

dividirAl :: Number -> Tiro -> Tiro
dividirAl n tiro = tiro {altura = (altura tiro) / n}

data Obstaculo = UnObstaculo {
    requisitos :: [Requisito],
    efectos :: [Modificacion]
} deriving(Show)

tunel :: Obstaculo
tunel = UnObstaculo [(> 90).(precision), (== 0).(altura)] [(duplicarVel), (nuevaPre 100), (nuevaAl 0)]

laguna :: Number -> Obstaculo
laguna largo = UnObstaculo [(> 80).(velocidad), (between 1 5).(altura)] [(dividirAl largo)]

hoyo :: Obstaculo
hoyo = UnObstaculo [(between 5 20).(velocidad), (== 0).(altura), (> 95).(precision)] [(detener)]

puedeSuperar :: Obstaculo -> Tiro -> Bool
puedeSuperar obstaculo tiro = supera (requisitos obstaculo) tiro

intentarSuperar :: Obstaculo -> Tiro -> Tiro
intentarSuperar obstaculo tiro
    | puedeSuperar obstaculo tiro = modificar (efectos obstaculo) tiro
    | otherwise = detener tiro

-- Punto 4

lePermiteSuperar :: Obstaculo -> Jugador -> Palo -> Bool
lePermiteSuperar obstaculo jugador palo = puedeSuperar obstaculo (golpe jugador palo)

palosUtiles :: Obstaculo -> Jugador -> [Palo]
palosUtiles obstaculo jugador = filter (lePermiteSuperar obstaculo jugador) palos

obstaculosSuperados :: [Obstaculo] -> Tiro -> [Obstaculo]
obstaculosSuperados (primero : resto) tiro
    | null [primero] = []
    | puedeSuperar primero tiro = [primero] ++ obstaculosSuperados resto (intentarSuperar primero tiro)
    | not (puedeSuperar primero tiro) = []

cuantosSupera :: [Obstaculo] -> Tiro -> Number
cuantosSupera obstaculos tiro = length (obstaculosSuperados obstaculos tiro)

cuantosPermiteSuperar :: [Obstaculo] -> Jugador -> Palo -> Number
cuantosPermiteSuperar obstaculos jugador palo = cuantosSupera obstaculos (golpe jugador palo)

maximoSegun :: (Palo -> Number) -> [Palo] -> Palo
maximoSegun f = foldl1 (mayorSegun f)

mayorSegun :: (Palo -> Number) -> Palo -> Palo -> Palo
mayorSegun f a b
  | f a > f b = a
  | otherwise = b

paloMasUtil :: [Obstaculo] -> Jugador -> Palo
paloMasUtil obstaculo jugador = maximoSegun (cuantosPermiteSuperar obstaculo jugador) palos

hola :: Tiro
hola = UnTiro 10 95 0

lista :: [Obstaculo]
lista = [(tunel), (tunel), (hoyo)]

-- Punto 5

quienCorresponde :: (Jugador, Puntos) -> Jugador
quienCorresponde (jugador, puntos) = jugador

quienGana :: (Jugador, Puntos) -> (Jugador, Puntos) -> (Jugador, Puntos)
quienGana (j1, p1) (j2, p2)
    | p1 > p2 = (j1, p1)
    | otherwise = (j2, p2)

ganador :: [(Jugador, Puntos)] -> Jugador
ganador = quienCorresponde.(foldl1 (quienGana))

noEsGanador :: [(Jugador, Puntos)] -> Jugador -> Bool
noEsGanador tabla jugador = jugador /= ganador tabla 

perdedores :: [(Jugador, Puntos)] -> [Jugador]
perdedores tabla = filter (noEsGanador tabla) (map quienCorresponde tabla)

padresPerdedores :: [(Jugador, Puntos)] -> [String]
padresPerdedores = (map padre).(perdedores)

puntaje :: [(Jugador, Puntos)]
puntaje = [(bart, 10), (rafa, 34), (todd, 15)]