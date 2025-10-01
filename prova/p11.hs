-- type Id = Integer
-- type Nome = String
-- type Amigos = [Integer]
-- type Usuario = (Id, Nome, Amigos)
-- type Rede = [Usuario]

-- exemplo :: Rede
-- exemplo = [(1, "Pedro", [2,3]),
--             (2, "Maria", [1,4]),
--             (3, "Joao", [1,3]),
--             (4, "Paulo", [2])]
            
-- usuarioComMaisAmigos :: Rede -> Usuario
-- usuarioComMaisAmigos rede = foldl1 (\u1@(_,_,a1) u2@(_,_,a2) -> if length a1 > length a2 then u1 else u2) rede

-- usuariosMaisDeDoisAmigos :: Rede -> [Usuario]
-- usuariosMaisDeDoisAmigos = filter (\(_, _, amigos) -> length amigos > 2)

-- nomeEAmigos :: Rede -> [(Nome, Amigos)]
-- nomeEAmigos = map (\(_, nome, amigos) -> (nome, amigos))
import Data.List (maximumBy)
type Id = Int
type Nome = String
type Amigos = [Id]
type Usuario = (Id, Nome, Amigos)
type Rede = [Usuario]
--Exemplo:
exemploRede :: Rede
exemploRede =
 [ (1, "Alice", [2,3])
 , (2, "Bruno", [1,4])
 , (3, "Carla", [1])
 , (4, "Diego", [2])]
 

--1 - C
--2 - D
--3 - D
--4 - D
--5 - C


-- a) número de amigos
numeroAmigos :: Usuario -> Int
numeroAmigos (_, _, amigos) = length amigos

--b) usuário com mais amigos
maisAmigos :: Rede -> Nome
maisAmigos rede = nome
  where
    (id, nome, amigos) = maximumBy (\(_,_,a1) (_,_,a2) -> compare (length a1) (length a2)) rede

--c) usuários com mais de 2 amigos
usuariosMaisDe2Amigos :: Rede -> [Usuario]
usuariosMaisDe2Amigos rede = filter (\u -> numeroAmigos u > 2) rede

--d) nomes dos amigos de um usuário
nomeAmigos :: Rede -> [(Nome, Amigos)]
nomeAmigos = map (\(_, nome, amigos) -> (nome, amigos))