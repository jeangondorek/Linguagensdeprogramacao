type Id = Integer
type Nome = String
type Amigos = [Integer]
type Usuario = (Id, Nome, Amigos)
type Rede = [Usuario]

exemplo :: Rede
exemplo = [(1, "Pedro", [2,3]),
            (2, "Maria", [1,4,3]),
            (3, "Joao", [1,3]),
            (4, "Paulo", [2])]
            
usuarioComMaisAmigos :: Rede -> Usuario
usuarioComMaisAmigos rede = foldl1 (\(_,_,a1) (_,_,a2) -> if length a1 > length a2 then (_,_,a1) else (_,_,a2)) rede

usuariosMaisDeDoisAmigos :: Rede -> [Usuario]
usuariosMaisDeDoisAmigos = filter (\(_, _, amigos) -> length amigos > 2)

nomeEAmigos :: Rede -> [(Nome, Amigos)]
nomeEAmigos = map (\(_, nome, amigos) -> (nome, amigos))

