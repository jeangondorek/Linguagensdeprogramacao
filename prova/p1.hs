type Matricula = Integer
type Nome = String
type Notas = (Double, Double, Double)
type Estudante = (Matricula, Nome, Notas)
type Turma = [Estudante]

exemplo :: Turma
exemplo = [(1, "Pedro", (5.0, 7.5, 4.5)),
            (2, "Maria", (9.0, 8.0, 10.0)),
            (3, "Joao", (3.0, 7.0, 4.5)),
            (4, "Paulo", (7.0, 5.0, 9.5)),
            (5, "Ana", (8.5, 8.0, 9.0))]

exemplo2 = [(1, "Pedro1", (0.5, 7.5, 4.5)),
            (2, "Maria1", (9.5, 1.0, 10.0)),
            (3, "Joao1", (3.0, 7.5, 4.5)),
            (4, "Paulo1", (7.0, 5.0, 9.0)),
            (5, "Ana1", (8.5, 8.0, 10.0))]

mediaTurma :: Turma -> Double
mediaTurma t = sum [mediaAluno notas | (_, _, notas) <- t] / fromIntegral (length t)

mediaAluno :: (Double, Double, Double) -> Double
mediaAluno (a, b, c) = (a + b + c) / 3

alunosMediaMaior6 :: Turma -> [Estudante]
alunosMediaMaior6 = filter (\(_, _, notas) -> mediaAluno notas > 6)

retornaMelhorTurma :: Turma -> Turma -> Turma
retornaMelhorTurma t1 t2 = if mediaTurma t1 > mediaTurma t2 then t1 else t2

nomeEMediaPorAluno :: Turma -> [(Nome, Double)]
nomeEMediaPorAluno = map (\(_, nome, notas) -> (nome, mediaAluno notas))


