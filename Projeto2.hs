import System.IO
import Data.List (delete)
import Data.List (nub)




--Tarefas--

--tarefa1
tarefa1 :: IO()
tarefa1 = do 
    conteudoDisciplina <- readFile "ucs.txt"
    ficheiro <- openFile "Tarefa1.txt" WriteMode
    hClose ficheiro
    --Pedir input
    putStrLn "indique numero de dias em que o exame pode ocorrer"
    dias <- getLine
    putStrLn "indique o numero de salas disponiveis por dia"
    salas <- getLine
    --linhas no ficheiro-
    let n_disciplinas = length (lines conteudoDisciplina) -- numero de disciplinas é o numero de linhas no ficheiro
    --convercao-- para int
    let n_dias = read dias :: Int
    let n_salas = read salas :: Int
    if n_disciplinas > n_dias * n_salas  -- se numero de disciplinas maior que dias livres * salas impossivel
        then putStrLn "Dias insuficientes para acomodar todos os exames"
        else escalonamento 1 n_salas (lines conteudoDisciplina)

--tarefa2
tarefa2 :: IO()
tarefa2 = do 
    conteudoDisciplina <- readFile "ucs.txt"

    ficheiro <- openFile "Tarefa2.txt" WriteMode
    hClose ficheiro
    --fazer uma copia de ucs para ficheiro 2 
    ficheiro <- openFile "Suporte2.txt" WriteMode
    hClose ficheiro

    ficheiro <- openFile "Suporte3.txt" WriteMode
    hClose ficheiro

    ficheiro <- openFile "Suporte.txt" WriteMode
    hPutStrLn ficheiro "0"
    hClose ficheiro
    --descobrir qual o ano mais alto escrever no ficheiro 1

    copyFile (lines conteudoDisciplina)

    suporte2 <- readFile' "Suporte2.txt"
    maiorAno (lines suporte2)
    let suporte = readFile' "Suporte.txt"
    maiorAnoString <- suporte
    let maiorAnoInt = read maiorAnoString :: Int -- maior ano no ficheiro
    

    --Pedir input
    putStrLn "indique numero de dias em que o exame pode ocorrer"
    dias <- getLine
    putStrLn "indique o numero de salas disponiveis por dia"
    salas <- getLine
    --linhas no ficheiro-
    let n_disciplinas = length (lines conteudoDisciplina) -- numero de disciplinas é o numero de linhas no ficheiro
    --convercao-- para int
    let n_dias = read dias :: Int
    let n_salas = read salas :: Int

    escalonamento2 maiorAnoInt 1 n_dias n_salas (lines conteudoDisciplina)

    suporte2 <- readFile' "Suporte2.txt"

    if null (lines suporte2)
        then return()
        else do 
            putStrLn "Tempo insuficiente para acomodar todos os exames"
            return()


-- tarefa 3
tarefa3 :: IO()
tarefa3 = do 
   tarefa31
   ficheiroSuporte <- readFile "Suporte.txt"
   let tamanhoInicial = length (lines ficheiroSuporte) 
   let tamanhoFinal = length (remDup(lines ficheiroSuporte)) 
   let incompativeis = tamanhoInicial - tamanhoFinal
   print incompativeis

--tarefa4
tarefa4 :: IO()
tarefa4 = do 
    --read 
    conteudoDisciplina <- readFile "ucs.txt"
    conteudoInscricao <- readFile "inscricoes.txt"
    conteudoAlunos <- readFile "listaalunos.txt"
    --limpar ficheiros
    conteudoDisciplina <- readFile "ucs.txt"
    ficheiro <- openFile "Tarefa1.txt" WriteMode
    hClose ficheiro

    --Pedir input
    putStrLn "indique numero de dias em que o exame pode ocorrer"
    dias <- getLine
    putStrLn "indique o numero de salas disponiveis por dia"
    salas <- getLine
    --linhas no ficheiro-
    let n_disciplinas = length (lines conteudoDisciplina) -- numero de disciplinas é o numero de linhas no ficheiro
    --convercao-- para int
    let n_dias = read dias :: Int
    let n_salas = read salas :: Int
    if n_disciplinas > n_dias * n_salas  -- se numero de disciplinas maior que dias livres * salas impossivel
        then putStrLn "Dias insuficientes para acomodar todos os exames"
        else escalonamento4 (lines conteudoDisciplina) (lines conteudoInscricao) (lines conteudoAlunos) 1 n_salas (lines conteudoDisciplina)

--tarefa5
tarefa5 :: IO()
tarefa5 = do 
    conteudoDisciplina <- readFile "ucs.txt"
    conteudoInscricao <- readFile "inscricoes.txt"
    conteudoAlunos <- readFile "listaalunos.txt"

    ficheiro <- openFile "Tarefa5.txt" WriteMode
    hClose ficheiro

    ficheiro <- openFile "Suporte2.txt" WriteMode
    hClose ficheiro

    ficheiro <- openFile "Suporte3.txt" WriteMode
    hClose ficheiro


    copyFile(lines conteudoDisciplina) --fazer uma copia para ficheiro suporte 2
    --Pedir input
    putStrLn "indique numero de dias em que o exame pode ocorrer"
    dias <- getLine
    putStrLn "indique o numero de salas disponiveis por dia"
    salas <- getLine

    --linhas no ficheiro-
    let n_disciplinas = length (lines conteudoDisciplina) -- numero de disciplinas é o numero de linhas no ficheiro
    --convercao-- para int
    let n_dias = read dias :: Int
    let n_salas = read salas :: Int

    if n_disciplinas > n_dias * n_salas  -- se numero de disciplinas maior que dias livres * salas impossivel
        then putStrLn "Dias insuficientes para acomodar todos os exames"
        else escalonamento5 (lines conteudoDisciplina) (lines conteudoInscricao) (lines conteudoAlunos) 1 n_salas 

--tarefa6
tarefa6 :: IO()
tarefa6 = do 
    conteudoDisciplina <- readFile "ucs.txt"
    conteudoInscricao <- readFile "inscricoes.txt"
    conteudoAlunos <- readFile "listaalunos.txt"


    ficheiro <- openFile "Tarefa6.txt" WriteMode
    hClose ficheiro

    ficheiro2 <- openFile "Suporte2.txt" WriteMode
    hClose ficheiro2 

    formatacao6 (lines conteudoDisciplina) (lines conteudoInscricao) (lines conteudoAlunos) (lines conteudoDisciplina)
    -- texto formatado no ficheiro suporte 2 quantidade nome disciplina

    putStrLn "indique numero de dias em que o exame pode ocorrer"
    dias <- getLine
    putStrLn "indique o numero de salas disponiveis por dia"
    salas <- getLine
    putStrLn "indique a capacidade das salas no formato [Sala 1,Sala 2,...]"
    lotacao <- getLine
    let lotacaoInt = read lotacao :: [Int]

    let n_dias = read dias :: Int
    let n_salas = read salas :: Int
    let lotacaoReverse = reverse lotacaoInt
    if length (lotacaoReverse) /= n_salas
        then do 
            putStrLn("lotacao diferente do numero de salas")
            return()
        else do 
            conteudoSuporte2 <- readFile' "Suporte2.txt"

            ficheiro <- openFile "Suporte.txt" WriteMode
            hPutStrLn ficheiro (head (lines conteudoSuporte2))  --escrever primeira linha do suporte2 no suporte
            hClose ficheiro
            
            escalonamento6 1 n_dias n_salas lotacaoReverse (lines conteudoSuporte2)

            suporte2 <- readFile' "Suporte2.txt"
            if suporte2 /= []
                then putStrLn ("Insuficiente para acomodar todos os exames")
                else return()
--funcoes tarefa 2 
maiorAno :: [String] -> IO()
maiorAno [] = return()
maiorAno (x:xs) = do 

    let suporte = readFile' "Suporte.txt"
    
    suporteString <-  suporte
    let suporteInt = read suporteString :: Int

    let numero = head(tail(words x))
    let numeroInt = read numero :: Int

    if numeroInt > suporteInt
        then do 
            ficheiro <- openFile "Suporte.txt" WriteMode
            hPrint ficheiro numeroInt
            hClose ficheiro
            maiorAno xs
        else maiorAno xs
            
escalonamento2 :: Int -> Int -> Int -> Int -> [String] -> IO() 
escalonamento2 anoMax dias diasMax salas [] = return()
escalonamento2 anoMax dias diasMax salas (x:xs) = do

    if dias > diasMax
        then return()
        else do 

            ficheiro <- openFile "Tarefa2.txt" AppendMode
            hPutStrLn ficheiro ("--- dia "++ show dias ++ "---")
            hClose ficheiro

            ficheiro <- openFile "Suporte3.txt" WriteMode
            hClose ficheiro --limpar ficheiro3

            --funcao que seleciona disciplinas para ficheiro3 e apaga o ficheiro 2


            repeater2 anoMax salas
            --ficheiro3 a zero
            
        

            suporte3 <- readFile' "Suporte3.txt"
            printSalas2 salas (lines suporte3)--print nos ficheiros do 3

            suporte2 <- readFile' "Suporte2.txt"
            if null (lines suporte2)
                then return()
                else do
                    ficheiro <- openFile "Suporte.txt" WriteMode
                    hPutStrLn ficheiro "0"
                    hClose ficheiro
                    maiorAno (lines suporte2)
                    let suporte = readFile' "Suporte.txt"
                    maiorAnoString <- suporte
                    let maiorAnoInt = read maiorAnoString :: Int -- maior ano no ficheiro
                    escalonamento2 maiorAnoInt (dias+1) diasMax salas (lines suporte2)

finder :: Int -> [String] -> IO()
finder _ [] = return()
finder ano (x:xs) = do --ano a encontrar, ficheiro2
    let anoLinhaString = head(tail(words x))
    let anoLinhaInt = read anoLinhaString :: Int

    if ano == anoLinhaInt
        then do
            ficheiro <- openFile "Suporte3.txt" AppendMode
            hPutStrLn ficheiro x
            hClose ficheiro --limpar ficheiro3
            copyFile xs
            return()
        else do
            ficheiro <- openFile "Suporte2.txt" AppendMode
            hPutStrLn ficheiro x
            hClose ficheiro 
            finder ano xs

 
repeater2 :: Int -> Int -> IO()
repeater2 0 _ = return()
repeater2 _ 0 = return()
repeater2 anoMax salas = do --ano maximo a procurar e numero de salas a preencher
    suporte2 <- readFile' "Suporte2.txt"

    ficheiro <- openFile "Suporte2.txt" WriteMode
    hClose ficheiro --limpar ficheiro2

    finder anoMax (lines suporte2)
    repeater2 (anoMax-1)(salas-1)


printSalas2 :: Int -> [String] -> IO()
printSalas2 salas [] = return()
printSalas2 0 (x:xs) = return()
printSalas2 salas (x:xs) = do
    ficheiro <- openFile "Tarefa2.txt" AppendMode
    hPutStrLn ficheiro ("Sala "++ show salas ++ ": "++ unwords (tail (tail(words x))))
    hClose ficheiro
    printSalas2 (salas-1) xs

--funcoes tarefa 5
copyFile :: [String] -> IO()
copyFile [] = return()
copyFile (x:xs)= do 
    ficheiro <- openFile "Suporte2.txt" AppendMode
    hPutStrLn ficheiro x
    hClose ficheiro
    copyFile xs

escalonamento5 :: [String]-> [String]-> [String]->Int -> Int  -> IO() 
escalonamento5 disciplina inscricao alunos dias salas  = do

    
    ficheiro <- openFile "Suporte.txt" WriteMode
    hClose ficheiro

    
    ficheiro <- openFile "Tarefa5.txt" AppendMode
    hPutStrLn ficheiro ("--- dia "++ show dias ++ "---")
    hClose ficheiro
   -- aqui tenho de fazer a selecao func select
    ficheiro <- openFile "Suporte3.txt" WriteMode
    hClose ficheiro
    
    repeater salas salas -- repete numero de salas ate encontrar a melhor combinacao que guarda no ficheiro 3

    suporte3 <- readFile' "Suporte3.txt"

    ficheiro <- openFile "Suporte.txt" WriteMode
    hClose ficheiro

    printSalas5 disciplina inscricao alunos salas (lines suporte3) --print selecao do suporte 3

    ficheiroSuporte <- readFile "Suporte.txt"
    let tamanhoInicial = length (lines ficheiroSuporte) 
    let tamanhoFinal = length (remDup(lines ficheiroSuporte)) 
    let incompativeis = tamanhoInicial - tamanhoFinal

    ficheiro <- openFile "Tarefa5.txt" AppendMode
    hPutStrLn ficheiro ("Numero de incompatibilidades: " ++ show incompativeis) -- print final das incompativilidad
    hClose ficheiro

    suporte2 <- readFile' "Suporte2.txt"
    
    if null (lines suporte2)
        then return()
        else escalonamento5 disciplina inscricao alunos (dias+1) salas

     
condTester :: Int -> Int-> [String] -> IO() 
condTester salas _ [] = return()
condTester 0 _ (x:xs) = return()-- tem de remover os valores utilizados do suporte2 e escolher os melhores para o suporte3  
condTester salas salasTotal (x:xs) = do -- numero de salas, ficheiro2
    conteudoDisciplina <- readFile "ucs.txt"
    conteudoInscricao <- readFile "inscricoes.txt"
    conteudoAlunos <- readFile "listaalunos.txt"

    if salas == salasTotal
        then do 
            suporte3 <- openFile "Suporte3.txt" AppendMode
            hPutStrLn suporte3 x 
            hClose suporte3

            suporte2 <- openFile "Suporte2.txt" WriteMode
            hClose suporte2

            copyFile xs -- remove o x do suporte 2 pois ja foi escolhido
        else do 
            ficheiro <- openFile "Suporte.txt" WriteMode
            hClose ficheiro --limpar o suporte 1 
            suporte3 <- readFile' "Suporte3.txt"
            let size = length (lines suporte3)
            loaderSup2 (lines suporte3) size --adicionar os valores de sup3 no 1 para testar
            -- load x
            let disciplinas = unwords(tail(tail(words x)))
            encontrarNome disciplinas (lines conteudoDisciplina) (lines conteudoInscricao) (lines conteudoAlunos)
            --testar
            ficheiroSuporte <- readFile' "Suporte.txt"
            let tamanhoInicial = length (lines ficheiroSuporte) 
            let tamanhoFinal = length (remDup(lines ficheiroSuporte)) 
            let incompativeisx = tamanhoInicial - tamanhoFinal

            if null xs 
                then do 
                    suporte3 <- openFile "Suporte3.txt" AppendMode
                    hPutStrLn suporte3 x 
                    hClose suporte3

                    suporte2 <- openFile "Suporte2.txt" WriteMode
                    hClose suporte2
                    return()
                else do
                    --reload
                    ficheiro <- openFile "Suporte.txt" WriteMode
                    hClose ficheiro --limpar o suporte 1 
                    suporte3 <- readFile' "Suporte3.txt"
                    let size = length (lines suporte3)
                    loaderSup2 (lines suporte3) size --adicionar os valores de sup3 no 1 para testar
                    --load xs
                    let disciplinas = unwords(tail(tail(words (head xs))))
                    encontrarNome disciplinas (lines conteudoDisciplina) (lines conteudoInscricao) (lines conteudoAlunos)
                    --testar
                    ficheiroSuporte <- readFile' "Suporte.txt"
                    let tamanhoInicial = length (lines ficheiroSuporte) 
                    let tamanhoFinal = length (remDup(lines ficheiroSuporte)) 
                    let incompativeisxs = tamanhoInicial - tamanhoFinal

                    if incompativeisxs >= incompativeisx
                        then do 
                            suporte3 <- openFile "Suporte3.txt" AppendMode
                            hPutStrLn suporte3 x 
                            hClose suporte3

                            suporte2 <- openFile "Suporte2.txt" WriteMode
                            hClose suporte2

                            copyFile xs -- remove o x do suporte 2 pois ja foi escolhido

                        else do 
                            suporte3 <- openFile "Suporte3.txt" AppendMode
                            hPutStrLn suporte3 (head xs )
                            hClose suporte3

                            suporte2 <- openFile "Suporte2.txt" WriteMode
                            hPutStrLn suporte2 x
                            hClose suporte2
                            
                            copyFile (tail xs) -- remove o x do suporte 2 pois ja foi escolhido

repeater :: Int -> Int-> IO()
repeater 0 salasTotal = return()
repeater salas salasTotal = do 

    suporte2 <- readFile' "Suporte2.txt"
    condTester salas salasTotal (lines suporte2)

    repeater (salas-1) salasTotal


loaderSup2 :: [String] -> Int -> IO()
loaderSup2 [] size = return()
loaderSup2 (x:xs) 0 = return()
loaderSup2 (x:xs) size = do --disciplina e sup3 para comparar size sup3

    conteudoDisciplina <- readFile "ucs.txt"
    conteudoInscricao <- readFile "inscricoes.txt"
    conteudoAlunos <- readFile "listaalunos.txt"


    let disciplinas = unwords(tail(tail(words x)))
    encontrarNome disciplinas (lines conteudoDisciplina) (lines conteudoInscricao) (lines conteudoAlunos)

    loaderSup2 xs (size-1)


printSalas5 :: [String]-> [String]-> [String]-> Int -> [String] -> IO()
printSalas5 disciplina inscricao alunos salas [] = return()
printSalas5 disciplina inscricao alunos 0 (x:xs) = return()
printSalas5 disciplina inscricao alunos salas (x:xs) = do

    let disciplinas = unwords (tail (tail(words x)))

    ficheiro <- openFile "Tarefa5.txt" AppendMode
    hPutStrLn ficheiro ("Sala "++ show salas ++ ": "++ disciplinas)
    hClose ficheiro

    encontrarNome disciplinas disciplina inscricao alunos
    printSalas5 disciplina inscricao alunos (salas-1) xs
--funcoes tarefa6

formatacao6 :: [String]->[String]->[String]->[String]->IO()
formatacao6 disciplina inscricao alunos [] = return()
formatacao6 disciplina inscricao alunos (x:xs) = do
    

    let disciplinas = unwords(tail(tail(words x)))

    --clear file 
    ficheiro2 <- openFile "Suporte.txt" WriteMode
    hClose ficheiro2 

    encontrarNome disciplinas disciplina inscricao alunos

    suporte1 <- readFile "Suporte.txt"
    let alunosInscritos = length (lines suporte1)

    suporte2 <- openFile "Suporte2.txt" AppendMode
    hPutStrLn suporte2 (show alunosInscritos++" "++disciplinas)
    hClose suporte2

    formatacao6 disciplina inscricao alunos xs

escalonamento6 :: Int -> Int -> Int -> [Int] -> [String] -> IO() 
escalonamento6 dias diasMax salas lotacao [] = return()
escalonamento6 dias diasMax salas lotacao (x:xs) = do
    if dias > diasMax 
        then return()
        else do
            ficheiro <- openFile "Tarefa6.txt" AppendMode
            hPutStrLn ficheiro ("--- dia "++ show dias ++ "---")
            hClose ficheiro

            printSalas6 salas lotacao (x:xs) --x:xs é suporte2

            conteudoSuporte2 <- readFile' "Suporte2.txt"
            escalonamento6 (dias+1) diasMax salas lotacao (lines conteudoSuporte2)
  
printSalas6 :: Int -> [Int] -> [String] -> IO()
printSalas6 salas (y:ys) [] = return()
printSalas6 0 (y:ys) (x:xs) = return()
printSalas6 salas [] [] = return()
printSalas6 salas [] (x:xs) = return()
printSalas6 salas (y:ys) (x:xs) = do

    --lotacao atual é y da sala
    
     
    suporte <- readFile' "Suporte.txt" -- numero de alunos na turma

    let disciplina = unwords (tail(words x))


    --numero de alunos na turma - lotacao da sala
    let alunosTurma = head(words suporte)
    let alunosTurmaint = read alunosTurma :: Int
    let alunosRestantes = alunosTurmaint - y 
    if alunosTurmaint > y
        then do 
            ficheiro <- openFile "Tarefa6.txt" AppendMode
            hPutStrLn ficheiro ("Sala "++ show salas ++ " "++ show y ++"/"++show y ++": "++ disciplina)
            hClose ficheiro
        else do 
            ficheiro <- openFile "Tarefa6.txt" AppendMode
            hPutStrLn ficheiro ("Sala "++ show salas ++ " "++ show alunosTurmaint ++"/"++show y ++": "++ disciplina)
            hClose ficheiro
    
    if alunosRestantes > 0 
        then do 
            ficheirosuporte <- openFile "Suporte.txt" WriteMode
            hPutStrLn ficheirosuporte (show alunosRestantes ++ " " ++ disciplina)
            hClose ficheirosuporte
            printSalas6 (salas-1) ys (x:xs)
            return()
        else do 
            if null xs 
                then do 
                ficheiro2 <- openFile "Suporte2.txt" WriteMode -- clear file
                hClose ficheiro2 
                escreverFicheiro xs
                printSalas6 (salas-1) ys xs
                return()    
                else do 
                ficheirosuporte <- openFile "Suporte.txt" WriteMode
                hPutStrLn ficheirosuporte (head xs)  --escrever primeira linha do suporte2 no suporte
                hClose ficheirosuporte
                ficheiro2 <- openFile "Suporte2.txt" WriteMode -- clear file
                hClose ficheiro2 
                escreverFicheiro xs
                printSalas6 (salas-1) ys xs
                return()    

escreverFicheiro :: [String] -> IO()
escreverFicheiro [] = return()
escreverFicheiro (x:xs)= do 
            ficheiro2 <- openFile "Suporte2.txt" AppendMode
            hPutStrLn ficheiro2 x
            hClose ficheiro2 
            escreverFicheiro xs
    
---funcoes tarefa4---
escalonamento4 :: [String]-> [String]-> [String]->Int -> Int -> [String] -> IO() 
escalonamento4 disciplina inscricao alunos dias salas [] = return()
escalonamento4 disciplina inscricao alunos dias salas (x:xs) = do

    ficheiro <- openFile "Suporte.txt" WriteMode
    hClose ficheiro

    ficheiro <- openFile "Tarefa1.txt" AppendMode
    hPutStrLn ficheiro ("--- dia "++ show dias ++ "---")
    hClose ficheiro

    printSalas4 disciplina inscricao alunos salas (x:xs)

    ficheiroSuporte <- readFile "Suporte.txt"
    let tamanhoInicial = length (lines ficheiroSuporte) 
    let tamanhoFinal = length (remDup(lines ficheiroSuporte)) 
    let incompativeis = tamanhoInicial - tamanhoFinal

    ficheiro <- openFile "Tarefa1.txt" AppendMode
    hPutStrLn ficheiro ("Numero de imcompatibilidades: " ++ show incompativeis)
    hClose ficheiro
    
    escalonamento4 disciplina inscricao alunos (dias+1) salas (funcTail salas (x:xs))
  
printSalas4 :: [String]-> [String]-> [String]-> Int -> [String] -> IO()
printSalas4 disciplina inscricao alunos salas [] = return()
printSalas4 disciplina inscricao alunos 0 (x:xs) = return()
printSalas4 disciplina inscricao alunos salas (x:xs) = do

    let disciplinas = unwords (tail (tail(words x)))

    ficheiro <- openFile "Tarefa1.txt" AppendMode
    hPutStrLn ficheiro ("Sala "++ show salas ++ ": "++ disciplinas)
    hClose ficheiro

    encontrarNome disciplinas disciplina inscricao alunos
    printSalas4 disciplina inscricao alunos (salas-1) xs

--Funcoes tarefa 3--

remDup :: Eq a => [a] -> [a]
remDup [] = []
remDup (x:xs)
  | x `elem` xs = x : remDup (delete x xs)
  | otherwise = x : remDup xs

tarefa31 :: IO()
tarefa31 = do
    ficheiro <- openFile "Suporte.txt" WriteMode
    hClose ficheiro
    conteudoDisciplina <- readFile "ucs.txt"
    conteudoInscricao <- readFile "inscricoes.txt"
    conteudoAlunos <- readFile "listaalunos.txt"
    putStrLn "indique o nome da disciplina"
    disciplina <- getLine
    encontrarNome disciplina (lines conteudoDisciplina) (lines conteudoInscricao) (lines conteudoAlunos)
    putStrLn "indique o nome da outra disciplina"
    disciplina <- getLine
    encontrarNome disciplina (lines conteudoDisciplina) (lines conteudoInscricao) (lines conteudoAlunos)
    
encontrarNome:: String -> [String] -> [String] -> [String]-> IO() --descobre o numero da disciplina a partir do nome
encontrarNome x [] y z = return()
encontrarNome input (linha:linhas) conteudo_insc conteudo_alunos = do
    let numero = head (words linha)
    if input == unwords (tail(tail(words linha)))
        then descobrirAlxxx numero conteudo_insc conteudo_alunos --funcao usada na tarefa 1, com o numero descobre o al
        else return()
    encontrarNome input linhas conteudo_insc conteudo_alunos  

descobrirAlxxx:: String -> [String] -> [String]-> IO() --descobre o al a partir do numero da disciplina
descobrirAlxxx numero [] conteudo_alunos = return()
descobrirAlxxx numero (linha:linhas) conteudo_alunos = do
    
    let numero_al = head (words linha) -- al do aluno
    if last (words linha) == numero
        then do 
            ficheiro <- openFile "Suporte.txt" AppendMode
            hPutStrLn ficheiro (head (words linha))--tranforma al em nome 
            hClose ficheiro
        else return ()
    descobrirAlxxx numero linhas conteudo_alunos
    
    
--Funcoes tarefa 1
escalonamento :: Int -> Int -> [String] -> IO() 
escalonamento dias salas [] = return()
escalonamento dias salas (x:xs) = do
    ficheiro <- openFile "Tarefa1.txt" AppendMode
    hPutStrLn ficheiro ("--- dia "++ show dias ++ "---")
    hClose ficheiro
    printSalas salas (x:xs)
    escalonamento (dias+1) salas (funcTail salas (x:xs))

funcTail :: Int -> [String] -> [String]
funcTail salas [] = []
funcTail 0 string = string
funcTail salas (x:xs) = do
    funcTail (salas-1) xs
    
printSalas :: Int -> [String] -> IO()
printSalas salas [] = return()
printSalas 0 (x:xs) = return()
printSalas salas (x:xs) = do
    ficheiro <- openFile "Tarefa1.txt" AppendMode
    hPutStrLn ficheiro ("Sala "++ show salas ++ ": "++ unwords (tail (tail(words x))))
    hClose ficheiro
    printSalas (salas-1) xs


    

    
