

--Jônatas de Brito Silva               10403674
--Felipe Gyotoku Koike                 10409640
--Bruno Viana Tripoli Barbosa          10409547



-- HuffmanEncoder.hs

-- Definição da árvore de Huffman
data HuffmanTree = Leaf Char Int
                | Node Int HuffmanTree HuffmanTree
                deriving (Show, Eq)

-- Função para obter a frequência de uma árvore
frequency :: HuffmanTree -> Int
frequency (Leaf _ freq)   = freq
frequency (Node freq _ _) = freq

-- Função para obter o menor caractere em uma árvore de Huffman
minChar :: HuffmanTree -> Char
minChar (Leaf c _)     = c
minChar (Node _ left right) = min (minChar left) (minChar right)

-- Função para verificar se um caractere é alfanumérico
isAlphaNumCustom :: Char -> Bool
isAlphaNumCustom c = (c >= 'A' && c <= 'Z') ||
                     (c >= 'a' && c <= 'z') ||
                     (c >= '0' && c <= '9')

-- Função para contar as frequências dos caracteres
buildFrequencyTable :: String -> [(Char, Int)]
buildFrequencyTable [] = []
buildFrequencyTable (c:cs)
    | not (isAlphaNumCustom c) = buildFrequencyTable cs
    | otherwise = let (count, rest) = countChar c cs
                  in (c, count) : buildFrequencyTable rest
  where
    -- Conta a quantidade de vezes que 'ch' aparece em 's' e retorna o restante da lista
    countChar :: Char -> String -> (Int, String)
    countChar ch s = countChar' ch s 1 []
    
    countChar' :: Char -> String -> Int -> String -> (Int, String)
    countChar' _ [] cnt acc = (cnt, reverse acc)
    countChar' ch (x:xs) cnt acc
        | x == ch    = countChar' ch xs (cnt + 1) acc
        | otherwise  = countChar' ch xs cnt (x:acc)

-- Função para ordenar a lista de árvores pela frequência e, em caso de empate, pelo menor caractere
sortHuffmanTrees :: [HuffmanTree] -> [HuffmanTree]
sortHuffmanTrees [] = []
sortHuffmanTrees (x:xs) = insertHuffmanTree x (sortHuffmanTrees xs)

-- Função auxiliar para inserir uma árvore na lista ordenada
insertHuffmanTree :: HuffmanTree -> [HuffmanTree] -> [HuffmanTree]
insertHuffmanTree t [] = [t]
insertHuffmanTree t (x:xs)
    | frequency t < frequency x = t : x : xs
    | frequency t == frequency x =
        if minChar t < minChar x
            then t : x : xs
            else x : insertHuffmanTree t xs
    | otherwise = x : insertHuffmanTree t xs

-- Função para construir a árvore de Huffman de forma segura usando pattern matching
buildHuffmanTree :: [HuffmanTree] -> HuffmanTree
buildHuffmanTree [] = error "buildHuffmanTree: Lista vazia. Não é possível construir uma árvore de Huffman sem elementos."
buildHuffmanTree [t] = t
buildHuffmanTree ts = buildHuffmanTree (insertHuffmanTree combined rest)
  where
    sortedTrees = sortHuffmanTrees ts
    -- Utiliza pattern matching para garantir que existem pelo menos dois elementos
    combined = case sortedTrees of
                 (t1:t2:restTrees) -> Node (frequency t1 + frequency t2) t1 t2
                 _ -> error "buildHuffmanTree: Lista com menos de dois elementos inesperada."
    
    rest = case sortedTrees of
             (_:_:restTrees) -> restTrees
             _ -> error "buildHuffmanTree: Lista com menos de dois elementos inesperada."

-- Função para gerar a tabela de códigos a partir da árvore de Huffman
generateCodes :: HuffmanTree -> [(Char, String)]
generateCodes tree = generateCodes' tree ""
  where
    generateCodes' :: HuffmanTree -> String -> [(Char, String)]
    generateCodes' (Leaf c _) code = [(c, code)]
    generateCodes' (Node _ left right) code =
        generateCodes' left (code ++ "0") ++ generateCodes' right (code ++ "1")

-- Função para codificar o texto usando a tabela de códigos
encodeText :: [(Char, String)] -> String -> String
encodeText _ [] = []
encodeText codes (c:cs) = 
    case lookup c codes of
        Just code -> code ++ encodeText codes cs
        Nothing   -> encodeText codes cs  -- Ignora caracteres não mapeados

-- Função para verificar se a árvore de Huffman possui apenas uma folha
singleLeaf :: HuffmanTree -> Bool
singleLeaf (Leaf _ _) = True
singleLeaf _           = False

-- Função para obter o único caractere de uma árvore com uma única folha
singleChar :: HuffmanTree -> Char
singleChar (Leaf c _) = c
singleChar _           = error "singleChar: Árvore não possui uma única folha."

-- Função principal
main :: IO ()
main = do
    -- Leitura do arquivo de entrada
    content <- readFile "in.txt"
    
    -- Construção da tabela de frequência
    let freqTable = buildFrequencyTable content
    
    -- Filtragem para manter apenas caracteres alfanuméricos
    let filteredContent = filter isAlphaNumCustom content
    
    -- Verificação de conteúdo vazio
    if null freqTable
        then writeFile "out.txt" ""
        else do
            -- Criação das folhas para a árvore de Huffman
            let leaves = map (\(c, f) -> Leaf c f) freqTable
            
            -- Construção da árvore de Huffman
            let huffmanTree = buildHuffmanTree leaves
            
            -- Geração da tabela de códigos
            let codes = if singleLeaf huffmanTree
                        then [(singleChar huffmanTree, "0")]
                        else generateCodes huffmanTree
            
            -- Codificação do texto
            let encoded = encodeText codes filteredContent
            
            -- Escrita no arquivo de saída
            writeFile "out.txt" encoded
