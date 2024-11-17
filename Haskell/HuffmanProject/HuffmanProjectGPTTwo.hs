import System.IO
import Data.List (sortBy, group, sort)
import Data.Ord (comparing)
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import Data.Char (isAlphaNum)

-- Definição da árvore de Huffman
data HuffmanTree = Leaf Char Int
                 | Node Int HuffmanTree HuffmanTree
                 deriving (Show, Eq)

-- Função para obter a frequência de uma árvore
frequency :: HuffmanTree -> Int
frequency (Leaf _ freq)     = freq
frequency (Node freq _ _)   = freq

-- Função para construir a tabela de frequência
buildFrequencyTable :: String -> Map Char Int
buildFrequencyTable text = Map.fromListWith (+) [(c, 1) | c <- text]

-- Função para construir a árvore de Huffman a partir da tabela de frequência
buildHuffmanTree :: Map Char Int -> HuffmanTree
buildHuffmanTree freqMap = buildTree initialTrees
  where
    initialTrees = sortBy (comparing frequency) [Leaf c f | (c, f) <- Map.toList freqMap]
    
    buildTree [t] = t
    buildTree ts = buildTree newTrees
      where
        -- Pega as duas árvores com menor frequência
        (t1:t2:rest) = ts
        combined = Node (frequency t1 + frequency t2) t1 t2
        -- Insere a nova árvore na lista e ordena novamente
        newTrees = insertByFrequency combined rest

    -- Função auxiliar para inserir mantendo a ordem
    insertByFrequency t [] = [t]
    insertByFrequency t (x:xs)
      | frequency t <= frequency x = t : x : xs
      | otherwise = x : insertByFrequency t xs

-- Função para gerar a tabela de códigos a partir da árvore de Huffman
generateCodes :: HuffmanTree -> Map Char String
generateCodes tree =
    if isSingleLeaf tree
        then case tree of
               Leaf c _ -> Map.singleton c "0"  -- Atribui "0" se houver apenas um caractere
               _        -> Map.empty
        else generateCodes' tree ""
  where
    isSingleLeaf (Leaf _ _) = True
    isSingleLeaf _           = False

    generateCodes' (Leaf c _) code = Map.singleton c code
    generateCodes' (Node _ left right) code =
        Map.union (generateCodes' left (code ++ "0"))
                  (generateCodes' right (code ++ "1"))

-- Função para codificar o texto usando a tabela de códigos
encodeText :: Map Char String -> String -> String
encodeText codeMap text = concatMap (\c -> Map.findWithDefault "" c codeMap) text

main :: IO ()
main = do
    -- Leitura do arquivo de entrada
    content <- readFile "in.txt"
    
    -- Filtragem para manter apenas caracteres alfanuméricos
    let filteredContent = filter isAlphaNum content
    
    -- Construção da tabela de frequência
    let freqTable = buildFrequencyTable filteredContent
    
    -- Verificação de conteúdo vazio
    if Map.null freqTable
        then writeFile "out.txt" ""
        else do
            -- Construção da árvore de Huffman
            let huffmanTree = buildHuffmanTree freqTable
            
            -- Geração da tabela de códigos
            let codeMap = generateCodes huffmanTree
            
            -- Codificação do texto
            let encodedText = encodeText codeMap filteredContent
            
            -- Escrita no arquivo de saída
            writeFile "out.txt" encodedText
