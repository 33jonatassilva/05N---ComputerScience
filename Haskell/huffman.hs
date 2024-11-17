import System.IO (readFile, writeFile)

-- Definindo o tipo da Árvore de Huffman
data HuffmanTree = Leaf Char Int | Node Int HuffmanTree HuffmanTree deriving Show

-- Função para contar a frequência de cada caractere manualmente
charFrequency :: String -> [(Char, Int)]
charFrequency [] = []
charFrequency (x:xs) = updateFreq x (charFrequency xs)
  where
    updateFreq c [] = [(c, 1)]
    updateFreq c ((ch, freq):rest)
      | c == ch   = (ch, freq + 1) : rest
      | otherwise = (ch, freq) : updateFreq c rest

-- Função para ordenar manualmente uma lista de tuplas com base no segundo elemento (frequência)
sortByFreq :: [(Char, Int)] -> [(Char, Int)]
sortByFreq [] = []
sortByFreq (x:xs) = insertByFreq x (sortByFreq xs)
  where
    insertByFreq y [] = [y]
    insertByFreq y (z:zs)
      | snd y <= snd z = y : z : zs
      | otherwise      = z : insertByFreq y zs

-- Função para obter a frequência de uma árvore de Huffman
getTreeFreq :: HuffmanTree -> Int
getTreeFreq (Leaf _ f)   = f
getTreeFreq (Node f _ _) = f

-- Função para ordenar a árvore de Huffman com base na frequência
sortByTreeFreq :: [HuffmanTree] -> [HuffmanTree]
sortByTreeFreq [] = []
sortByTreeFreq (x:xs) = insertByTreeFreq x (sortByTreeFreq xs)
  where
    insertByTreeFreq y [] = [y]
    insertByTreeFreq y (z:zs)
      | getTreeFreq y <= getTreeFreq z = y : z : zs
      | otherwise = z : insertByTreeFreq y zs

-- Função para construir a Árvore de Huffman manualmente
buildHuffmanTree :: [(Char, Int)] -> HuffmanTree
buildHuffmanTree freqs = buildTree (map (\(c, f) -> Leaf c f) freqs)
  where
    buildTree [t] = t
    buildTree ts  = buildTree (sortByTreeFreq (Node (getTreeFreq a + getTreeFreq b) a b : rest))
      where
        (a:b:rest) = sortByTreeFreq ts

-- Função para gerar o código de Huffman manualmente
huffmanCodes :: HuffmanTree -> [(Char, String)]
huffmanCodes tree = generateCodes tree ""
  where
    generateCodes (Leaf c _) code = [(c, code)]
    generateCodes (Node _ left right) code =
      generateCodes left (code ++ "0") ++ generateCodes right (code ++ "1")

-- Função para buscar um código de um caractere
findCode :: Char -> [(Char, String)] -> String
findCode _ [] = ""
findCode c ((ch, code):xs)
  | c == ch   = code
  | otherwise = findCode c xs

-- Função para codificar o texto usando os códigos de Huffman manualmente
encodeText :: [(Char, String)] -> String -> String
encodeText _ [] = []
encodeText codes (x:xs) = findCode x codes ++ encodeText codes xs

-- Função principal para ler o arquivo de entrada, processar e escrever o arquivo de saída
huffmanEncodeFile :: FilePath -> FilePath -> IO ()
huffmanEncodeFile inputFile outputFile = do
  inputText <- readFile inputFile
  let filteredText = filter (`elem` (['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'])) inputText
  let frequencies = sortByFreq (charFrequency filteredText)
  let huffmanTree = buildHuffmanTree frequencies
  let codes = huffmanCodes huffmanTree
  let encodedText = encodeText codes filteredText
  writeFile outputFile encodedText
  putStrLn "Arquivo codificado com sucesso!"

-- Executa o programa, lendo do arquivo "in.txt" e escrevendo em "out.txt"
main :: IO ()
main = huffmanEncodeFile "C:\\Haskell\\Haskell\\HuffmanProject\\in.txt" "C:\\Haskell\\HuffmanProject\\out.txt"
