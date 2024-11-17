

-- primeiro programa em haskell


polinomio :: Integer -> Integer
polinomio x = x*x + 10*x + 2


-- calcula o quadrado de um inteiro

quadrado :: Integer -> Integer
quadrado n = n * n


-- soma

soma :: Float -> Float -> Float
soma a b = a + b

-- triplica

triplica :: Integer -> Integer
triplica n = n * 3

-- area 

area :: Float -> Float
area raio = raio * raio * pi

-- perimetro

perimetro :: Float -> Float
perimetro raio = 2*pi*raio


-- hipotenusa

hipotenusa :: Float -> Float -> Float
hipotenusa a b = sqrt (a*a + b*b)

-- diferenca de area

diferencaDeArea :: Float -> Float -> Float
diferencaDeArea r1 r2 = abs ((area r1) - (area r2))

-- maior


maior :: Float -> Float -> Float

maior a b = if a > b then a else b


-- fatorial

fatorial :: Integer -> Integer

fatorial n
    | n == 0 = 1
    | n > 0 = n * fatorial(n-1)

-- maiorg

maiorg :: Float -> Float -> Float

maiorg a b
    | a > b = a
    | b > a = b
    | otherwise = 0 

-- ehpar


ehpar :: Integer -> Bool
ehpar n
    | mod n 2 == 0 = True
    | otherwise = False


-- charcase

charcase :: Char -> String
charcase ch
    | ch >= 'a' && ch <= 'z' = "Minusculo"
    | ch >= 'A' && ch <= 'Z' = "Maiusculo"
    | otherwise = "Caractere Invalido"