-- Quiero hacer un analizador léxico donde recibo un archivo .txt y lo convierto en una lista de tokens. 
-- Los tokens son:
-- 1. Enteros
-- 2. Flotantes (reales, con punto o exponencial con e o E)
-- 3. Operadores (= asignacion, + suma, - resta, * multiplicacion, / division, ^ potencia)
-- 4. Parentesis (abre y cierra)
-- 5. Identificadores (letras seguidas de letras o numeros o _)
-- 6. Comentarios (empiezan con // y terminan con salto de linea)

-- Ejemplo de archivo .txt:
-- 1 + 2
-- 3.14 * 2
-- x = 3
-- // Comentario
-- 2^3

-- Ejemplo de lista de tokens:
-- [Entero 1, Operador "+", Entero 2, Entero 3, Punto, Entero 14, Operador "*", Entero 2, Identificador "x", Operador "=", Entero 3, Comentario "// Comentario", Entero 2, Operador "^", Entero 3]


-- Definición de tokens
data Token
    = Entero Int
    | Real Float
    | Suma Char
    | Resta Char
    | Multiplicacion Char
    | Division Char
    | Potencia Char
    | Asignacion Char
    | Parentesis_abre Char
    | Parentesis_cierra Char
    | Variable String
    | Comentario String
    deriving (Eq)

-- Función que recibe un archivo .txt y devuelve una lista de tokens
analizadorLexico :: String -> [Token]
analizadorLexico [] = []

-- Si el primer caracter es un espacio, lo ignoro y sigo con el resto del archivo
analizadorLexico (' ':xs) = analizadorLexico xs

-- Si un entero sea negativo o no, tiene un punto, y en algunos casos una e o E, lo convierto en un flotante y sigo con el resto del archivo
analizadorLexico ('-':x:xs) | elem x ['0'..'9'] = (Real (read ('-':x:takeWhile (\y -> elem y ['0'..'9'] || y == '.' ) xs))):(analizadorLexico (dropWhile (\y -> elem y ['0'..'9'] || y == '.') xs))
analizadorLexico (x:xs) | elem x ['0'..'9'] = (Real (read (x:takeWhile (\y -> elem y ['0'..'9'] || y == '.' || y == 'e' || y == 'E' || y == '-') xs))):(analizadorLexico (dropWhile (\y -> elem y ['0'..'9'] || y == '.' || y == 'e' || y == 'E' || y == '-') xs))

-- Si el primer caracter es un digito, lo convierto en un entero y sigo con el resto del archivo
analizadorLexico (x:xs) | elem x ['0'..'9'] = (Entero (read (x:takeWhile (\y -> elem y ['0'..'9']) xs))):(analizadorLexico (dropWhile (\y -> elem y ['0'..'9'] || y == '.') xs))

-- Si el primer caracter es una letra, lo convierto en un identificador y sigo con el resto del archivo
analizadorLexico (x:xs) | elem x ['a'..'z'] || elem x ['A'..'Z'] = (Variable (x:takeWhile (\y -> elem y ['a'..'z'] || elem y ['A'..'Z'] || elem y ['0'..'9'] || y == '_') xs)):(analizadorLexico (dropWhile (\y -> elem y ['a'..'z'] || elem y ['A'..'Z'] || elem y ['0'..'9'] || y == '_') xs))

-- Si el primer caracter es una barra, verifico si el siguiente caracter es una barra para saber si es un comentario
analizadorLexico ('/':'/':xs) = (Comentario ('/':'/':takeWhile (\y -> y /= '\n') xs)):(analizadorLexico (dropWhile (\y -> y /= '\n') xs))

-- Si el primer caracter es un operador, lo agrego a la lista de tokens y sigo con el resto del archivo
analizadorLexico (x:xs) | x == '+' = (Suma x):(analizadorLexico xs)
analizadorLexico (x:xs) | x == '-' = (Resta x):(analizadorLexico xs)
analizadorLexico (x:xs) | x == '*' = (Multiplicacion x):(analizadorLexico xs)
analizadorLexico (x:xs) | x == '/' = (Division x):(analizadorLexico xs)
analizadorLexico (x:xs) | x == '^' = (Potencia x):(analizadorLexico xs)
analizadorLexico (x:xs) | x == '=' = (Asignacion x):(analizadorLexico xs)

-- Si el primer caracter es un parentesis, lo agrego a la lista de tokens y sigo con el resto del archivo
analizadorLexico (x:xs) | x == '(' = (Parentesis_abre x):(analizadorLexico xs)
analizadorLexico (x:xs) | x == ')' = (Parentesis_cierra x):(analizadorLexico xs)

-- Si el primer caracter no es ninguno de los anteriores, lo ignoro y sigo con el resto del archivo
analizadorLexico (x:xs) = analizadorLexico xs


-- queremos imprimir la lista de tokens en formato de tabla, para eso creamos una instancia de la clase Show para Token
instance Show Token where
    show (Entero x) = show x ++ "\t | \t Entero" 
    show (Real x) = show x ++ "\t | \t Real"
    show (Suma x) = [x] ++ "\t | \t Suma"
    show (Resta x) = [x] ++ "\t | \t Resta"
    show (Multiplicacion x) = [x] ++ "\t | \t Multiplicacion"
    show (Division x) = [x] ++ "\t | \t Division"
    show (Potencia x) = [x] ++ "\t | \t Potencia"
    show (Asignacion x) = [x] ++ "\t | \t Asignacion"
    show (Parentesis_abre x) = [x] ++ "\t | \t Parentesis que abre"
    show (Parentesis_cierra x) = [x] ++ "\t | \t Parentesis que cierra"
    show (Variable x) = x ++ "\t | \t Variable"
    show (Comentario x) = x ++ "\t | \t Comentario"

-- main
main = do
    -- Solicitamos el archivo
    putStrLn "Ingrese el nombre del archivo .txt"
    file <- getLine
    -- Leemos el archivo
    archivo <- readFile file
    -- Imprimimos la lista de tokens
    putStr "\n Token \t | \t Tipo \n"
    putStr "------------------------\n"
    putStr $ unlines $ map show (analizadorLexico archivo)