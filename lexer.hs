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
    | Error String  -- New token type for errors
    deriving (Eq)

analizadorLexico :: String -> [Token]
analizadorLexico [] = []
analizadorLexico (' ':xs) = analizadorLexico xs  -- Ignore spaces
analizadorLexico ('\n':xs) = analizadorLexico xs  -- Ignore newlines
analizadorLexico ('\t':xs) = analizadorLexico xs  -- Ignore tabs
analizadorLexico ('/':'/':xs) = (Comentario ('/':'/':takeWhile (/= '\n') xs)) : analizadorLexico (dropWhile (/= '\n') xs)  
analizadorLexico (x:xs)
    | elem x ['0'..'9'] = let (numero, resto) = span (\y -> elem y ['0'..'9'] || y `elem` ".eE-") (x:xs)
                              parseResult = if '.' `elem` numero || 'e' `elem` numero || 'E' `elem` numero
                                                then readFloating numero
                                                else readInteger numero
                          in case parseResult of
                                Right token -> token : analizadorLexico resto
                                Left errMsg -> Error errMsg : analizadorLexico resto  -- Handle error by creating an Error token
    | elem x ['a'..'z'] || elem x ['A'..'Z'] = let (identificador, resto) = span (\y -> elem y ['a'..'z'] || elem y ['A'..'Z'] || elem y ['0'..'9'] || y == '_') (x:xs)
                                                in (Variable identificador) : analizadorLexico resto  
    | otherwise = case x of
                    '+' -> Suma x : analizadorLexico xs
                    '-' -> Resta x : analizadorLexico xs
                    '*' -> Multiplicacion x : analizadorLexico xs
                    '/' -> Division x : analizadorLexico xs
                    '^' -> Potencia x : analizadorLexico xs
                    '=' -> Asignacion x : analizadorLexico xs
                    '(' -> Parentesis_abre x : analizadorLexico xs
                    ')' -> Parentesis_cierra x : analizadorLexico xs
                    '@' -> Error "Simbolo no permitido: @" : analizadorLexico xs  -- Handle invalid symbol
                    '#' -> Error "Simbolo no permitido: #" : analizadorLexico xs  -- Handle invalid symbol
                    '$' -> Error "Simbolo no permitido: $" : analizadorLexico xs  -- Handle invalid symbol
                    '%' -> Error "Simbolo no permitido: %" : analizadorLexico xs  -- Handle invalid symbol
                    '!' -> Error "Simbolo no permitido: !" : analizadorLexico xs  -- Handle invalid symbol
                    _   -> Error ("Simbolo no reconocido: " ++ [x]) : analizadorLexico xs  -- General case for any other invalid symbols

readFloating :: String -> Either String Token
readFloating str = case reads str :: [(Float, String)] of
                    [(val, "")] -> Right (Real val)
                    _           -> Left $ "Error de sintaxis: " ++ str

readInteger :: String -> Either String Token
readInteger str = case reads str :: [(Int, String)] of
                    [(val, "")] -> Right (Entero val)
                    _           -> Left $ "Error de sintaxis: " ++ str

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
    show (Error msg) = "ERROR: " ++ msg  -- Display error message

main :: IO ()
main = do
    putStrLn "Ingrese el nombre del archivo .txt"
    file <- getLine
    archivo <- readFile file
    putStr "\n Token \t | \t Tipo \n"
    putStr "------------------------\n"
    putStr $ unlines $ map show (analizadorLexico archivo)
