import Data.List (intercalate)

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
    | Llave_abre Char
    | Llave_cierra Char
    | Punto_y_coma Char
    | Variable String
    | Programa
    | Principal
    | Comentario String
    | Error String
    deriving (Eq, Show)

analizadorLexico :: String -> [Token]
analizadorLexico [] = []
analizadorLexico (' ':xs) = analizadorLexico xs
analizadorLexico ('\n':xs) = analizadorLexico xs
analizadorLexico ('\t':xs) = analizadorLexico xs
analizadorLexico ('/':'/':xs) = (Comentario ('/':'/':takeWhile (/= '\n') xs)) : analizadorLexico (dropWhile (/= '\n') xs)
analizadorLexico (x:xs)
    | elem x ['0'..'9'] = let (numero, resto) = span (\y -> elem y ['0'..'9'] || elem y ".eE-") (x:xs)
                              parseResult = if elem '.' numero || elem 'e' numero || elem 'E' numero
                                                then readFloating numero
                                                else readInteger numero
                          in case parseResult of
                                Right token -> token : analizadorLexico resto
                                Left errMsg -> Error errMsg : analizadorLexico resto
    | elem x ['a'..'z'] || elem x ['A'..'Z'] = let (identificador, resto) = span (\y -> elem y ['a'..'z'] || elem y ['A'..'Z'] || elem y ['0'..'9'] || y == '_') (x:xs)
                                                in case identificador of
                                                    "Programa" -> Programa : analizadorLexico resto
                                                    "principal" -> Principal : analizadorLexico resto
                                                    _ -> Variable identificador : analizadorLexico resto
    | otherwise = case x of
                    '+' -> Suma x : analizadorLexico xs
                    '-' -> Resta x : analizadorLexico xs
                    '*' -> Multiplicacion x : analizadorLexico xs
                    '/' -> Division x : analizadorLexico xs
                    '^' -> Potencia x : analizadorLexico xs
                    '=' -> Asignacion x : analizadorLexico xs
                    '(' -> Parentesis_abre x : analizadorLexico xs
                    ')' -> Parentesis_cierra x : analizadorLexico xs
                    '{' -> Llave_abre x : analizadorLexico xs
                    '}' -> Llave_cierra x : analizadorLexico xs
                    ';' -> Punto_y_coma x : analizadorLexico xs
                    '@' -> Error "Simbolo no permitido: @" : analizadorLexico xs
                    '#' -> Error "Simbolo no permitido: #" : analizadorLexico xs
                    '$' -> Error "Simbolo no permitido: $" : analizadorLexico xs
                    '%' -> Error "Simbolo no permitido: %" : analizadorLexico xs
                    '!' -> Error "Simbolo no permitido: !" : analizadorLexico xs
                    _   -> Error ("Simbolo no reconocido: " ++ [x]) : analizadorLexico xs

readFloating :: String -> Either String Token
readFloating str = case reads str :: [(Float, String)] of
                    [(val, "")] -> Right (Real val)
                    _           -> Left $ "Error de sintaxis: " ++ str

readInteger :: String -> Either String Token
readInteger str = case reads str :: [(Int, String)] of
                    [(val, "")] -> Right (Entero val)
                    _           -> Left $ "Error de sintaxis: " ++ str

-- Definición de la estructura del árbol de sintaxis
data SyntaxTree
    = ProgramNode SyntaxTree
    | MainNode [SyntaxTree]
    | StatementNode SyntaxTree SyntaxTree
    | AssignmentNode SyntaxTree SyntaxTree SyntaxTree
    | TypeNode String
    | VariableNode String
    | ExpressionNode SyntaxTree SyntaxTree SyntaxTree
    | TermNode SyntaxTree SyntaxTree
    | FactorNode SyntaxTree
    | OperatorNode Char
    | NumberNode String
    | CommentNode String
    | ErrorNode String
    deriving (Show)

-- Estado del parser
data ParserState = ParserState [Token] deriving (Show)

-- Funciones del parser
parseProgram :: ParserState -> (SyntaxTree, ParserState)
parseProgram (ParserState (Programa : Llave_abre '{' : tokens)) =
    let (mainNode, newState) = parseMain (ParserState tokens)
    in case newState of
        ParserState (Llave_cierra '}':rest) -> (ProgramNode mainNode, ParserState rest)
        _ -> (ErrorNode "Expected closing '}' for Programa", newState)
parseProgram state = (ErrorNode "Expected 'Programa' at the beginning", state)

parseMain :: ParserState -> (SyntaxTree, ParserState)
parseMain (ParserState (Principal : Llave_abre '{' : tokens)) =
    let (statements, newState) = parseStatements (ParserState tokens)
    in case newState of
        ParserState (Llave_cierra '}':rest) -> (MainNode statements, ParserState rest)
        _ -> (ErrorNode "Expected closing '}' for principal", newState)
parseMain state = (ErrorNode "Expected 'principal { ... }' at the beginning of main block", state)

parseStatements :: ParserState -> ([SyntaxTree], ParserState)
parseStatements state@(ParserState (Llave_cierra '}':_)) = ([], state)
parseStatements state@(ParserState []) = ([], state)
parseStatements state =
    let (statement, newState) = parseStatement state
    in case statement of
        ErrorNode _ -> ([statement], newState)  -- Detenerse y reportar el error
        _ -> let (statements, finalState) = parseStatements newState
             in (statement : statements, finalState)

parseStatement :: ParserState -> (SyntaxTree, ParserState)
parseStatement (ParserState (Variable varType : Variable varName : Asignacion '=' : tokens)) =
    let (expr, newState) = parseExpression (ParserState tokens)
    in case newState of
        ParserState (Punto_y_coma ';' : rest) -> (StatementNode (AssignmentNode (VariableNode varName) (OperatorNode '=') expr) (TypeNode varType), ParserState rest)
        _ -> (ErrorNode $ "Expected ';' after assignment to variable " ++ varName, newState)
parseStatement (ParserState (Comentario comment : tokens)) = (CommentNode comment, ParserState tokens)
parseStatement (ParserState []) = (ErrorNode "Unexpected end of input", ParserState [])
parseStatement state = (ErrorNode "Invalid statement", state)

parseExpression :: ParserState -> (SyntaxTree, ParserState)
parseExpression state =
    let (termNode, newState) = parseTerm state
    in parseExpression' termNode newState

parseExpression' :: SyntaxTree -> ParserState -> (SyntaxTree, ParserState)
parseExpression' leftTerm (ParserState (Suma '+' : tokens)) =
    let (rightTerm, newState) = parseTerm (ParserState tokens)
    in parseExpression' (ExpressionNode leftTerm (OperatorNode '+') rightTerm) newState
parseExpression' leftTerm (ParserState (Resta '-' : tokens)) =
    let (rightTerm, newState) = parseTerm (ParserState tokens)
    in parseExpression' (ExpressionNode leftTerm (OperatorNode '-') rightTerm) newState
parseExpression' leftTerm state = (leftTerm, state)

parseTerm :: ParserState -> (SyntaxTree, ParserState)
parseTerm state =
    let (factorNode, newState) = parseFactor state
    in parseTerm' factorNode newState

parseTerm' :: SyntaxTree -> ParserState -> (SyntaxTree, ParserState)
parseTerm' leftFactor (ParserState (Multiplicacion '*' : tokens)) =
    let (rightFactor, newState) = parseFactor (ParserState tokens)
    in parseTerm' (ExpressionNode leftFactor (OperatorNode '*') rightFactor) newState
parseTerm' leftFactor (ParserState (Division '/' : tokens)) =
    let (rightFactor, newState) = parseFactor (ParserState tokens)
    in parseTerm' (ExpressionNode leftFactor (OperatorNode '/') rightFactor) newState
parseTerm' leftFactor state = (leftFactor, state)

parseFactor :: ParserState -> (SyntaxTree, ParserState)
parseFactor (ParserState (Parentesis_abre '(' : tokens)) =
    let (exprNode, newState) = parseExpression (ParserState tokens)
    in case newState of
        ParserState (Parentesis_cierra ')' : rest) -> (FactorNode exprNode, ParserState rest)
        _ -> (ErrorNode "Expected closing ')'", newState)
parseFactor (ParserState (Entero n : tokens)) = (FactorNode (NumberNode (show n)), ParserState tokens)
parseFactor (ParserState (Real n : tokens)) = (FactorNode (NumberNode (show n)), ParserState tokens)
parseFactor (ParserState (Variable var : tokens)) = (FactorNode (VariableNode var), ParserState tokens)
parseFactor state = (ErrorNode "Invalid factor", state)

-- Función principal para ejecutar el parser
main :: IO ()
main = do
    putStrLn "Ingrese el nombre del archivo .txt"
    file <- getLine
    archivo <- readFile file
    let tokens = analizadorLexico archivo
    putStrLn "Tokens:"
    putStrLn $ unlines $ map show tokens
    let (parseTree, _) = parseProgram (ParserState tokens)
    putStrLn "Parse Tree:"
    print parseTree
