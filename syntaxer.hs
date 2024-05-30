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
    | Variable String
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
    | elem x ['0'..'9'] = let (numero, resto) = span (\y -> elem y ['0'..'9'] || y `elem` ".eE-") (x:xs)
                              parseResult = if '.' `elem` numero || 'e' `elem` numero || 'E' `elem` numero
                                                then readFloating numero
                                                else readInteger numero
                          in case parseResult of
                                Right token -> token : analizadorLexico resto
                                Left errMsg -> Error errMsg : analizadorLexico resto
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
    | StatementNode SyntaxTree
    | TypeNode String
    | VariableNode String
    | ExpressionNode SyntaxTree
    | TermNode SyntaxTree
    | FactorNode SyntaxTree
    | NumberNode String
    | CommentNode String
    | ErrorNode String
    deriving (Show)

-- Estado del parser
data ParserState = ParserState [Token] deriving (Show)

-- Funciones del parser
parseProgram :: ParserState -> (SyntaxTree, ParserState)
parseProgram (ParserState (Variable "Programa" : Parentesis_abre '{' : tokens)) =
    let (mainNode, newState) = parseMain (ParserState tokens)
    in case newState of
        ParserState (Parentesis_cierra '}' : rest) -> (ProgramNode mainNode, ParserState rest)
        _ -> (ErrorNode "Expected closing '}' for Programa", newState)
parseProgram state = (ErrorNode "Expected 'Programa' at the beginning", state)

parseMain :: ParserState -> (SyntaxTree, ParserState)
parseMain (ParserState (Variable "principal" : Parentesis_abre '(' : Parentesis_cierra ')' : Parentesis_abre '{' : tokens)) =
    let (statements, newState) = parseStatements (ParserState tokens)
    in case newState of
        ParserState (Parentesis_cierra '}' : rest) -> (MainNode statements, ParserState rest)
        _ -> (ErrorNode "Expected closing '}' for principal", newState)
parseMain state = (ErrorNode "Expected 'principal()' at the beginning of main block", state)

parseStatements :: ParserState -> ([SyntaxTree], ParserState)
parseStatements state =
    let (statement, newState) = parseStatement state
    in case statement of
        ErrorNode _ -> ([], state)  -- Stop parsing if an error is encountered
        _ -> let (moreStatements, finalState) = parseStatements newState
             in (statement : moreStatements, finalState)

parseStatement :: ParserState -> (SyntaxTree, ParserState)
parseStatement state =
    case state of
        ParserState (Variable varType : Variable varName : Asignacion '=' : tokens) ->
            let (expr, newState) = parseExpression (ParserState tokens)
            in case newState of
                ParserState (Parentesis_abre ';' : rest) -> (StatementNode (TypeNode varType), ParserState rest)
                _ -> (ErrorNode "Expected ';' after assignment", newState)
        ParserState (Comentario comment : tokens) -> (CommentNode comment, ParserState tokens)
        _ -> (ErrorNode "Invalid statement", state)

parseExpression :: ParserState -> (SyntaxTree, ParserState)
parseExpression state =
    let (termNode, newState) = parseTerm state
    in parseExpression' termNode newState

parseExpression' :: SyntaxTree -> ParserState -> (SyntaxTree, ParserState)
parseExpression' leftTerm (ParserState (Suma '+' : tokens)) =
    let (rightTerm, newState) = parseTerm (ParserState tokens)
    in parseExpression' (ExpressionNode (TermNode leftTerm)) newState
parseExpression' leftTerm (ParserState (Resta '-' : tokens)) =
    let (rightTerm, newState) = parseTerm (ParserState tokens)
    in parseExpression' (ExpressionNode (TermNode leftTerm)) newState
parseExpression' leftTerm state = (leftTerm, state)

parseTerm :: ParserState -> (SyntaxTree, ParserState)
parseTerm state =
    let (factorNode, newState) = parseFactor state
    in parseTerm' factorNode newState

parseTerm' :: SyntaxTree -> ParserState -> (SyntaxTree, ParserState)
parseTerm' leftFactor (ParserState (Multiplicacion '*' : tokens)) =
    let (rightFactor, newState) = parseFactor (ParserState tokens)
    in parseTerm' (TermNode leftFactor) newState
parseTerm' leftFactor (ParserState (Division '/' : tokens)) =
    let (rightFactor, newState) = parseFactor (ParserState tokens)
    in parseTerm' (TermNode leftFactor) newState
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