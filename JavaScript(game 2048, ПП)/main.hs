module Main where
import Graphics.UI.WX
import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Token
import Text.Parsec.Expr
import Control.Monad.State
import qualified Data.Map

main :: IO()
main = start gui

gui :: IO ()
gui = do
	frame <- frameFixed [ text := "Lab Work 3 - Calc", clientSize := (Size 377 200) ]
	panel <- panel frame []
	textEntry <- textEntry panel [text := ""]
	bclearall <- button panel [text := "C", on command := set textEntry [text := ""]]
	button0 <- button panel [text := "0", on command := set textEntry [text :~ (++"0")]]
	button1 <- button panel [text := "1", on command := set textEntry [text :~ (++"1")]]
	button2 <- button panel [text := "2", on command := set textEntry [text :~ (++"2")]]
	button3 <- button panel [text := "3", on command := set textEntry [text :~ (++"3")]]
	button4 <- button panel [text := "4", on command := set textEntry [text :~ (++"4")]]
	button5 <- button panel [text := "5", on command := set textEntry [text :~ (++"5")]]
	button6 <- button panel [text := "6", on command := set textEntry [text :~ (++"6")]]
	button7 <- button panel [text := "7", on command := set textEntry [text :~ (++"7")]]
	button8 <- button panel [text := "8", on command := set textEntry [text :~ (++"8")]]
	button9 <- button panel [text := "9", on command := set textEntry [text :~ (++"9")]]
	buttonAdd <- button panel [text := "+", on command := set textEntry [text :~ (++"+")]]
	buttonSub <- button panel [text := "-", on command := set textEntry [text :~ (++"-")]]
	buttonMult <- button panel [text := "*", on command := set textEntry [text :~ (++"*")]]
	buttonDiv <- button panel [text := "/", on command := set textEntry [text :~ (++"/")]]
	buttonFraction <- button panel [text := ".", on command := set textEntry [text :~ (++".")]]
	buttonEqual <- button panel [text := "=", on command := setAnswer textEntry]
	set panel
		[ layout := margin 10 (column 5
			[
				row 5 [ hfill(widget textEntry), widget bclearall ],
				row 5 [ widget button7, widget button8, widget button9, widget buttonAdd ],
				row 5 [ widget button4, widget button5, widget button6, widget buttonSub ],
				row 5 [ widget button1, widget button2, widget button3, widget buttonMult ],
				row 5 [ widget buttonFraction, widget button0, widget buttonEqual, widget buttonDiv ]
			]
		)]
	return ()

isDigit (x:xs) = if (fromEnum x >= 48 && fromEnum x <= 57) then 0 else 1

setAnswer w = do
	val <- Graphics.UI.WX.get w text
	ans <- calc2 val
	set w [text := ans]

data Expr = Constant Double | Multiplication Expr Expr | Division Expr Expr
	| Addition Expr Expr | Subtraction Expr Expr | Negation Expr
	deriving(Show)
data Stmt = PrintStmt Expr | AssignStmt String Expr deriving(Show)

defaultValues = LanguageDef
	{
		commentStart = "/*",
		commentEnd = "*/",
		commentLine = "//",
		identStart = letter <|> char '_',
		identLetter = alphaNum <|> oneOf "_'",
		opStart = oneOf "+-/",
		opLetter = oneOf "+-/"
	}
lexer :: TokenParser()
lexer = makeTokenParser(defaultValues)

table =
	[
		[ Prefix (reservedOp lexer "-" >> return Negation)],
		[ Infix	(reservedOp lexer "*" >> return Multiplication) AssocLeft,
		  Infix	(reservedOp lexer "/" >> return Division) AssocLeft],
		[ Infix	(reservedOp lexer "+" >> return Addition) AssocLeft,
		  Infix	(reservedOp lexer "-" >> return Subtraction) AssocLeft]
	]
parseExpr = buildExpressionParser table parseTerm
parseTerm = parens lexer parseExpr <|> parseNumber
parsePrint = do
	expr <- parseExpr
	return (PrintStmt expr)
parseInput = do
	whiteSpace lexer
	s <- (try parseAssign <|> parsePrint)
	return s
parseAssign :: Parser Stmt
parseAssign = do
	ident <- identifier lexer
	reservedOp lexer "="
	expr <- parseExpr
	return (AssignStmt ident expr)
parseNumber :: Parser Expr
parseNumber = do
	val <- naturalOrFloat lexer
	case val of
		Left i -> return (Constant (fromIntegral i))
		Right n -> return (Constant n)

interpretExpr :: Expr -> StateT (Data.Map.Map String Expr) IO Double
interpretExpr (Constant n) = return (n)
interpretExpr (Multiplication e1 e2) = do
	v1 <- interpretExpr e1
	v2 <- interpretExpr e2
	return (v1 * v2)
interpretExpr (Division e1 e2) = do
	v1 <- interpretExpr e1
	v2 <- interpretExpr e2
	return (v1 / v2)
interpretExpr (Addition e1 e2) = do
	v1 <- interpretExpr e1
	v2 <- interpretExpr e2
	return (v1 + v2)
interpretExpr (Subtraction e1 e2) = do
	v1 <- interpretExpr e1
	v2 <- interpretExpr e2
	return (v1 - v2)
interpretExpr (Negation e1) = do
	v1 <- interpretExpr e1
	return (negate v1)

interpretStmt (PrintStmt expr) = do
	n <- interpretExpr expr
	liftIO(print n)
interpretStmt (AssignStmt ident expr) = do
	n <- interpretExpr expr
	modify(Data.Map.insert ident (Constant n))

inter (PrintStmt expr) = do
	n <- interpretExpr expr
	return n

calc1 s =
	case (parse parseInput "" s) of
		Right n -> evalStateT (inter n) Data.Map.empty
calc2 s = do
	val <- calc1 s
	return (show val)
calc3 s = case (parse parseInput "" s) of Right n -> interpretStmt n
calc4 s = mapM_ calc3 (lines s)
