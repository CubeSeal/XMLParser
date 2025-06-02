import Control.Applicative ((<|>), Alternative)
import GHC.Base (Alternative(..))
import Debug.Trace (trace, traceShow)

-- XML Data Structure
newtype Attribute = Attribute [String]
  deriving Show
newtype Tag = Tag {showTag :: (String, Attribute)}
  deriving Show
data XML = XML (Tag, [XML]) | Literal String
  deriving Show

newtype Parser a = Parser {runParser :: String -> Maybe (a, String)}

instance Functor Parser where
  fmap :: (a -> b) -> Parser a -> Parser b
  fmap f (Parser p) = Parser $ \input -> do
    (parsed, input') <- p input
    Just (f parsed, input')

instance Applicative Parser where
  pure :: a -> Parser a
  pure x = Parser $ \input -> Just (x, input)
  (<*>) :: Parser (a -> b) -> Parser a -> Parser b
  (Parser p1) <*> (Parser p2) = Parser $ \input -> do
    (f, input') <- p1 input
    (x, input'') <- p2 input'
    Just (f x, input'')

instance Alternative Parser where
  empty :: Parser a
  empty = Parser $ const Nothing
  (<|>) :: Parser a -> Parser a -> Parser a
  (Parser p1) <|> (Parser p2) = Parser $ \input -> p1 input<|> p2 input

strPred :: (Char -> Bool) -> Parser String
strPred p = Parser f
  where
    f [] = Nothing
    f input = catch $ span p input
    catch ("", _) = Nothing
    catch tpl = Just tpl

charP :: Char -> Parser Char
charP p = Parser f
  where
    f (x:xs)
      | x == p = Just (x, xs)
      | otherwise = Nothing
    f [] = Nothing

strP :: String -> Parser String
strP = traverse charP

parseOpeningTag :: Parser Tag
parseOpeningTag = charP '<' *> Parser g <* charP '>'
  where
    g ('/':xs) = Nothing
    g xs = do
      (tagInfo, input') <- runParser (strPred (/= '>')) xs
      let ls = words tagInfo
          tag = head ls
          attributes = Attribute $ tail ls
      Just (Tag (tag, attributes), input')

parseClosingTag :: Tag -> Parser String
parseClosingTag (Tag (str, _)) = charP '<' *> charP '/' *> strP str <* charP '>'

singleFieldParser :: Parser XML
singleFieldParser = Parser $ \input -> do
  (tag, input') <- runParser parseOpeningTag input
  (xml, input'') <- runParser xmlParser input'
  (_, input''') <- runParser (parseClosingTag tag) input''
  Just (XML (tag, xml), input''')

literalParser :: Parser XML
literalParser = Parser f
  where
    f "" = Nothing
    f input = flip runParser input $ Literal <$> strPred (`notElem` ['<', '>'])

xmlParser :: Parser [XML]
xmlParser = many (singleFieldParser <|> literalParser)

main :: IO ()
main = do
  text <- readFile "input.txt"
  let parsed = runParser xmlParser [x | x <- text, x `notElem` ['\t', '\n']]
  print parsed