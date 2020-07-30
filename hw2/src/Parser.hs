{-# LANGUAGE LambdaCase #-}

module Parser where

import Control.Applicative (Alternative (..))
import Control.Arrow (first)
import Data.Char

data Command = CommandCD FilePath
            | CommandInformation FilePath
            | CommandFindFile String
            | CommandCat String
            | CommandCreateFolder String
            | CommandCreateFile String
            | CommandRemove String
            | CommandWriteFile FilePath String
            | CommandDir
            | CommandLS FilePath
            | CommandExit
            | CommandHelp
            | CommandEmpty
            | CommandCVSInit
            | CommandCVSAdd FilePath
            | CommandCVSUpdate FilePath String
            | CommandCVSHistory FilePath
            | CommandCVSCat FilePath String
            | CommandCVSMergeRevs FilePath String String String
            | CommandCVSDeleteVersion FilePath String
            | CommandCVSRemove FilePath
            | CommandCVSShowEverything
            deriving (Show, Eq)

data Parser s a =
  Parser
    { runParser :: ([s] -> Maybe (a, [s]))
    }

instance Functor (Parser p) where
  fmap f (Parser parser) = Parser (fmap (first f) . parser)

instance Applicative (Parser p) where
  pure a = Parser $ \inp -> return (a, inp)
  (<*>) p1 p2 =
    Parser $ \inp -> do
      (r1, ost1) <- runParser p1 inp
      (r2, ost2) <- runParser p2 ost1
      return (r1 r2, ost2)

instance Monad (Parser p) where
  return = pure
  (>>=) p1 f =
    Parser $ \inp -> do
      (r, ost) <- runParser p1 inp
      runParser (f r) ost

instance Alternative (Parser p) where
  empty = Parser (const Nothing)
  (<|>) f g = Parser $ \inp -> (runParser f inp) <|> (runParser g inp)

ok :: Parser s ()
ok = Parser $ \inp -> return ((), inp)

eof :: Parser s ()
eof =
  Parser $ \inp ->
    if null inp
      then return ((), inp)
      else Nothing

satisfy :: (s -> Bool) -> Parser s s
satisfy p =
  Parser $ \case
    [] -> Nothing
    (x:xs) ->
      if p x
        then return (x, xs)
        else Nothing

element :: Eq s => s -> Parser s s
element s = satisfy (== s)

stream :: Eq s => [s] -> Parser s [s]
stream s = traverse element s

digit :: Parser Char Char
digit = satisfy isDigit

notQuotes :: Parser Char Char
notQuotes = satisfy (\x -> x /= '\"')

commandParser :: Parser Char Command
commandParser = parseCD <|>
                parseInformation <|>
                parseFindFile <|>
                parseCat <|>
                parseCreateFolder <|>
                parseCreateFile <|>
                parseRemove <|>
                parseWriteFile <|>
                parseDir <|>
                parseLS <|>
                parseExit <|>
                parseHelp <|>
                parseEmpty <|>
                parseCVSInit <|>
                parseCVSAdd <|>
                parseCVSUpdate <|>
                parseCVSHistory <|>
                parseCVSCat <|>
                parseCVSMergeRevs <|>
                parseCVSDeleteVersion <|>
                parseCVSRemove <|>
                parseCVSShowEverything

commandDigit :: Parser Char Char
commandDigit = digit

commandChar :: Parser Char Char
commandChar = satisfy isLetter

commandOtherCharacters :: Parser Char Char
commandOtherCharacters = satisfy (\x -> (x == '.') || (x == '/') || (x == '_') || (x == '-') || (x == '!') || (x == '\'') || (x == '(') || (x == ')') || (x == '[') || (x == ']'))

parseCD :: Parser Char Command
parseCD = CommandCD <$> (stream "cd " *> (many $ element ' ') *> parseString) <* (many $ element ' ') <* eof

parseInformation :: Parser Char Command
parseInformation = CommandInformation <$> (stream "information " *> (many $ element ' ') *> parseString) <* (many $ element ' ') <* eof

parseFindFile :: Parser Char Command
parseFindFile = CommandFindFile <$> (stream "find-file " *> (many $ element ' ') *> parseStringInQuotes) <* (many $ element ' ') <* eof

parseCat :: Parser Char Command
parseCat = CommandCat <$> (stream "cat " *> (many $ element ' ') *> parseString) <* (many $ element ' ') <* eof

parseCreateFolder :: Parser Char Command
parseCreateFolder = CommandCreateFolder <$> (stream "create-folder " *> (many $ element ' ') *> parseStringInQuotes) <* (many $ element ' ') <* eof

parseCreateFile :: Parser Char Command
parseCreateFile = CommandCreateFile <$> (stream "create-file " *> (many $ element ' ') *> parseStringInQuotes) <* (many $ element ' ') <* eof

parseRemove :: Parser Char Command
parseRemove = CommandRemove <$> (stream "remove " *> (many $ element ' ') *> parseString) <* (many $ element ' ') <* eof

parseWriteFile :: Parser Char Command
parseWriteFile = CommandWriteFile <$> (stream "write-file " *> (many $ element ' ') *> parseString <* (many $ element ' ')) <*> parseStringInQuotes <* (many $ element ' ') <* eof

parseDir :: Parser Char Command
parseDir = CommandDir <$ stream "dir" <* (many $ element ' ') <* eof

parseLS :: Parser Char Command
parseLS = CommandLS <$> (stream "ls " *> (many $ element ' ') *> parseString) <* (many $ element ' ') <* eof

parseString :: Parser Char String
parseString = (some (commandDigit <|> commandChar <|> commandOtherCharacters)) <|> (fmap (\_ -> "") ok)

parseStringInQuotes :: Parser Char String
parseStringInQuotes = (element '"' *> (some notQuotes) <* element '"')  <|> (fmap (\_ -> "") ok)

parseExit :: Parser Char Command
parseExit = CommandExit <$ stream "exit" <* (many $ element ' ') <* eof

parseHelp :: Parser Char Command
parseHelp = CommandHelp <$ stream "help" <* (many $ element ' ') <* eof

parseEmpty :: Parser Char Command
parseEmpty = CommandEmpty <$ eof

parseCVSInit :: Parser Char Command
parseCVSInit = CommandCVSInit <$ stream "cvs-init" <* (many $ element ' ') <* eof

parseCVSAdd :: Parser Char Command
parseCVSAdd = CommandCVSAdd <$ stream "cvs-add " <* (many $ element ' ') <*> parseString <* (many $ element ' ') <* eof

parseCVSUpdate :: Parser Char Command
parseCVSUpdate = CommandCVSUpdate <$ stream "cvs-update " <* (many $ element ' ') <*> (parseString <* (many $ element ' ')) <*> (parseStringInQuotes <* (many $ element ' ')) <* eof

parseCVSHistory :: Parser Char Command
parseCVSHistory = CommandCVSHistory <$ stream "cvs-history " <* (many $ element ' ') <*> parseString <* (many $ element ' ') <* eof

parseCVSCat :: Parser Char Command
parseCVSCat = CommandCVSCat <$ stream "cvs-cat " <* (many $ element ' ') <*> (parseString <* (many $ element ' ')) <*> (parseStringInQuotes <* (many $ element ' ')) <* eof

parseCVSMergeRevs :: Parser Char Command
parseCVSMergeRevs = CommandCVSMergeRevs <$ stream "cvs-merge-revs " <* (many $ element ' ') <*> (parseString <* (many $ element ' ')) <*> (parseStringInQuotes <* (many $ element ' ')) <*> (parseStringInQuotes <* (many $ element ' ')) <*> ((element '\"') *> (stream "left" <|> stream "right" <|> stream "both") <* (element '\"') <* (many $ element ' ')) <* eof

parseCVSDeleteVersion :: Parser Char Command
parseCVSDeleteVersion = CommandCVSDeleteVersion <$ stream "cvs-delete-version " <* (many $ element ' ') <*> (parseString <* (many $ element ' ')) <*> (parseStringInQuotes <* (many $ element ' ')) <* eof

parseCVSRemove :: Parser Char Command
parseCVSRemove = CommandCVSRemove <$ stream "cvs-remove " <* (many $ element ' ') <*> parseString <* (many $ element ' ') <* eof

parseCVSShowEverything :: Parser Char Command
parseCVSShowEverything = CommandCVSShowEverything <$ stream "cvs-show-everything" <* (many $ element ' ') <* eof
