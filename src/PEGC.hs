module PEGC where

import           PEGC.Types

import qualified Data.Aeson              as Aeson

import           Data.Text               (Text)
import qualified Data.Text               as Text

import           Control.Applicative
import           Data.Functor

-- import           Control.Lens

import qualified Language.C.Syntax.AST   as C

import           Text.Parser.Char
import           Text.Parser.Combinators
import           Text.Parser.Token

import           Text.Trifecta

(|>) :: a -> (a -> b) -> b
(|>) = flip ($)
infixl 0 |>

annotate :: String -> Parser a -> Parser a
annotate s p = p <?> s

labelP :: Parser Label
labelP = [ annotate "wrapper" (LabelWrapper <$  symbol "_")
         , annotate "nil"     (LabelNil     <$  symbol "[]")
         , annotate "cons"    (LabelCons    <$  symbol "(:)")
         , annotate "single"  (LabelSingle  <$  symbol "(:[])")
         , annotate "name"    (LabelName    <$> nameP)
         ] |> choice |> annotate "label"

pegP :: Parser PEG
pegP = [ annotate "empty"
         (PEGEmpty       <$  symbol "eps")
       , annotate "term"
         (PEGTerminal    <$> terminalP)
       , annotate "nonterm"
         (PEGNonTerminal <$> nameP)
       , parens pegP
       , annotate "and"
         (PEGAnd         <$> (symbol "&" *> pegP))
       , annotate "not"
         (PEGNot         <$> (symbol "!" *> pegP))
       , annotate "choice"
         (PEGChoice      <$> (pegP `sepBy1` token (char '/')))
       , annotate "seq"
         (PEGSequence    <$> (pegP `sepBy1` someSpace))
       ] |> choice |> annotate "peg"

terminalP :: Parser Terminal
terminalP = [ annotate "any"   (TerminalAny   <$  symbol "any")
            , annotate "char"  (TerminalChar  <$> charLiteral)
            , annotate "range" (TerminalRange <$> characterRangeP)
            ] |> choice |> annotate "terminal" |> token
  where
    characterRangeP :: Parser (Char, Char)
    characterRangeP = token $ do
      char '['
      s <- charLiteral
      char '-'
      e <- charLiteral
      char ']'
      pure (s, e)

nameP :: Parser Name
nameP = ((:) <$> upper <*> many (alphaNum <|> oneOf ""))
        |> fmap Text.pack |> annotate "name" |> token

definitionP :: Parser GrammarLine
definitionP = GLDefine
              <$> labelP
              <*> (symbol "." *> nameP)
              <*> (symbol "::=" *> pegP <* semi)

-- grammarP :: Parser Grammar
-- grammarP = undefined

main :: IO ()
main = do
  str <- getLine
  let Success gl = parseString definitionP mempty str
  print $ Aeson.encode gl
