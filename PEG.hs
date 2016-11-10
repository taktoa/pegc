{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell    #-}
module Main where

import           Data.Text               (Text)
import qualified Data.Text               as Text

import qualified Data.Aeson              as Aeson
import qualified Data.Aeson.TH           as Aeson

import           Control.Applicative
import           Data.Data
import           Data.Functor

-- import           Control.Lens

import qualified Language.C.Syntax.AST   as C

import           Text.Parser.Char
import           Text.Parser.Combinators
import           Text.Parser.Token

import           Text.Trifecta

type Grammar = [GrammarLine]

data GrammarLine
  = GLDefine Label Name PEG
  | GLDeclare Name [Name]
  | GLMacro Name [Name] PEG
  | GLInclude Path
  deriving (Show, Eq, Typeable, Data)

data PEG
  = PEGEmpty
  | PEGAnd PEG
  | PEGNot PEG
  | PEGApply Name [PEG]
  | PEGChoice [PEG]
  | PEGSequence [PEG]
  | PEGTerminal Terminal
  | PEGNonTerminal Name
  deriving (Show, Eq, Typeable, Data)

data Terminal
  = TerminalAny
  | TerminalChar Char
  | TerminalRange (Char, Char)
  deriving (Show, Eq, Typeable, Data)

data Label
  = LabelWrapper
  | LabelNil
  | LabelCons
  | LabelSingle
  | LabelName Name
  deriving (Show, Eq, Typeable, Data)

type Path = Text
type Name = Text

$(concat <$> mapM (Aeson.deriveJSON Aeson.defaultOptions)
  [ ''Label, ''Terminal, ''PEG, ''GrammarLine ])

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
