{-# LANGUAGE DeriveDataTypeable #-}
module Main where

import           Data.Text               (Text)
import qualified Data.Text               as Text

import           Data.Data
import           Data.Functor

-- import           Control.Lens

import qualified Language.C.Syntax.AST   as C

import           Text.Parser.Char
import           Text.Parser.Combinators
import           Text.Parser.Token

import           Text.Trifecta

type Grammar = [Definition]

data Definition
  = Definition Label Identifier PEG
  deriving (Show, Eq, Typeable, Data)

data PEG
  = PEGEmpty
  | PEGAnd PEG
  | PEGNot PEG
  | PEGChoice [PEG]
  | PEGSequence [PEG]
  | PEGTerminal Terminal
  | PEGNonTerminal Identifier
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
  | LabelName Identifier
  deriving (Show, Eq, Typeable, Data)

type Identifier = Text

(|>) :: a -> (a -> b) -> b
(|>) = flip ($)
infixl 0 |>

labelP :: Parser Label
labelP = [ LabelWrapper <$  symbol "_"
         , LabelNil     <$  symbol "[]"
         , LabelCons    <$  symbol "(:)"
         , LabelSingle  <$  symbol "(:[])"
         , LabelName    <$> identifierP
         ] |> choice

pegP :: Parser PEG
pegP = [ PEGEmpty       <$  symbol "eps"
       , PEGTerminal    <$> terminalP
       , PEGNonTerminal <$> identifierP
       , PEGAnd         <$> (symbol "&" *> pegP)
       , PEGNot         <$> (symbol "!" *> pegP)
       , PEGChoice      <$> sepBy1 pegP (symbol "/")
       , PEGSequence    <$> sepBy1 pegP whiteSpace
       , parens pegP
       ] |> choice

terminalP :: Parser Terminal
terminalP = [ TerminalAny   <$  symbol "any"
            , TerminalChar  <$> (char '\'' *> characterP <* char '\'')
            , TerminalRange <$> characterRangeP
            ] |> choice
  where
    characterP :: Parser Char
    characterP = [ alphaNum
                 , [ char 'u'
                   , char 'n' $> '\n'
                   , char 'r' $> '\r'
                   ] |> choice |> escape
                 ] |> choice
      where
        escape p = char '\\' *> p

    characterRangeP :: Parser (Char, Char)
    characterRangeP = do char '['
                         s <- characterP
                         char '-'
                         e <- characterP
                         char ']'
                         pure (s, e)

identifierP :: Parser Identifier
identifierP = _

definitionP :: Parser Definition
definitionP = Definition
              <$> labelP
              <*> identifierP
              <*> (symbol "::=" *> pegP)

-- grammarP :: Parser Grammar
-- grammarP = undefined

main :: IO ()
main = return ()
