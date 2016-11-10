{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell    #-}

module PEGC.Types where

import           Data.Data

import           Data.Text     (Text)
import qualified Data.Text     as Text

import qualified Data.Aeson.TH as Aeson

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
