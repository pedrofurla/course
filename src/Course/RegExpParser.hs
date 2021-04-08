{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Course.RegExpParser
  ( RegExp (..),
  -- parseRegExp,
  )
where

import Course.List
import Course.Optional
import Course.Parser
import Data.Char
import Prelude (Eq, Show)

data RegExp
  = -- | A character that is not in "()*|."
    Normal Char
  | -- | Any character
    Any
  | -- | Zero or more occurances of the same regexp
    ZeroOrMore RegExp
  | -- | A choice between 2 regexps
    Or RegExp RegExp
  | -- | A sequence of regexps.
    Str [RegExp]
  deriving (Show, Eq)

-- parseRegExp :: Input -> Optional RegExp
-- parseRegExp = undefined

regCharacter :: Parser RegExp
regCharacter = satisfy (notElem "()*|.") >>= Normal

anyParser :: Parser RegExp
anyParser = is '.' >> Any

-- zeroOrMore :: Parser RegExp
