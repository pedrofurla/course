{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module Course.ParserExperiments where

import Course.Core
import Course.Person
import Course.Functor
import Course.Apply
import Course.Applicative
--import Course.Bind
--import Course.Monad
import Course.List
import Course.Optional
import Course.Parser
import Course.Parser()
import qualified Prelude as P

-- $setup
-- >>> :set -XOverloadedStrings
-- >>> import Data.Char(isUpper)

-- Experimenting Parsing Persons

-- Some helpers

toOptionalResult :: ParseResult t -> Optional (ParseResult t)
toOptionalResult r =
  case r of 
    (Result _ _) -> Full r
    _ -> Empty

applyUpdates :: List (Person -> Person) -> Person
applyUpdates = foldRight (\a b -> a b) emptyPerson

-- an parser with the identity function
fskipSpaceP :: Parser (a -> a) 
fskipSpaceP = bindParser (const $ valueParser $ id) space

emptyPerson = Person {age = 0, firstName = "", surname = "", gender = 'x', phone = ""}
samplePerson = "123 Fred Clarkson m 123-456.789# rest"


personParser0 :: Parser Person = 
  let
    -- first attempt, builds a Parser with a "person transformer" that fill in one field
    --ageP ::Parser (Person -> Person)
    ageP  = bindParser (\i -> valueParser $ (\p -> p {age = i}) ) ageParser
    fnameP = bindParser (\i -> valueParser $ (\p -> p {firstName = i}) ) firstNameParser
    fsurnameP = bindParser (\i -> valueParser $ (\p -> p {surname = i}) ) surnameParser
    fgenderP = bindParser (\i -> valueParser $ (\p -> p {gender = i}) ) genderParser
    fphoneParserP = bindParser (\i -> valueParser $ (\p -> p {phone = i}) ) phoneParser

    parseList2 = 
      (ageP          :. fskipSpaceP :. 
       fnameP        :. fskipSpaceP :. 
       fsurnameP     :. fskipSpaceP :. 
       fgenderP      :. fskipSpaceP :. 
       fphoneParserP :. Nil)
    seq2 = sequenceParser parseList2

    result2 = parse seq2 $ listh samplePerson
    person2 = applyUpdates <$> (\(Result _ fs) -> fs) <$> (toOptionalResult result2)
    in error "skiped" person2
-----
-- The same as above, but this time trying to me more concise by using "updater functions"
-- It still annoys me I need a "empty person"

personParser1 :: Parser Person = 
  let
    updAge     p a = p { age       = a }
    updName    p a = p { firstName = a }
    updSurname p a = p { surname   = a }
    updGender  p a = p { gender    = a }
    updPhone   p a = p { phone     = a }

    pParser :: (Person -> a -> Person) -> Parser a -> Parser (Person -> Person)
    pParser upd parser = bindParser (valueParser . (flip upd)) parser

    pList = 
      (pParser updAge ageParser         :. fskipSpaceP :. 
       pParser updName firstNameParser  :. fskipSpaceP :. 
       pParser updSurname surnameParser :. fskipSpaceP :. 
       pParser updGender genderParser   :. fskipSpaceP :. 
       pParser updPhone phoneParser     :. Nil)
    pSequence = sequenceParser pList

    result3 = parse pSequence $ listh samplePerson
    person3 = applyUpdates <$> (\(Result _ fs) -> fs) <$> (toOptionalResult result3)

  in error "skiped" person3

-- | Variations and attempts on Perser#personParser
-- >>> isErrorResult (parse personParser4 "")
-- True
--
-- >>> isErrorResult (parse personParser4 "12x Fred Clarkson m 123-456.789#")
-- True
--
-- >>> isErrorResult (parse personParser4 "123 fred Clarkson m 123-456.789#")
-- True
--
-- >>> isErrorResult (parse personParser4 "123 Fred Cla m 123-456.789#")
-- True
--
-- >>> isErrorResult (parse personParser4 "123 Fred clarkson m 123-456.789#")
-- True
--
-- >>> isErrorResult (parse personParser4 "123 Fred Clarkson x 123-456.789#")
-- True
--
-- >>> isErrorResult (parse personParser4 "123 Fred Clarkson m 1x3-456.789#")
-- True
--
-- >>> isErrorResult (parse personParser4 "123 Fred Clarkson m -123-456.789#")
-- True
--
-- >>> isErrorResult (parse personParser4 "123 Fred Clarkson m 123-456.789")
-- True
--
-- >>> parse personParser4 "123 Fred Clarkson m 123-456.789#"
-- Result >< Person {age = 123, firstName = "Fred", surname = "Clarkson", gender = 'm', phone = "123-456.789"}
--
-- >>> parse personParser4 "123 Fred Clarkson m 123-456.789# rest"
-- Result > rest< Person {age = 123, firstName = "Fred", surname = "Clarkson", gender = 'm', phone = "123-456.789"}
personParser4 ::
  Parser Person
personParser4 =
  let
    lift5 ::
      Apply f =>
      (a -> b -> c -> d -> e -> k)
      -> f a
      -> f b
      -> f c
      -> f d
      -> f e
      -> f k
    lift5 f a b c d e = f <$> a <*> b <*> c <*> d <*> e

    p3 = (lift5 Person) 
            ageParser  
            (space >>> firstNameParser)
            (space >>> surnameParser)
            (space >>> genderParser) 
            (space >>>phoneParser)
   in p3

-- | Variations and attempts on Perser#personParser
-- >>> isErrorResult (parse personParser3 "")
-- True
--
-- >>> isErrorResult (parse personParser3 "12x Fred Clarkson m 123-456.789#")
-- True
--
-- >>> isErrorResult (parse personParser3 "123 fred Clarkson m 123-456.789#")
-- True
--
-- >>> isErrorResult (parse personParser3 "123 Fred Cla m 123-456.789#")
-- True
--
-- >>> isErrorResult (parse personParser3 "123 Fred clarkson m 123-456.789#")
-- True
--
-- >>> isErrorResult (parse personParser3 "123 Fred Clarkson x 123-456.789#")
-- True
--
-- >>> isErrorResult (parse personParser3 "123 Fred Clarkson m 1x3-456.789#")
-- True
--
-- >>> isErrorResult (parse personParser3 "123 Fred Clarkson m -123-456.789#")
-- True
--
-- >>> isErrorResult (parse personParser3 "123 Fred Clarkson m 123-456.789")
-- True
--
-- >>> parse personParser3 "123 Fred Clarkson m 123-456.789#"
-- Result >< Person {age = 123, firstName = "Fred", surname = "Clarkson", gender = 'm', phone = "123-456.789"}
--
-- >>> parse personParser3 "123 Fred Clarkson m 123-456.789# rest"
-- Result > rest< Person {age = 123, firstName = "Fred", surname = "Clarkson", gender = 'm', phone = "123-456.789"}
personParser3 ::
  Parser Person
personParser3 =
  let
    emptyPerson = Person {age = 0, firstName = "", surname = "", gender = 'x', phone = ""}

    applyUpdates :: List (Person -> Person) -> Person
    applyUpdates = foldRight ($) emptyPerson

    -- some semi-lenses for the rescue
    updAge     a p = p { age       = a }
    updName    a p = p { firstName = a }
    updSurname a p = p { surname   = a }
    updGender  a p = p { gender    = a }
    updPhone   a p = p { phone     = a }

    pParser :: (a -> Person -> Person) -> Parser a -> Parser (Person -> Person)
    pParser upd parser = bindParser (valueParser . upd) parser

    pList2:: Parser (List (Person -> Person))
    pList2 = do 
      age  <- pParser updAge ageParser 
      name <- space >>> pParser updName firstNameParser
      sur  <- space >>> pParser updSurname surnameParser
      gen  <- space >>> pParser updGender genderParser
      phn  <- space >>> pParser updPhone phoneParser
      pure (age :. name :. sur :. gen :. phn :. Nil)

  in
    bindParser (\pps -> valueParser $ applyUpdates pps) pList2

