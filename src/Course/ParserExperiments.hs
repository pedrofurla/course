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
import Course.Bind
import Course.Monad
import Course.List
import Course.Optional
import Course.Parser
import qualified Prelude as P
-- Experimenting Parsing Persons

-- Some helpers
emptyPerson = Person {age = 0, firstName = "", surname = "", gender = 'x', phone = ""}
samplePerson = "123 Fred Clarkson m 123-456.789# rest"

toOptionalResult :: ParseResult t -> Optional (ParseResult t)
toOptionalResult r =
  case r of 
    (Result input fs) -> Full r
    _ -> Empty

applyUpdates :: List (Person -> Person) -> Person
applyUpdates = foldRight (\a b -> a b) emptyPerson

-- first attempt, builds a Parser with a "person transformer" that fill in one field
ageP ::Parser (Person -> Person)
ageP  = bindParser (\i -> valueParser $ (\p -> p {age = i}) ) ageParser
fnameP = bindParser (\i -> valueParser $ (\p -> p {firstName = i}) ) firstNameParser
fsurnameP = bindParser (\i -> valueParser $ (\p -> p {surname = i}) ) surnameParser
fgenderP = bindParser (\i -> valueParser $ (\p -> p {gender = i}) ) genderParser
fphoneParserP = bindParser (\i -> valueParser $ (\p -> p {phone = i}) ) phoneParser

-- an parser with the identity function
fskipSpaceP :: Parser (a -> a) 
fskipSpaceP = bindParser (const $ valueParser $ id) space

parseList2 = 
  (ageP          :. fskipSpaceP :. 
   fnameP        :. fskipSpaceP :. 
   fsurnameP     :. fskipSpaceP :. 
   fgenderP      :. fskipSpaceP :. 
   fphoneParserP :. Nil)
seq2 = sequenceParser parseList2

result2 = parse seq2 $ listh samplePerson
person2 = applyUpdates <$> (\(Result _ fs) -> fs) <$> (toOptionalResult result2)

-----
-- The same as above, but this time trying to me more concise by using "updater functions"
-- It still annoys me I need a "empty person"


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

ignoreSpaces parser = spaces1 >~< parser >~< spaces1 
ignoreSpacesC parserC = spaces1 >~< (parserC ~~< spaces1)

skipSpaces1 = bindParser (const $ valueParser $ "") spaces1


--personParser' = ageParser ~~~ space ~~~ firstNameParser ~~~ space ~~~ surnameParser
