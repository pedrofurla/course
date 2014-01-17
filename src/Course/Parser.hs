{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

module Course.Parser where

import Course.Core
import Course.Person
import Course.Functor
import Course.Apply
import Course.Applicative
import Course.Bind
import Course.Monad
import Course.List
import Course.Optional
import qualified Prelude as P

-- $setup
-- >>> :set -XOverloadedStrings
-- >>> import Data.Char(isUpper)

type Input = Chars

data ParseResult a =
  UnexpectedEof
  | ExpectedEof Input
  | UnexpectedChar Char
  | Failed
  | Result Input a
  deriving Eq

instance Show a => Show (ParseResult a) where
  show UnexpectedEof =
    "Expected end of stream"
  show (ExpectedEof i) =
    stringconcat ["Expected end of stream, but got >", show i, "<"]
  show (UnexpectedChar c) =
    stringconcat ["Unexpected character", show [c]]
  show Failed =
    "Parse failed"
  show (Result i a) =
    stringconcat ["Result >", hlist i, "< ", show a]

-- Function to also access the input while binding parsers.
withResultInput ::
  (Input -> a -> ParseResult b)
  -> ParseResult a
  -> ParseResult b
withResultInput _ UnexpectedEof =
  UnexpectedEof
withResultInput _ (ExpectedEof i) =
  ExpectedEof i
withResultInput _ (UnexpectedChar c) =
  UnexpectedChar c
withResultInput _ Failed =
  Failed
withResultInput f (Result i a) =
  f i a

-- Function to determine is a parse result is an error.
isErrorResult :: -- TODO can simplify that to only the False result and _ for everything else
  ParseResult a
  -> Bool
isErrorResult UnexpectedEof = 
  True
isErrorResult (ExpectedEof _) =
  True
isErrorResult (UnexpectedChar _) =
  True
isErrorResult Failed =
  True
isErrorResult (Result _ _) =
  False

data Parser a = P {
  parse :: Input -> ParseResult a
}

-- | Return a parser that always succeeds with the given value and consumes no input.
--
-- >>> parse (valueParser 3) "abc"
-- Result >abc< 3
valueParser ::
  a
  -> Parser a
valueParser a = P (\i -> Result i a)

-- | Return a parser that always fails with the given error.
--
-- >>> isErrorResult (parse failed "abc")
-- True
failed ::
  Parser a
failed = P $ const Failed

-- | Return a parser that succeeds with a character off the input or fails with an error if the input is empty.
--
-- >>> parse character "abc"
-- Result >bc< 'a'
--
-- >>> isErrorResult (parse character "")
-- True
character ::
  Parser Char
character = P (\i -> 
  case i of
    (c:.cs) -> Result cs c
    _ -> Failed)

-- | Return a parser that puts its input into the given parser and
--
--   * if that parser succeeds with a value (a), put that value into the given function
--     then put in the remaining input in the resulting parser.
--
--   * if that parser fails with an error the returned parser fails with that error.
--
-- /Tip:/ Use @withResultInput@.
--
-- >>> parse (bindParser (\c -> if c == 'x' then character else valueParser 'v') character) "abc"
-- Result >bc< 'v'
--
-- >>> parse (bindParser (\c -> if c == 'x' then character else valueParser 'v') character) "a"
-- Result >< 'v'
--
-- >>> parse (bindParser (\c -> if c == 'x' then character else valueParser 'v') character) "xabc"
-- Result >bc< 'a'
--
-- >>> isErrorResult (parse (bindParser (\c -> if c == 'x' then character else valueParser 'v') character) "")
-- True
--
-- >>> isErrorResult (parse (bindParser (\c -> if c == 'x' then character else valueParser 'v') character) "x")
-- True
bindParser ::
-- withResultInput :: (Input -> a -> ParseResult b) -> ParseResult a -> ParseResult b
-- ParseResult a = Result Input a | ...
-- Parser a = P { Input -> ParseResult a }
  (a -> Parser b)
  -> Parser a
  -> Parser b
bindParser f (P pa) = 
  P(\s ->
      let 
        ra = pa s
      in
       withResultInput (\i a -> 
        let 
          P pb = f a 
        in
          case pb i of
            Result i' a' -> Result i' a'
            other -> other 
            ) ra )

fbindParser ::
  Parser a
  -> (a -> Parser b)
  -> Parser b
fbindParser =
  flip bindParser

-- notice parse 
--    (bindParser valueParser (list character)) "AAA" === parse (list character) "AAA"
-- simplifing 
--     bindParser valueParser X === X

-- | Return a parser that puts its input into the given parser and
--
--   * if that parser succeeds with a value (a), ignore that value
--     but put the remaining input into the second given parser.
--
--   * if that parser fails with an error the returned parser fails with that error.
--
-- /Tip:/ Use @bindParser@.
--
-- >>> parse (character >>> valueParser 'v') "abc"
-- Result >bc< 'v'
--
-- >>> isErrorResult (parse (character >>> valueParser 'v') "")
-- True
(>>>) ::
  Parser a
  -> Parser b
  -> Parser b
p1 >>> p2 =  p1 `fbindParser` (\_ -> p2)

-- | Return a parser that tries the first parser for a successful value.
--
--   * If the first parser succeeds then use this parser.
--
--   * If the first parser fails, try the second parser.
--
-- >>> parse (character ||| valueParser 'v') ""
-- Result >< 'v'
--
-- >>> parse (failed ||| valueParser 'v') ""
-- Result >< 'v'
--
-- >>> parse (character ||| valueParser 'v') "abc"
-- Result >bc< 'a'
--
-- >>> parse (failed ||| valueParser 'v') "abc"
-- Result >abc< 'v'
(|||) ::
  Parser a
  -> Parser a
  -> Parser a
P pa ||| P pb = P (\s -> let r = pa s in if not $ isErrorResult r then r else pb s )

infixl 3 |||

-- | Return a parser that continues producing a list of values from the given parser.
--
-- /Tip:/ Use @many1@, @valueParser@ and @(|||)@.
--
-- >>> parse (list (character)) "abc"
-- Result >< "abc"
--
-- >>> parse (list (character *> valueParser 'v')) "abc"
-- Result >< "vvv"
--
-- >>> parse (list (character *> valueParser 'v')) ""
-- Result >< ""
--
-- >>> parse (list (character)) ""
-- Result >< ""
--
-- >>> parse (list (digit)) "123abc"
-- Result >abc< ""
--
-- >>> parse (list digit) "abc"
-- Result >abc< ""
list ::
  Parser a
  -> Parser (List a)
list (P pa) = -- TODO this impl is odd. Use foldr ? Use the recommened combinators?
  let 
    lp parsed = 
      P(\i -> 
        let r = pa i in
        case r of 
            Result "" a  -> Result "" $ parsed ++ (a:.Nil)
            Result i2 a  -> parse (lp (parsed ++ (a:.Nil))) i2
            _            -> Result i parsed)
  in
    lp Nil

-- | Return a parser that produces at least one value from the given parser then
-- continues producing a list of values from the given parser (to ultimately produce a non-empty list).
-- The returned parser fails if The input is empty.
--
-- /Tip:/ Use @bindParser@, @list@ and @value@.
--
-- >>> parse (many1 (character)) "abc"
-- Result >< "abc"
--
-- >>> parse (many1 (character *> valueParser 'v')) "abc"
-- Result >< "vvv"
--
-- >>> isErrorResult (parse (many1 (character *> valueParser 'v')) "")
-- True
--
-- >>> parse (list (digit)) "123abc"
-- Result >abc< ""
--
-- >>> isErrorResult(parse (list digit) "abc")
-- True
many1 ::
  Parser a
  -> Parser (List a)
many1 (P pa) = -- shamelessly copied from `list`
  let 
    lp parsed = 
      P(\i -> 
        let r = pa i in
        case (r, parsed) of 
            (Result "" a, _)  -> Result "" $ parsed ++ (a:.Nil)
            (Result i2 a, _)  -> parse (lp (parsed ++ (a:.Nil))) i2
            (_, Nil) -> Failed
            (_,_) -> Result i parsed)
  in
    lp Nil

-- | Return a parser that produces a character but fails if
--
--   * The input is empty.
--
--   * The character does not satisfy the given predicate.
--
-- /Tip:/ The @bindParser@ and @character@ functions will be helpful here.
--
-- >>> parse (satisfy isUpper) "Abc"
-- Result >bc< 'A'
--
-- >>> isErrorResult (parse (satisfy isUpper) "abc")
-- True
satisfy ::
  (Char -> Bool)
  -> Parser Char
satisfy f =
  bindParser (\i -> if f i then valueParser i else failed) character

-- | Return a parser that produces the given character but fails if
--
--   * The input is empty.
--
--   * The produced character is not equal to the given character.
--
-- /Tip:/ Use the @satisfy@ function.
is ::
  Char -> Parser Char
is = satisfy . (==)

-- | Return a parser that produces a character between '0' and '9' but fails if
--
--   * The input is empty.
--
--   * The produced character is not a digit.
--
-- /Tip:/ Use the @satisfy@ and @Data.Char.isDigit@ functions.
digit ::
  Parser Char
digit = satisfy isDigit

-- | Return a parser that produces zero or a positive integer but fails if
--
--   * The input is empty.
--
--   * The input does not produce a value series of digits
--
-- /Tip:/ Use the @bindParser@, @valueParser@, @list@, @reads@ and @digit@
-- functions.
natural ::
  Parser Int
natural =
  -- bindParser :: (a -> Parser b) -> Parser a -> Parser b
  let 
    oparser :: Chars -> Optional (Parser Int)
    oparser cs = (valueParser . fst) <$> (reads cs)
  in 
    bindParser (\cs -> (oparser cs) ?? failed) (list digit)
    
natural_old ::
  Parser Int
natural_old =
  -- bindParser :: (a -> Parser b) -> Parser a -> Parser b
  let 
    --bind = valueParser . P.read . hlist
    res :: Chars -> (ParseResult Int)
    res cs = ((\(i,cs') -> Result cs' i) <$> (reads cs::Optional (Int,Chars))) ?? Failed
    opt :: Chars -> Parser Int
    opt cs = P (\_ -> res cs )
  in 
    bindParser opt (list digit)
    

--
-- | Return a parser that produces a space character but fails if
--
--   * The input is empty.
--
--   * The produced character is not a space.
--
-- /Tip:/ Use the @satisfy@ and @Data.Char.isSpace@ functions.
space ::
  Parser Char
space = is ' '

-- | Return a parser that produces one or more space characters
-- (consuming until the first non-space) but fails if
--
--   * The input is empty.
--
--   * The first produced character is not a space.
--
-- /Tip:/ Use the @many1@ and @space@ functions.
spaces1 ::
  Parser Chars
spaces1 = many1 space

-- | Return a parser that produces a lower-case character but fails if
--
--   * The input is empty.
--
--   * The produced character is not lower-case.
--
-- /Tip:/ Use the @satisfy@ and @Data.Char.isLower@ functions.
lower ::
  Parser Char
lower = satisfy isLower

-- | Return a parser that produces an upper-case character but fails if
--
--   * The input is empty.
--
--   * The produced character is not upper-case.
--
-- /Tip:/ Use the @satisfy@ and @Data.Char.isUpper@ functions.
upper ::
  Parser Char
upper = satisfy isUpper

-- | Return a parser that produces an alpha character but fails if
--
--   * The input is empty.
--
--   * The produced character is not alpha.
--
-- /Tip:/ Use the @satisfy@ and @Data.Char.isAlpha@ functions.
alpha ::
  Parser Char
alpha = satisfy isAlpha

-- Helper
toParserList :: Parser a -> Parser (List a)
toParserList = bindParser (\i -> valueParser (i:.Nil))

concatParser :: Parser (List t) -> Parser (List t) -> Parser (List t)
concatParser (P p1) (P p2) = 
  P(\i -> withResultInput (\i2 a2 -> withResultInput (\i3 a3 -> Result i3 $ a2 ++ a3) $ p2 i2  ) $ p1 i)

(~~~) :: 
  Parser t 
  -> Parser t 
  -> Parser (List t)
l ~~~ r =  concatParser (toParserList l) (toParserList r) 

(~~<) :: Parser t -> Parser (List t) -> Parser (List t)
l ~~< r =  concatParser (toParserList l) r

(>~~) :: Parser (List t) -> Parser t -> Parser (List t)
l >~~ r =  concatParser l (toParserList r)

(>~<) :: Parser (List t) -> Parser (List t) -> Parser (List t)
(>~<) = concatParser

-- | Return a parser that sequences the given list of parsers by producing all their results
-- but fails on the first failing parser of the list.
--
-- /Tip:/ Use @bindParser@ and @valueParser@.
-- /Tip:/ Optionally use @Prelude.foldr@. If not, an explicit recursive call.
--
-- >>> parse (sequenceParser (character :. is 'x' :. upper :. Nil)) "axCdef"
-- Result >def< "axC"
--
-- >>> isErrorResult (parse (sequenceParser (character :. is 'x' :. upper :. Nil)) "abCdef")
-- True
sequenceParser ::
  List (Parser a)
  -> Parser (List a)
sequenceParser ps = 
  let
    fold = foldRight (\p pl -> concatParser (toParserList p) pl) (valueParser Nil) ps
  in fold 

-- | Return a parser that produces the given number of values off the given parser.
-- This parser fails if the given parser fails in the attempt to produce the given number of values.
--
-- /Tip:/ Use @sequenceParser@ and @Prelude.replicate@.
--
-- >>> parse (thisMany 4 upper) "ABCDef"
-- Result >ef< "ABCD"
--
-- >>> isErrorResult (parse (thisMany 4 upper) "ABcDef")
-- True
thisMany ::
  Int
  -> Parser a
  -> Parser (List a)
thisMany n = sequenceParser . (replicate n)

-- | Write a parser for Person.age.
--
-- /Age: positive integer/
--
-- /Tip:/ Equivalent to @natural@.
--
-- >>> parse ageParser "120"
-- Result >< 120
--
-- >>> isErrorResult (parse ageParser "abc")
-- True
--
-- >>> isErrorResult (parse ageParser "-120")
-- True
ageParser ::
  Parser Int
ageParser = natural

-- | Write a parser for Person.firstName.
-- /First Name: non-empty string that starts with a capital letter/
--
-- /Tip:/ Use @bindParser@, @value@, @upper@, @list@ and @lower@.
--
-- >>> parse firstNameParser "Abc"
-- Result >< "Abc"
--
-- >>> isErrorResult (parse firstNameParser "abc")
-- True
firstNameParser ::
  Parser Chars
firstNameParser = upper ~~< (many1 lower)
  --concatParser (toParserList upper) (many1 lower)
  --bindParser (\i -> ) upper

-- | Write a parser for Person.surname.
--
-- /Surname: string that starts with a capital letter and is followed by 5 or more lower-case letters./
--
-- /Tip:/ Use @bindParser@, @value@, @upper@, @thisMany@, @lower@ and @list@.
--
-- >>> parse surnameParser "Abcdef"
-- Result >< "Abcdef"
--
-- >>> isErrorResult (parse surnameParser "Abc")
-- True
--
-- >>> isErrorResult (parse surnameParser "abc")
-- True
surnameParser :: Parser Chars
surnameParser = upper ~~< (thisMany 5 lower) >~< (list lower)
  --concatParser (toParserList upper) ( concatParser (thisMany 4 lower) (many1 lower))

-- | Write a parser for Person.gender.
--
-- /Gender: character that must be @'m'@ or @'f'@/
--
-- /Tip:/ Use @is@ and @(|||)@./
--
-- >>> parse genderParser "mabc"
-- Result >abc< 'm'
--
-- >>> parse genderParser "fabc"
-- Result >abc< 'f'
--
-- >>> isErrorResult (parse genderParser "abc")
-- True
genderParser ::
  Parser Char
genderParser = (is 'm') ||| (is 'f')

-- | Write part of a parser for Person.phoneBody.
-- This parser will only produce a string of digits, dots or hyphens.
-- It will ignore the overall requirement of a phone number to
-- start with a digit and end with a hash (#).
--
-- /Phone: string of digits, dots or hyphens .../
--
-- /Tip:/ Use @list@, @digit@, @(|||)@ and @is@.
--
-- >>> parse phoneBodyParser "123-456"
-- Result >< "123-456"
--
-- >>> parse phoneBodyParser "123-4a56"
-- Result >a56< "123-4"
--
-- >>> parse phoneBodyParser "a123-456"
-- Result >a123-456< ""
phoneBodyParser ::
  Parser Chars
phoneBodyParser = list (digit ||| (is '-') ||| (is '.'))

-- | Write a parser for Person.phone.
--
-- /Phone: ... but must start with a digit and end with a hash (#)./
--
-- /Tip:/ Use @bindParser@, @value@, @digit@, @phoneBodyParser@ and @is@.
--
-- >>> parse phoneParser "123-456#"
-- Result >< "123-456"
--
-- >>> parse phoneParser "123-456#abc"
-- Result >abc< "123-456"
--
-- >>> isErrorResult (parse phoneParser "123-456")
-- True
--
-- >>> isErrorResult (parse phoneParser "a123-456")
-- True
phoneParser ::
  Parser Chars
phoneParser = digit ~~< phoneBodyParser >~< ( (is '#') >>> (valueParser Nil)  )
  

-- | Write a parser for Person.
--
-- /Tip:/ Use @bindParser@,
--            @value@,
--            @(>>>)@,
--            @ageParser@,
--            @firstNameParser@,
--            @surnameParser@,
--            @genderParser@,
--            @phoneParser@.
--
-- >>> isErrorResult (parse personParser "")
-- True
--
-- >>> isErrorResult (parse personParser "12x Fred Clarkson m 123-456.789#")
-- True
--
-- >>> isErrorResult (parse personParser "123 fred Clarkson m 123-456.789#")
-- True
--
-- >>> isErrorResult (parse personParser "123 Fred Cla m 123-456.789#")
-- True
--
-- >>> isErrorResult (parse personParser "123 Fred clarkson m 123-456.789#")
-- True
--
-- >>> isErrorResult (parse personParser "123 Fred Clarkson x 123-456.789#")
-- True
--
-- >>> isErrorResult (parse personParser "123 Fred Clarkson m 1x3-456.789#")
-- True
--
-- >>> isErrorResult (parse personParser "123 Fred Clarkson m -123-456.789#")
-- True
--
-- >>> isErrorResult (parse personParser "123 Fred Clarkson m 123-456.789")
-- True
--
-- >>> parse personParser "123 Fred Clarkson m 123-456.789#"
-- Result >< Person {age = 123, firstName = "Fred", surname = "Clarkson", gender = 'm', phone = "123-456.789"}
--
-- >>> parse personParser "123 Fred Clarkson m 123-456.789# rest"
-- Result > rest< Person {age = 123, firstName = "Fred", surname = "Clarkson", gender = 'm', phone = "123-456.789"}
personParser ::
  Parser Person
personParser =
  let
    emptyPerson = Person {age = 0, firstName = "", surname = "", gender = 'x', phone = ""}

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

    result str = parse pSequence str
  in
    P ( \i -> 
      ( (\(Result left fs) -> Result left (applyUpdates fs)) <$> (toOptionalResult $ result i)) ?? Failed )



-- Make sure all the tests pass!


-- | Write a Functor instance for a @Parser@.
-- /Tip:/ Use @bindParser@ and @valueParser@.
instance Functor Parser where
  (<$>) =
     error "todo"

-- | Write a Apply instance for a @Parser@.
-- /Tip:/ Use @bindParser@ and @valueParser@.
instance Apply Parser where
  (<*>) =
    error "todo"

-- | Write an Applicative functor instance for a @Parser@.
instance Applicative Parser where
  pure =
    error "todo"

-- | Write a Bind instance for a @Parser@.
instance Bind Parser where
  (=<<) =
    error "todo"

instance Monad Parser where

instance P.Monad Parser where
  (>>=) =
    flip (=<<)
  return =
    pure
