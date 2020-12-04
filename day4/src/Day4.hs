{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

module Day4
  ( parsePassport,
    parsePassports,
    isValid,
    validatePassport,
  )
where

import Data.Map (keys)
import Data.Set (insert, isSubsetOf, member)
import Data.Text (stripPrefix, stripSuffix)
import qualified Data.Text as Text
import Relude.Extra ((!?))

type Passport = Map Text Text

newtype Byr = Byr Int
  deriving (Show)

newtype Iyr = Iyr Int
  deriving (Show)

newtype Eyr = Eyr Int
  deriving (Show)

data Hgt = Cm Int | In Int
  deriving (Show)

newtype Hcl = Hcl Text
  deriving (Show)

data Ecl = Amb | Blu | Brn | Gry | Grn | Hzl | Oth
  deriving (Show)

newtype Pid = Pid Text
  deriving (Show)

newtype Cid = Cid Text
  deriving (Show)

data ValidPassport = ValidPassport Byr Iyr Eyr Hgt Hcl Ecl Pid (Maybe Cid)
  deriving (Show)

validatePassport :: Passport -> Maybe ValidPassport
validatePassport p =
  ValidPassport
    <$> (validateByr =<< (p !? "byr"))
    <*> (validateIyr =<< (p !? "iyr"))
    <*> (validateEyr =<< (p !? "eyr"))
    <*> (validateHgt =<< (p !? "hgt"))
    <*> (validateHcl =<< (p !? "hcl"))
    <*> (validateEcl =<< (p !? "ecl"))
    <*> (validatePid =<< (p !? "pid"))
    <*> pure (Cid <$> (p !? "cid"))

validateByr :: Text -> Maybe Byr
validateByr t = Byr <$> (validate check =<< readInt t)
  where
    check y = 1920 <= y && y <= 2002

validateIyr :: Text -> Maybe Iyr
validateIyr t = Iyr <$> (validate check =<< readInt t)
  where
    check y = 2010 <= y && y <= 2020

validateEyr :: Text -> Maybe Eyr
validateEyr t = fmap Eyr (validate check =<< readInt t)
  where
    check y = 2020 <= y && y <= 2030

validateHgt :: Text -> Maybe Hgt
validateHgt t = validate check =<< readHgt t
  where
    readHgt = \case
      (stripSuffix "cm" -> Just s) -> Cm <$> readInt s
      (stripSuffix "in" -> Just s) -> In <$> readInt s
      _ -> Nothing

    check = \case
      Cm c -> 150 <= c && c <= 193
      In i -> 59 <= i && i <= 76

validateHcl :: Text -> Maybe Hcl
validateHcl t = Hcl <$> validate check t
  where
    check = \case
      (stripPrefix "#" -> Just s) -> Text.all isCharAccepted s
      _ -> False

    isCharAccepted = (`member` fromList "0123456789abcdef")

validateEcl :: Text -> Maybe Ecl
validateEcl = \case
  "amb" -> Just Amb
  "blu" -> Just Blu
  "brn" -> Just Brn
  "gry" -> Just Gry
  "grn" -> Just Grn
  "hzl" -> Just Hzl
  "oth" -> Just Oth
  _ -> Nothing

validatePid :: Text -> Maybe Pid
validatePid t = Pid <$> validate check t
  where
    check s = Text.length s == 9 && Text.all isDigit s
    isDigit = (`member` fromList "0123456789")

readInt :: Text -> Maybe Int
readInt = readMaybe . toString

validate :: (a -> Bool) -> a -> Maybe a
validate p x = if p x then Just x else Nothing

requiredKeys :: Set Text
requiredKeys = fromList ["byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid"]

acceptedKey :: Set Text
acceptedKey = insert "cid" requiredKeys

isAcceptedKey :: Text -> Bool
isAcceptedKey k = member k acceptedKey

isValid :: Passport -> Bool
isValid p = requiredKeys `isSubsetOf` passportKeys
  where
    passportKeys = fromList (keys p)

parsePassports :: Text -> [Passport]
parsePassports = fmap parsePassport . splitPassports
  where
    splitPassports = Text.splitOn "\n\n"

parsePassport :: Text -> Passport
parsePassport = fromList . fmap parseField . splitFields

parseField :: Text -> (Text, Text)
parseField t = case Text.splitOn ":" t of
  [key, value]
    | isAcceptedKey key -> (key, value)
    | otherwise -> error $ "Key is not accepted : >" <> key <> "<"
  _ -> error $ "Cannot parse field >" <> t <> "<"

splitFields :: Text -> [Text]
splitFields = concatMap (Text.splitOn " ") . Text.splitOn "\n"