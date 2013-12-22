-- |
-- Module: Options.OptionTypes
-- License: MIT
module Options.OptionTypes where

import           Data.Int
import           Data.List (intercalate)
import qualified Data.Map as Map
import qualified Data.Set as Set
import           Data.Word

import           Options.Types

-- | Store an option as a @'Bool'@. The option's value must be either
-- @\"true\"@ or @\"false\"@.
--
-- Boolean options are unary, which means that their value is optional when
-- specified on the command line. If a flag is present, the option is set to
-- True.
--
-- >$ ./app -q
-- >$ ./app --quiet
--
-- Boolean options may still be specified explicitly by using long flags with
-- the @--flag=value@ format. This is the only way to set a unary flag to
-- @\"false\"@.
--
-- >$ ./app --quiet=true
-- >$ ./app --quiet=false
optionType_bool :: OptionType Bool
optionType_bool = (optionType "bool" False parseBool (\x -> if x then "true" else "false"))
	{ optionTypeUnary = Just True
	}

parseBool :: String -> Either String Bool
parseBool s = case s of
	"true" -> Right True
	"false" -> Right False
	_ -> Left (show s ++ " is not in {\"true\", \"false\"}.")

-- | Store an option value as a @'String'@. The value is decoded to Unicode
-- first, if needed. The value may contain non-Unicode bytes, in which case
-- they will be stored using GHC 7.4's encoding for mixed-use strings.
optionType_string :: OptionType String
optionType_string = optionType "text" "" Right id

parseInteger :: String -> Either String Integer
parseInteger s = parsed where
	parsed = if valid
		then Right (read s)
		else Left (show s ++ " is not an integer.")
	valid = case s of
		[] -> False
		'-':s' -> allDigits s'
		_ -> allDigits s
	allDigits = all (\c -> c >= '0' && c <= '9')

parseBoundedIntegral :: (Bounded a, Integral a) => String -> String -> Either String a
parseBoundedIntegral label = parse where
	getBounds :: (Bounded a, Integral a) => (String -> Either String a) -> a -> a -> (Integer, Integer)
	getBounds _ min' max' = (toInteger min', toInteger max')
	
	(minInt, maxInt) = getBounds parse minBound maxBound
	
	parse s = case parseInteger s of
		Left err -> Left err
		Right int -> if int < minInt || int > maxInt
			then Left (show int ++ " is not within bounds [" ++ show minInt ++ ":" ++ show maxInt ++ "] of type " ++ label ++ ".")
			else Right (fromInteger int)

parseFloat :: Read a => String -> Either String a
parseFloat s = case reads s of
	[(x, "")] -> Right x
	_ -> Left (show s ++ " is not a number.")

optionTypeBoundedInt :: (Bounded a, Integral a, Show a) => String -> OptionType a
optionTypeBoundedInt tName = optionType tName 0 (parseBoundedIntegral tName) show

-- | Store an option as an @'Int'@. The option value must be an integer /n/
-- such that @'minBound' <= n <= 'maxBound'@.
optionType_int :: OptionType Int
optionType_int = optionTypeBoundedInt "int"

-- | Store an option as an @'Int8'@. The option value must be an integer /n/
-- such that @'minBound' <= n <= 'maxBound'@.
optionType_int8 :: OptionType Int8
optionType_int8 = optionTypeBoundedInt "int8"

-- | Store an option as an @'Int16'@. The option value must be an integer /n/
-- such that @'minBound' <= n <= 'maxBound'@.
optionType_int16 :: OptionType Int16
optionType_int16 = optionTypeBoundedInt "int16"

-- | Store an option as an @'Int32'@. The option value must be an integer /n/
-- such that @'minBound' <= n <= 'maxBound'@.
optionType_int32 :: OptionType Int32
optionType_int32 = optionTypeBoundedInt "int32"

-- | Store an option as an @'Int64'@. The option value must be an integer /n/
-- such that @'minBound' <= n <= 'maxBound'@.
optionType_int64 :: OptionType Int64
optionType_int64 = optionTypeBoundedInt "int64"

-- | Store an option as a @'Word'@. The option value must be a positive
-- integer /n/ such that @0 <= n <= 'maxBound'@.
optionType_word :: OptionType Word
optionType_word = optionTypeBoundedInt "uint"

-- | Store an option as a @'Word8'@. The option value must be a positive
-- integer /n/ such that @0 <= n <= 'maxBound'@.
optionType_word8 :: OptionType Word8
optionType_word8 = optionTypeBoundedInt "uint8"

-- | Store an option as a @'Word16'@. The option value must be a positive
-- integer /n/ such that @0 <= n <= 'maxBound'@.
optionType_word16 :: OptionType Word16
optionType_word16 = optionTypeBoundedInt "uint16"

-- | Store an option as a @'Word32'@. The option value must be a positive
-- integer /n/ such that @0 <= n <= 'maxBound'@.
optionType_word32 :: OptionType Word32
optionType_word32 = optionTypeBoundedInt "uint32"

-- | Store an option as a @'Word64'@. The option value must be a positive
-- integer /n/ such that @0 <= n <= 'maxBound'@.
optionType_word64 :: OptionType Word64
optionType_word64 = optionTypeBoundedInt "uint64"

-- | Store an option as an @'Integer'@. The option value must be an integer.
-- There is no minimum or maximum value.
optionType_integer :: OptionType Integer
optionType_integer = optionType "integer" 0 parseInteger show

-- | Store an option as a @'Float'@. The option value must be a number. Due to
-- the imprecision of floating-point math, the stored value might not exactly
-- match the user's input. If the user's input is out of range for the
-- @'Float'@ type, it will be stored as @Infinity@ or @-Infinity@.
optionType_float :: OptionType Float
optionType_float = optionType "float32" 0 parseFloat show

-- | Store an option as a @'Double'@. The option value must be a number. Due to
-- the imprecision of floating-point math, the stored value might not exactly
-- match the user's input. If the user's input is out of range for the
-- @'Double'@ type, it will be stored as @Infinity@ or @-Infinity@.
optionType_double :: OptionType Double
optionType_double = optionType "float64" 0 parseFloat show

-- | Store an option as a @'Maybe'@ of another type. The value will be
-- @Nothing@ if the option is set to an empty string.
optionType_maybe :: OptionType a -> OptionType (Maybe a)
optionType_maybe t = maybeT { optionTypeUnary = unary } where
	maybeT = optionType name Nothing (parseMaybe t) (showMaybe t)
	name = "maybe<" ++ optionTypeName t ++ ">"
	unary = case optionTypeUnary t of
		Nothing -> Nothing
		Just val -> Just (Just val)

parseMaybe :: OptionType val -> String -> Either String (Maybe val)
parseMaybe t s = case s of
	"" -> Right Nothing
	_ -> case optionTypeParse t s of
		Left err -> Left err
		Right a -> Right (Just a)

showMaybe :: OptionType val -> Maybe val -> String
showMaybe _ Nothing = ""
showMaybe t (Just x) = optionTypeShow t x

-- | Store an option as a @'Set.Set'@, using another option type for the
-- elements. The separator should be a character that will not occur within
-- the values, such as a comma or semicolon.
--
-- Duplicate elements in the input are permitted.
optionType_set :: Ord a
               => Char -- ^ Element separator
               -> OptionType a -- ^ Element type
               -> OptionType (Set.Set a)
optionType_set sep t = optionType name Set.empty parseSet showSet where
	name = "set<" ++ optionTypeName t ++ ">"
	parseSet s = case parseList (optionTypeParse t) (split sep s) of
		Left err -> Left err
		Right xs -> Right (Set.fromList xs)
	showSet xs = intercalate [sep] (map (optionTypeShow t) (Set.toList xs))

-- | Store an option as a 'Map.Map', using other option types for the keys and
-- values.
--
-- The item separator is used to separate key/value pairs from eachother. It
-- should be a character that will not occur within either the keys or values.
--
-- The value separator is used to separate the key from the value. It should
-- be a character that will not occur within the keys. It may occur within the
-- values.
--
-- Duplicate keys in the input are permitted. The final value for each key is
-- stored.
optionType_map :: Ord k
               => Char -- ^ Item separator
               -> Char -- ^ Key/Value separator
               -> OptionType k -- ^ Key type
               -> OptionType v -- ^ Value type
               -> OptionType (Map.Map k v)
optionType_map itemSep keySep kt vt = optionType name Map.empty parser showMap where
	name = "map<" ++ optionTypeName kt ++ "," ++ optionTypeName vt ++ ">"
	parser s = parseMap keySep (optionTypeParse kt) (optionTypeParse vt) (split itemSep s)
	showMap m = intercalate [itemSep] (map showItem (Map.toList m))
	showItem (k, v) = optionTypeShow kt k ++ [keySep] ++ optionTypeShow vt v

parseList :: (String -> Either String a) -> [String] -> Either String [a]
parseList p = loop where
	loop [] = Right []
	loop (x:xs) = case p x of
		Left err -> Left err
		Right v -> case loop xs of
			Left err -> Left err
			Right vs -> Right (v:vs)

parseMap :: Ord k => Char -> (String -> Either String k) -> (String -> Either String v) -> [String] -> Either String (Map.Map k v)
parseMap keySep pKey pVal = parsed where
	parsed strs = case parseList pItem strs of
		Left err -> Left err
		Right xs -> Right (Map.fromList xs)
	pItem s = case break (== keySep) s of
		(sKey, valAndSep) -> case valAndSep of
			[] -> Left ("Map item " ++ show s ++ " has no value.")
			_ : sVal -> case pKey sKey of
				Left err -> Left err
				Right key -> case pVal sVal of
					Left err -> Left err
					Right val -> Right (key, val)

split :: Char -> String -> [String]
split _ [] = []
split sep s0 = loop s0 where
	loop s = let
		(chunk, rest) = break (== sep) s
		cont = chunk : loop (tail rest)
		in if null rest then [chunk] else cont

-- | Store an option as a list, using another option type for the elements.
-- The separator should be a character that will not occur within the values,
-- such as a comma or semicolon.
optionType_list :: Char -- ^ Element separator
                -> OptionType a -- ^ Element type
                -> OptionType [a]
optionType_list sep t = optionType name [] parser shower where
	name =  "list<" ++ optionTypeName t ++ ">"
	parser s = parseList (optionTypeParse t) (split sep s)
	shower xs = intercalate [sep] (map (optionTypeShow t) xs)

-- | Store an option as one of a set of possible values. The type must be a
-- bounded enumeration, and the type's 'Show' instance will be used to
-- implement the parser.
--
-- This is a simplistic implementation, useful for quick scripts. Users with
-- more complex requirements for enum parsing are encouraged to define their
-- own option types using 'optionType'.
--
-- @
--data Action = Hello | Goodbye
--    deriving (Bounded, Enum, Show)
--
--data MainOptions = MainOptions { optAction :: Action }
--
--instance 'Options' MainOptions where
--    'defineOptions' = pure MainOptions
--        \<*\> defineOptionWith (optionType_enum \"action\") (\\o -> o
--            { 'optionLongFlags' = [\"action\"]
--            , 'optionDefault' = Hello
--            })
--
--main = 'runCommand' $ \\opts args -> do
--    putStrLn (\"Running action \" ++ show (optAction opts))
-- @
--
-- >$ ./app
-- >Running action Hello
-- >$ ./app --action=Goodbye
-- >Running action Goodbye
optionType_enum :: (Bounded a, Enum a, Show a) => String -> OptionType a
optionType_enum tName = optionType tName minBound parseEnum show where
	values = Map.fromList [(show x, x) | x <- enumFrom minBound]
	setString = "{" ++ intercalate ", " (map show (Map.keys values)) ++ "}"
	parseEnum s = case Map.lookup s values of
		Nothing -> Left (show s ++ " is not in " ++ setString ++ ".")
		Just x -> Right x
