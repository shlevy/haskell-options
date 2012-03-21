{-# LANGUAGE TemplateHaskell #-}

-- |
-- Module: Options
-- License: MIT
--
-- The @Options@ package lets library and application developers easily work
-- with command-line options.
--
-- The following example is a full program that can accept two options,
-- @--message@ and @--quiet@:
--
-- @
--{-\# LANGUAGE TemplateHaskell \#-}
--
--import Options
--
--'defineOptions' \"MainOptions\" $ do
--    'stringOption' \"optMessage\" \"message\" \"Hello world!\"
--        \"A message to show the user.\"
--    'boolOption' \"optQuiet\" \"quiet\" False
--        \"Whether to be quiet.\"
--
--main :: IO ()
--main = do
--    opts <- 'getOptionsOrDie'
--    if optQuiet opts
--        then return ()
--        else putStrLn (optMessage opts)
-- @
--
-- >$ ./hello
-- >Hello world!
-- >$ ./hello --message='ciao mondo'
-- >ciao mondo
-- >$ ./hello --quiet
-- >$
--
-- In addition, this library will automatically create documentation options
-- such as @--help@ and @--help-all@:
--
-- >$ ./hello --help
-- >Help Options:
-- >  -h, --help                  Show option summary.
-- >  --help-all                  Show all help options.
-- >
-- >Application Options:
-- >  --message                   A message to show the user.
-- >  --quiet                     Whether to be quiet.
--
module Options
	(
	-- * The Options type
	  Options
	, defaultOptions
	
	-- * Parsing options
	, getOptionsOrDie
	
	-- * Defining options
	, defineOptions
	
	-- ** Simple option definitions
	, boolOption
	, stringOption
	, stringsOption
	, textOption
	, textsOption
	, pathOption
	, intOption
	, integerOption
	, floatOption
	, doubleOption
	
	-- ** Using imported options
	, ImportedOptions
	, importedOptions
	, options
	
	-- ** Advanted option definitions
	, Option
	, option
	, optionShortFlags
	, optionLongFlags
	, optionDefault
	, optionType
	, optionDescription
	, optionGroup
	
	-- ** Option types
	, OptionType
	
	, optionTypeBool
	
	, optionTypeString
	, optionTypeText
	, optionTypeRawString
	, optionTypeFilePath
	
	, optionTypeInt
	, optionTypeInt8
	, optionTypeInt16
	, optionTypeInt32
	, optionTypeInt64
	, optionTypeWord
	, optionTypeWord8
	, optionTypeWord16
	, optionTypeWord32
	, optionTypeWord64
	, optionTypeInteger
	
	, optionTypeFloat
	, optionTypeDouble
	
	, optionTypeList
	, optionTypeSet
	, optionTypeMap
	, optionTypeEnum
	
	-- * Option groups
	, Group
	, group
	, groupTitle
	, groupDescription
	
	-- * Subcommands
	, Subcommand
	, subcommand
	, runSubcommand
	
	-- * Argument Parsing
	, Parsed
	, parsedError
	, parsedHelp
	
	-- ** Options
	, ParsedOptions
	, parsedOptions
	, parseOptions
	
	-- ** Subcommands
	, ParsedSubcommand
	, parsedSubcommand
	, parseSubcommand
	) where

import           Control.Monad.Error (ErrorT, runErrorT, throwError)
import           Control.Monad.IO.Class
import           Control.Monad.Reader
import           Control.Monad.State
import           Data.Char (isAlpha, isAlphaNum, isLower)
import           Data.List (foldl', intercalate)
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified System.Environment
import           System.Exit (exitFailure, exitSuccess)
import           System.IO

import qualified Filesystem.Path.CurrentOS as Path
import           Language.Haskell.TH
import           Language.Haskell.TH.Syntax (mkNameG_tc)

import           Options.Help
import           Options.OptionTypes
import           Options.Tokenize
import           Options.Types

-- | Options are defined together in a single data type, which will be an
-- instance of 'Options'.
--
-- See 'defineOptions' for details on defining instances of 'Options'.
--
-- See 'options' for details on including imported 'Options' types in locally
-- defined options.
class Options a where
	optionsDefs :: OptionDefinitions a
	optionsParse :: TokensFor a -> Either String a
	optionsMeta :: OptionsMeta a

data OptionsMeta a = OptionsMeta
	{ optionsMetaName :: Name
	, optionsMetaKeys :: Set.Set String
	, optionsMetaShortFlags :: Set.Set Char
	, optionsMetaLongFlags :: Set.Set String
	}

defaultOptions :: Options a => a
defaultOptions = opts where
	parsed = parseOptions []
	opts = case parsedOptions parsed of
		Just v -> v
		Nothing -> error ("defaultOptions: Internal error, option defaults are invalid: " ++ (case parsedError parsed of
			Just err -> err
			Nothing -> "(no error provided)"))

newtype OptionsM a = OptionsM { unOptionsM :: StateT OptionsDeclState (ErrorT String (Reader Loc)) a }

data OptionsDeclState = OptionsDeclState
	{ stDecls :: [(Name, Type, Q Exp, Q Exp)]
	, stSeenFieldNames :: Set.Set String
	, stSeenKeys :: Set.Set String
	, stSeenShortFlags :: Set.Set Char
	, stSeenLongFlags :: Set.Set String
	}

instance Monad OptionsM where
	return = OptionsM . return
	m >>= f = OptionsM (unOptionsM m >>= (unOptionsM . f))

runOptionsM :: Loc -> OptionsM () -> Either String OptionsDeclState
runOptionsM loc (OptionsM m) = runReader (runErrorT (execStateT m initState)) loc where
	initState = OptionsDeclState [] Set.empty Set.empty Set.empty Set.empty

-- | Defines a new data type, containing fields for application or library
-- options. The new type will be an instance of 'Options'.
--
-- Example: this use of @defineOptions@:
--
-- @
--'defineOptions' \"MainOptions\" $ do
--    'stringOption' \"optMessage\" \"message\" \"Hello world!\" \"\"
--    'boolOption' \"optQuiet\" \"quiet\" False \"\"
-- @
--
-- expands to the following definition:
--
-- >data MainOptions = MainOptions
-- >    { optMessage :: String
-- >    , optQuiet :: Bool
-- >    }
-- >
-- >instance Options MainOptions
--
defineOptions :: String -> OptionsM () -> Q [Dec]
defineOptions rawName optionsM = do
	loc <- location
	let dataName = mkName rawName
	declState <- case runOptionsM loc optionsM of
		Left err -> fail err
		Right st -> return st
	let fields = stDecls declState
	
	let dataDec = DataD [] dataName [] [RecC dataName
		[(fName, NotStrict, t) | (fName, t, _, _) <- fields]
		][]
	
	exp_optionsDefs <- getOptionsDefs fields
	exp_optionsParse <- getOptionsParse dataName fields
	exp_optionsMeta <- getOptionsMeta loc rawName declState
	let instanceDec = InstanceD [] (AppT (ConT ''Options) (ConT dataName))
		[ ValD (VarP 'optionsDefs) (NormalB exp_optionsDefs) []
		, ValD (VarP 'optionsParse) (NormalB exp_optionsParse) []
		, ValD (VarP 'optionsMeta) (NormalB exp_optionsMeta) []
		]
	
	return [dataDec, instanceDec]

getOptionsDefs :: [(Name, Type, Q Exp, Q Exp)] -> Q Exp
getOptionsDefs fields = do
	infoExps <- forM fields (\(_, _, infoExp, _) -> infoExp)
	[| OptionDefinitions (concat $(return (ListE infoExps))) [] |]

getOptionsParse :: Name -> [(Name, Type, Q Exp, Q Exp)] -> Q Exp
getOptionsParse dataName fields = do
	let genBind (_, _, _, qParseExp) = do
		varName <- newName "val"
		parseExp <- qParseExp
		return (varName, BindS (VarP varName) parseExp)
	
	names_and_binds <- mapM genBind fields
	let names = [n | (n, _) <- names_and_binds]
	let binds = [b | (_, b) <- names_and_binds]
	
	returnExp <- [| return |]
	let consExp = foldl' AppE (ConE dataName) (map VarE names)
	let parserM = return (DoE (binds ++ [NoBindS (AppE returnExp consExp)]))
	[| unParserM $parserM |]

getOptionsMeta :: Loc -> String -> OptionsDeclState -> Q Exp
getOptionsMeta loc typeName st = do
	let pkg = loc_package loc
	let mod' = loc_module loc
	let keys = Set.toList (stSeenKeys st)
	let shorts = Set.toList (stSeenShortFlags st)
	let longs = Set.toList (stSeenLongFlags st)
	[| OptionsMeta (mkNameG_tc pkg mod' typeName) (Set.fromList keys) (Set.fromList shorts) (Set.fromList longs) |]

newtype ParserM optType a = ParserM { unParserM :: TokensFor optType -> Either String a }

instance Monad (ParserM optType) where
	return x = ParserM (\_ -> Right x)
	m >>= f = ParserM (\env -> case unParserM m env of
		Left err -> Left err
		Right x -> unParserM (f x) env)

putOptionDecl :: Name -> Type -> Q Exp -> Q Exp -> OptionsM ()
putOptionDecl name qtype infoExp parseExp = OptionsM (modify (\st -> st
	{ stDecls = stDecls st ++ [(name, qtype, infoExp, parseExp)]
	}))

-- | Defines a new option in the current options type.
--
-- All options must have a /field name/ and one or more /flags/. Options may
-- also have a default value, a description, or a group.
--
-- The field name is how the option will be accessed in Haskell, and is
-- typically prefixed with \"opt\". This is used to define a record field,
-- and must be a valid Haskell field name (see 'defineOptions' for details).
--
-- The /flags/ are how the user specifies an option on the command line. Flags
-- may be /short/ or /long/. See 'optionShortFlags' and 'optionLongFlags' for
-- details.
--
-- @
--'option' \"optFoo\" (\\o -> o
--    { 'optionLongFlags' = [\"names\"]
--    , 'optionDefault' = \"Alice;Bob;Charles\"
--    , 'optionType' = 'optionTypeList' \';\' 'optionTypeString'
--    }
-- @
option :: String -- ^ Field name
       -> (Option String -> Option a) -- ^ Option definition
       -> OptionsM ()
option fieldName f = do
	let emptyGroup = Group
		{ groupName = Nothing
		, groupTitle = ""
		, groupDescription = ""
		}
	let opt = f (Option
		{ optionShortFlags = []
		, optionLongFlags = []
		, optionDefault = ""
		, optionType = optionTypeString
		, optionDescription = ""
		, optionGroup = emptyGroup
		})
	
	loc <- OptionsM ask
	let key = loc_package loc ++ ":" ++ loc_module loc ++ ":" ++ fieldName
	
	let shorts = optionShortFlags opt
	let longs = optionLongFlags opt
	let def = optionDefault opt
	
	let desc = optionDescription opt
	
	let optGroup = optionGroup opt
	let optGroupDesc = groupTitle optGroup
	let optGroupHelpDesc = groupDescription optGroup
	let groupInfoExp = case groupName optGroup of
		Nothing -> [| Nothing |]
		Just n -> [| Just (GroupInfo n optGroupDesc optGroupHelpDesc) |]
	
	let OptionType thType unary parseOptType parseExp = optionType opt
	
	checkFieldName fieldName
	checkValidFlags fieldName shorts longs
	checkUniqueKey key
	checkUniqueFlags fieldName shorts longs
	
	case parseOptType def of
		Right _ -> return ()
		Left err -> OptionsM (throwError ("Invalid default vaue for option " ++ show fieldName ++ ": " ++ err))
	
	OptionsM (modify (\st -> st
		{ stSeenFieldNames = Set.insert fieldName (stSeenFieldNames st)
		, stSeenKeys = Set.insert key (stSeenKeys st)
		, stSeenShortFlags = Set.union (Set.fromList shorts) (stSeenShortFlags st)
		, stSeenLongFlags = Set.union (Set.fromList longs) (stSeenLongFlags st)
		}))
	
	putOptionDecl
		(mkName fieldName)
		thType
		[| [OptionInfo key shorts longs def unary desc $groupInfoExp] |]
		[| parseOptionTok key $parseExp def |]

parseOptionTok :: String -> (String -> Either String a) -> String -> ParserM optType a
parseOptionTok key p def = do
	TokensFor tokens _ <- ParserM (\t -> Right t)
	let val = case lookup key tokens of
		Nothing -> def
		Just x -> x
	case p val of
		Left err -> ParserM (\_ -> Left err)
		Right x -> return x

validFieldName :: String -> Bool
validFieldName = valid where
	valid s = case s of
		[] -> False
		c : cs -> validFirst c && all validGeneral cs
	validFirst c = isLower c || c == '_'
	validGeneral c = isAlphaNum c || c == '_' || c == '\''

checkFieldName :: String -> OptionsM ()
checkFieldName name = do
	unless (validFieldName name)
		(OptionsM (throwError ("Option field name " ++ show name ++ " is invalid.")))
	st <- OptionsM get
	when (Set.member name (stSeenFieldNames st))
		(OptionsM (throwError ("Duplicate definitions of field " ++ show name ++ ".")))

checkUniqueKey :: String -> OptionsM ()
checkUniqueKey key = do
	st <- OptionsM get
	when (Set.member key (stSeenKeys st))
		(OptionsM (throwError ("Option key " ++ show key ++ " has already been defined. This should never happen; please send an error report to the maintainer of the 'options' package.")))

checkValidFlags :: String -> [Char] -> [String] -> OptionsM ()
checkValidFlags fieldName shorts longs = do
	-- Check that at least one flag is defined (in either 'shorts' or 'longs').
	when (length shorts == 0 && length longs == 0)
		(OptionsM (throwError ("Option " ++ show fieldName ++ " does not define any flags")))
	
	-- Check that 'shorts' contains only non-repeated letters and digits
	when (hasDuplicates shorts)
		(OptionsM (throwError ("Option " ++ show fieldName ++ " has duplicate short flags")))
	case filter (not . isAlphaNum) shorts of
		[] -> return ()
		invalid -> OptionsM (throwError ("Option " ++ show fieldName ++ " has invalid short flags " ++ show invalid))
	
	-- Check that 'longs' contains only non-repeated, non-empty strings
	-- containing {LETTER,DIGIT,-,_} and starting with a letter.
	when (hasDuplicates longs)
		(OptionsM (throwError ("Option " ++ show fieldName ++ " has duplicate long flags.")))
	case filter (not . validLongFlag) longs of
		[] -> return ()
		invalid -> OptionsM (throwError ("Option " ++ show fieldName ++ " has invalid long flags " ++ show invalid))

checkUniqueFlags :: String -> [Char] -> [String] -> OptionsM ()
checkUniqueFlags fieldName shorts longs = do
	st <- OptionsM get
	
	-- Check that none of this option's flags are already used.
	let dupShort = do
		f <- Set.toList (Set.intersection (stSeenShortFlags st) (Set.fromList shorts))
		return ('-' : [f])
	let dupLong = do
		f <- Set.toList (Set.intersection (stSeenLongFlags st) (Set.fromList longs))
		return ("--" ++ f)
	let dups = dupShort ++ dupLong
	unless (null dups)
		(OptionsM (throwError ("Option " ++ show fieldName ++ " uses already-defined flags " ++ show dups)))

validLongFlag :: String -> Bool
validLongFlag = valid where
	valid s = case s of
		[] -> False
		c : cs -> isAlpha c && all validGeneral cs
	validGeneral c = isAlphaNum c || c == '-' || c == '_'

hasDuplicates :: Ord a => [a] -> Bool
hasDuplicates xs = Set.size (Set.fromList xs) /= length xs

-- | Include options defined elsewhere into the current options definition.
--
-- This is typically used by application developers to include options defined
-- in third-party libraries. For example, the author of the \"foo\" library
-- would define and export @FooOptions@:
--
-- @
--module Foo (FooOptions, foo) where
--
--import Options
--
--'defineOptions' \"FooOptions\" $ do
--    'boolOption' \"optFrob\" \"frob\" True \"Enable frobnication.\"
--
--foo :: FooOptions -> IO ()
-- @
--
-- and the author of an application would use @options@ to let users specify
-- @--frob@:
--
-- @
--module Main where
--
--import Options
--import Foo
--
--'defineOptions' \"MainOptions\" $ do
--   'boolOption' \"optVerbose\" \"verbose\" False \"Be really loud.\"
--   'options' \"optFoo\" \'\'FooOptions
--
--main :: IO ()
--main = do
--    opts <- 'getOptionsOrDie'
--    foo (optFoo opts)
-- @
--
-- Use of 'options' may be arbitrarily nested. Library authors are encouraged
-- to aggregate their options into a single top-level type, so application
-- authors can include it easily in their own option definitions.
options :: String -> ImportedOptions a -> OptionsM ()
options fieldName (ImportedOptions meta) = do
	checkFieldName fieldName
	
	let typeName = optionsMetaName meta
	st <- OptionsM get
	
	-- Check unique keys
	let dupKeys = Set.intersection (stSeenKeys st) (optionsMetaKeys meta)
	unless (Set.null dupKeys)
		(OptionsM (throwError ("Imported options type " ++ show typeName ++ " contains duplicate keys " ++ show (Set.toList dupKeys) ++ ". This should never happen; please send an error report to the maintainer of the 'options' package.")))
	
	-- Check unique flags
	let dupShort = do
		f <- Set.toList (Set.intersection (stSeenShortFlags st) (optionsMetaShortFlags meta))
		return ('-' : [f])
	let dupLong = do
		f <- Set.toList (Set.intersection (stSeenLongFlags st) (optionsMetaLongFlags meta))
		return ("--" ++ f)
	let dups = dupShort ++ dupLong
	unless (null dups)
		(OptionsM (throwError ("Imported options type " ++ show typeName ++ " contains conflicting definitions for flags " ++ show dups)))
	
	OptionsM (modify (\st' -> st'
		{ stSeenFieldNames = Set.insert fieldName (stSeenFieldNames st)
		, stSeenShortFlags = Set.union (optionsMetaShortFlags meta) (stSeenShortFlags st)
		, stSeenLongFlags = Set.union (optionsMetaLongFlags meta) (stSeenLongFlags st)
		}))
	
	putOptionDecl
		(mkName fieldName)
		(ConT typeName)
		[| suboptsDefs $(varE (mkName fieldName)) |]
		[| parseSubOptions |]

newtype ImportedOptions a = ImportedOptions (OptionsMeta a)

importedOptions :: Options a => ImportedOptions a
importedOptions = ImportedOptions optionsMeta

castTokens :: TokensFor a -> TokensFor b
castTokens (TokensFor tokens args) = TokensFor tokens args

parseSubOptions :: Options a => ParserM optType a
parseSubOptions = do
	tokens <- ParserM (\t -> Right t)
	case optionsParse (castTokens tokens) of
		Left err -> ParserM (\_ -> Left err)
		Right x -> return x

suboptsDefs :: Options a => (b -> a) -> [OptionInfo]
suboptsDefs rec = defsB where
	defsB = case defsA rec of
		OptionDefinitions opts _ -> opts
	defsA :: Options a => (b -> a) -> OptionDefinitions a
	defsA _ = optionsDefs

-- | Define an option of type @'Bool'@. This is a simple wrapper around
-- 'option'.
boolOption :: String -- ^ Field name
           -> String -- ^ Long flag
           -> Bool -- ^ Default value
           -> String -- ^ Description in @--help@
           -> OptionsM ()
boolOption name flag def desc = option name (\o -> o
	{ optionLongFlags = [flag]
	, optionDefault = if def then "true" else "false"
	, optionType = optionTypeBool
	, optionDescription = desc
	})

-- | Define an option of type @'String'@. This is a simple wrapper around
-- 'option'.
stringOption :: String -- ^ Field name
             -> String -- ^ Long flag
             -> String -- ^ Default value
             -> String -- ^ Description in @--help@
             -> OptionsM ()
stringOption name flag def desc = option name (\o -> o
	{ optionLongFlags = [flag]
	, optionDefault = def
	, optionDescription = desc
	})

-- | Define an option of type @['String']@. This is a simple wrapper around
-- 'option'. Items are comma-separated.
stringsOption :: String -- ^ Field name
              -> String -- ^ Long flag
              -> [String] -- ^ Default value
              -> String -- ^ Description in @--help@
              -> OptionsM ()
stringsOption name flag def desc = option name (\o -> o
	{ optionLongFlags = [flag]
	, optionDefault = intercalate "," def
	, optionType = optionTypeList ',' optionTypeString
	, optionDescription = desc
	})

-- | Define an option of type @'Text.Text'@. This is a simple wrapper around
-- 'option'.
textOption :: String -- ^ Field name
           -> String -- ^ Long flag
           -> Text.Text -- ^ Default value
           -> String -- ^ Description in @--help@
           -> OptionsM ()
textOption name flag def desc = option name (\o -> o
	{ optionLongFlags = [flag]
	, optionDefault = Text.unpack def
	, optionType = optionTypeText
	, optionDescription = desc
	})

-- | Define an option of type @['Text.Text']@. This is a simple wrapper around
-- 'option'. Items are comma-separated.
textsOption :: String -- ^ Field name
            -> String -- ^ Long flag
            -> [Text.Text] -- ^ Default value
            -> String -- ^ Description in @--help@
            -> OptionsM ()
textsOption name flag def desc = option name (\o -> o
	{ optionLongFlags = [flag]
	, optionDefault = Text.unpack (Text.intercalate (Text.pack ",") def)
	, optionType = optionTypeList ',' optionTypeText
	, optionDescription = desc
	})

-- | Define an option of type @'Path.FilePath'@. This is a simple wrapper
-- around 'option'.
pathOption :: String -- ^ Field name
           -> String -- ^ Long flag
           -> Path.FilePath -- ^ Default value
           -> String -- ^ Description in @--help@
           -> OptionsM ()
pathOption name flag def desc = option name (\o -> o
	{ optionLongFlags = [flag]
	, optionDefault = Path.encodeString def
	, optionType = optionTypeFilePath
	, optionDescription = desc
	})

-- | Define an option of type @'Int'@. This is a simple wrapper around
-- 'option'.
intOption :: String -- ^ Field name
          -> String -- ^ Long flag
          -> Int -- ^ Default value
          -> String -- ^ Description in @--help@
          -> OptionsM ()
intOption name flag def desc = option name (\o -> o
	{ optionLongFlags = [flag]
	, optionDefault = show def
	, optionType = optionTypeInt
	, optionDescription = desc
	})

-- | Define an option of type @'Integer'@. This is a simple wrapper around
-- 'option'.
integerOption :: String -- ^ Field name
              -> String -- ^ Long flag
              -> Integer -- ^ Default value
              -> String -- ^ Description in @--help@
              -> OptionsM ()
integerOption name flag def desc = option name (\o -> o
	{ optionLongFlags = [flag]
	, optionDefault = show def
	, optionType = optionTypeInteger
	, optionDescription = desc
	})

-- | Define an option of type @'Float'@. This is a simple wrapper around
-- 'option'.
floatOption :: String -- ^ Field name
            -> String -- ^ Long flag
            -> Float -- ^ Default value
            -> String -- ^ Description in @--help@
            -> OptionsM ()
floatOption name flag def desc = option name (\o -> o
	{ optionLongFlags = [flag]
	, optionDefault = show def
	, optionType = optionTypeFloat
	, optionDescription = desc
	})

-- | Define an option of type @'Double'@. This is a simple wrapper around
-- 'option'.
doubleOption :: String -- ^ Field name
             -> String -- ^ Long flag
             -> Double -- ^ Default value
             -> String -- ^ Description in @--help@
             -> OptionsM ()
doubleOption name flag def desc = option name (\o -> o
	{ optionLongFlags = [flag]
	, optionDefault = show def
	, optionType = optionTypeDouble
	, optionDescription = desc
	})

-- | Define an option group.
--
-- Option groups are used to make long @--help@ output more readable, by
-- hiding obscure or rarely-used options from the main summary.
--
-- If an option is in a group named @\"examples\"@, it will only be shown
-- in the help output if the user provides the flag @--help-examples@ or
-- @--help-all@. The flag @--help-all@ will show all options, in all groups.
group :: String -- ^ Group name
      -> (Group -> Group)
      -> Group
group name f = f (Group
	{ groupName = Just name
	, groupTitle = ""
	, groupDescription = ""
	})

-- | See @'parseOptions'@ and @'parseSubcommand'@.
class Parsed a where
	parsedError_ :: a -> Maybe String
	parsedHelp_ :: a -> String

-- | See @'parseOptions'@.
data ParsedOptions opts = ParsedOptions (Maybe opts) (Maybe String) String

-- | See @'parseSubcommand'@.
data ParsedSubcommand action = ParsedSubcommand (Maybe action) (Maybe String) String

instance Parsed (ParsedOptions a) where
	parsedError_ (ParsedOptions _ x _) = x
	parsedHelp_ (ParsedOptions _ _ x) = x

instance Parsed (ParsedSubcommand a) where
	parsedError_ (ParsedSubcommand _ x _) = x
	parsedHelp_ (ParsedSubcommand _ _ x) = x

-- | Get the options value that was parsed from argv, or @Nothing@ if the
-- arguments could not be converted into options.
--
-- Note: This function return @Nothing@ if the user provided a help flag. To
-- check whether an error occured during parsing, check the value of
-- @'parsedError'@.
parsedOptions :: ParsedOptions opts -> Maybe opts
parsedOptions (ParsedOptions x _ _) = x

-- | Get the subcommand action that was parsed from argv, or @Nothing@ if the
-- arguments could not be converted into a valid action.
--
-- Note: This function return @Nothing@ if the user provided a help flag. To
-- check whether an error occured during parsing, check the value of
-- @'parsedError'@.
parsedSubcommand :: ParsedSubcommand action -> Maybe action
parsedSubcommand (ParsedSubcommand x _ _) = x

-- | Get the error that prevented options from being parsed from argv,
-- or @Nothing@ if no error was detected.
parsedError :: Parsed a => a -> Maybe String
parsedError = parsedError_

-- | Get a help message to show the user. If the arguments included
-- a help flag, this will be a message appropriate to that flag.
-- Otherwise, it is a summary (equivalent to @--help@).
--
-- This is always a non-empty string, regardless of whether the parse
-- succeeded or failed. If you need to perform additional validation
-- on the options value, this message can be displayed if validation
-- fails.
parsedHelp :: Parsed a => a -> String
parsedHelp = parsedHelp_

-- | Attempt to convert a list of command-line arguments into an options
-- value. This can be used by application developers who want finer control
-- over error handling, or who want to perform additional validation on the
-- options value.
--
-- Use @'parsedOptions'@, @'parsedError'@, and @'parsedHelp'@ to inspect the
-- result of @'parseOptions'@.
--
-- Example:
--
-- @
--getOptionsOrDie :: Options a => IO a
--getOptionsOrDie = do
--    args <- System.Environment.getArgs
--    let parsed = 'parseOptions' args
--    case 'parsedOptions' parsed of
--        Just opts -> return opts
--        Nothing -> case 'parsedError' parsed of
--            Just err -> do
--                hPutStrLn stderr ('parsedHelp' parsed)
--                hPutStrLn stderr err
--                exitFailure
--            Nothing -> do
--                hPutStr stdout ('parsedHelp' parsed)
--                exitSuccess
-- @
parseOptions :: Options opts => [String] -> ParsedOptions opts
parseOptions args = parsed where
	defs = addHelpFlags optionsDefs
	help flag = helpFor flag defs Nothing
	parsed = case tokenize defs args of
		(_, Left err) -> ParsedOptions Nothing (Just err) (help HelpSummary)
		(_, Right tokens) -> case checkHelpFlag tokens of
			Just helpFlag -> ParsedOptions Nothing Nothing (help helpFlag)
			Nothing -> case optionsParse tokens of
				Left err -> ParsedOptions Nothing (Just err) (help HelpSummary)
				Right opts -> ParsedOptions (Just opts) Nothing (help HelpSummary)

-- | Retrieve 'System.Environment.getArgs', and attempt to parse it into a
-- valid value of an 'Options' type.
--
-- If parsing fails, this computation will print an error and call
-- 'exitFailure'.
--
-- If parsing succeeds, and the user has passed a @--help@ flag, and the
-- developer is using the default help flag definitions, then this computation
-- will print documentation and call 'exitSuccess'.
--
-- See 'runSubcommands' for details on subcommand support.
getOptionsOrDie :: (MonadIO m, Options a) => m a
getOptionsOrDie = do
	args <- liftIO System.Environment.getArgs
	let parsed = parseOptions args
	case parsedOptions parsed of
		Just opts -> return opts
		Nothing -> liftIO $ case parsedError parsed of
			Just err -> do
				hPutStrLn stderr (parsedHelp parsed)
				hPutStrLn stderr err
				exitFailure
			Nothing -> do
				hPutStr stdout (parsedHelp parsed)
				exitSuccess

-- | See 'runSubcommand'.
data Subcommand cmdOpts action = Subcommand String [OptionInfo] (TokensFor cmdOpts -> Either String action)

-- | See 'runSubcommand'.
subcommand :: (Options cmdOpts, Options subcmdOpts)
           => String -- ^ The subcommand name.
           -> (cmdOpts -> subcmdOpts -> [String] -> action) -- ^ The action to run.
           -> Subcommand cmdOpts action
subcommand name fn = Subcommand name opts checkTokens where
	opts = optInfosFromOptType fn optionsDefs
	
	optInfosFromOptType :: Options subcmdOpts => (cmdOpts -> subcmdOpts -> [String] -> action) -> OptionDefinitions subcmdOpts -> [OptionInfo]
	optInfosFromOptType _ (OptionDefinitions infos _) = infos
	
	checkTokens tokens = case optionsParse tokens of
		Left err -> Left err
		Right cmdOpts -> case optionsParse (castTokens tokens) of
			Left err -> Left err
			Right subcmdOpts -> case tokens of
				TokensFor _ args -> Right (fn cmdOpts subcmdOpts args)

subcommandInfo :: Subcommand cmdOpts action -> (String, [OptionInfo])
subcommandInfo (Subcommand name opts _) = (name, opts)

addSubcommands :: [Subcommand cmdOpts action] -> OptionDefinitions cmdOpts -> OptionDefinitions cmdOpts
addSubcommands subcommands defs = case defs of
	OptionDefinitions mainOpts subcmdOpts -> OptionDefinitions mainOpts (subcmdOpts ++ map subcommandInfo subcommands)

findSubcmd :: [Subcommand cmdOpts action] -> String -> TokensFor cmdOpts -> Either String action
findSubcmd subcommands name tokens = subcmd where
	asoc = [(n, cmd) | cmd@(Subcommand n _ _) <- subcommands]
	subcmd = case lookup name asoc of
		Nothing -> Left ("Unknown subcommand: " ++ show name)
		Just (Subcommand _ _ checkTokens) -> checkTokens tokens

-- | Attempt to convert a list of command-line arguments into a subcommand
-- action. This can be used by application developers who want finer control
-- over error handling, or who want subcommands that run in an unusual monad.
--
-- Use @'parsedSubcommand'@, @'parsedError'@, and @'parsedHelp'@ to inspect the
-- result of @'parseSubcommand'@.
--
-- Example:
--
-- @
--runSubcommand :: Options cmdOpts => [Subcommand cmdOpts (IO ())] -> IO ()
--runSubcommand subcommands = do
--    args <- System.Environment.getArgs
--    let parsed = 'parseSubcommand' subcommands args
--    case 'parsedSubcommand' parsed of
--        Just cmd -> cmd
--        Nothing -> case 'parsedError' parsed of
--            Just err -> do
--                hPutStrLn stderr ('parsedHelp' parsed)
--                hPutStrLn stderr err
--                exitFailure
--            Nothing -> do
--                hPutStr stdout ('parsedHelp' parsed)
--                exitSuccess
-- @
--
parseSubcommand :: Options cmdOpts => [Subcommand cmdOpts action] -> [String] -> ParsedSubcommand action
parseSubcommand subcommands args = parsed where
	defs = addHelpFlags (addSubcommands subcommands optionsDefs)
	help flag = helpFor flag defs
	parsed = case tokenize defs args of
		(subcmd, Left err) -> ParsedSubcommand Nothing (Just err) (help HelpSummary subcmd)
		(Nothing, Right tokens) -> case checkHelpFlag tokens of
			Just helpFlag -> ParsedSubcommand Nothing Nothing (help helpFlag Nothing)
			Nothing -> ParsedSubcommand Nothing (Just "No subcommand specified") (help HelpSummary Nothing)
		(Just subcmdName, Right tokens) -> case findSubcmd subcommands subcmdName tokens of
			Left err -> ParsedSubcommand Nothing (Just err) (help HelpSummary (Just subcmdName))
			Right io -> case checkHelpFlag tokens of
				Just helpFlag -> ParsedSubcommand Nothing Nothing (help helpFlag (Just subcmdName))
				Nothing -> ParsedSubcommand (Just io) Nothing (help HelpSummary (Just subcmdName))

-- | Used to run applications that are split into subcommands.
--
-- Use 'subcommand' to define available commands and their actions, then pass
-- them to this computation to select one and run it. If the user specifies
-- an invalid subcommand, this computation will print an error and call
-- 'exitFailure'. In handling of invalid flags or @--help@, 'runSubcommand'
-- acts like 'getOptionsOrDie'.
--
-- @
--'defineOptions' \"MainOptions\" $ do
--    'boolOption' \"optQuiet\" \"quiet\" False \"Whether to be quiet.\"
--
--'defineOptions' \"HelloOpts\" $ do
--    stringOption \"optHello\" \"hello\" \"Hello!\" \"How to say hello.\"
--
--'defineOptions' \"ByeOpts\" $ do
--    'stringOption' \"optName\" \"name\" \"\" \"The user's name.\"
--
--hello :: MainOptions -> HelloOpts -> [String] -> IO ()
--hello mainOpts opts args = unless (optQuiet mainOpts) $ do
--    putStrLn (optHello opts)
--
--bye :: MainOptions -> ByeOpts -> [String] -> IO ()
--bye mainOpts opts args = unless (optQuiet mainOpts) $ do
--    putStrLn (\"Good bye \" ++ optName opts)
--
--main :: IO ()
--main = 'runSubcommand'
--    [ 'subcommand' \"hello\" hello
--    , 'subcommand' \"bye\" bye
--    ]
-- @
--
-- >$ ./app hello
-- >Hello!
-- >$ ./app hello --hello='Allo!'
-- >Allo!
-- >$ ./app bye
-- >Good bye 
-- >$ ./app bye --name='John'
-- >Good bye John
runSubcommand :: (Options cmdOpts, MonadIO m) => [Subcommand cmdOpts (m ())] -> m ()
runSubcommand subcommands = do
	args <- liftIO System.Environment.getArgs
	let parsed = parseSubcommand subcommands args
	case parsedSubcommand parsed of
		Just cmd -> cmd
		Nothing -> liftIO $ case parsedError parsed of
			Just err -> do
				hPutStrLn stderr (parsedHelp parsed)
				hPutStrLn stderr err
				exitFailure
			Nothing -> do
				hPutStr stdout (parsedHelp parsed)
				exitSuccess
