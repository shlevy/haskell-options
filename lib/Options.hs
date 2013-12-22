{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}

-- |
-- Module: Options
-- License: MIT
--
-- The @options@ package lets library and application developers easily work
-- with command-line options.
--
-- The following example is a full program that can accept two options,
-- @--message@ and @--quiet@:
--
-- @
--import Control.Applicative
--import Options
--
--data MainOptions = MainOptions
--    { optMessage :: String
--    , optQuiet :: Bool
--    }
--
--instance 'Options' MainOptions where
--    'defineOptions' = pure MainOptions
--        \<*\> 'defineOption' \"message\" \"Hello world!\"
--            \"A message to show the user.\"
--        \<*\> 'defineOption' \"quiet\" False
--            \"Whether to be quiet.\"
--
--main :: IO ()
--main = 'runCommand' $ \\opts args -> do
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
	-- * Options
	  Options(..)
	, defaultOptions
	
	-- * Commands and Subcommands
	, runCommand
	, Subcommand
	, subcommand
	, runSubcommand
	
	-- * Defining options
	, DefineOptions
	, defineOption
	, IsOptionType
	
	-- ** Advanced option definitions
	, OptionType
	, defineOptionWith
	, Option
	, optionShortFlags
	, optionLongFlags
	, optionDefault
	, optionDescription
	, optionGroup
	
	-- ** Built-in option types
	, optionType_bool
	
	, optionType_string
	
	, optionType_int
	, optionType_int8
	, optionType_int16
	, optionType_int32
	, optionType_int64
	, optionType_word
	, optionType_word8
	, optionType_word16
	, optionType_word32
	, optionType_word64
	, optionType_integer
	
	, optionType_float
	, optionType_double
	
	, optionType_maybe
	, optionType_list
	, optionType_set
	, optionType_map
	, optionType_enum
	
	-- ** Defining custom option types
	, optionType
	, optionTypeName
	, optionTypeDefault
	, optionTypeParse
	, optionTypeShow
	, optionTypeUnary
	
	-- * Option groups
	, Group
	, groupName
	, groupTitle
	, groupDescription
	
	-- * Parsing argument lists
	, Parsed
	, parsedError
	, parsedHelp
	
	-- ** Parsing options
	, ParsedOptions
	, parsedOptions
	, parsedArguments
	, parseOptions
	
	-- ** Parsing subcommands
	, ParsedSubcommand
	, parsedSubcommand
	, parseSubcommand
	) where

import           Control.Applicative
import           Control.Monad.IO.Class (liftIO, MonadIO)
import qualified Data.Map as Map
import           Data.Maybe (isJust)
import qualified System.Environment
import           System.Exit (exitFailure, exitSuccess)
import           System.IO (hPutStr, hPutStrLn, stderr, stdout)

import           Options.Help
import           Options.OptionTypes
import           Options.Tokenize
import           Options.Types

-- | Options are defined together in a single data type, which will be an
-- instance of 'Options'.
--
-- See 'defineOptions' for details on defining instances of 'Options'.
--
-- See 'includeOptions' for details on including imported 'Options' types in
-- locally defined options.
class Options opts where
	-- | TODO docs
	--
	-- Use of 'options' may be arbitrarily nested. Library authors are encouraged
	-- to aggregate their options into a single top-level type, so application
	-- authors can include it easily in their own option definitions.
	defineOptions :: DefineOptions opts

data DefineOptions a = DefineOptions a (Integer -> (Integer, [OptionInfo])) (Integer -> Map.Map OptionKey Token -> Either String (Integer, a))

instance Functor DefineOptions where
	fmap fn (DefineOptions defaultValue getInfo parse) = DefineOptions (fn defaultValue) getInfo (\key tokens -> case parse key tokens of
		Left err -> Left err
		Right (key', a) -> Right (key', fn a))

instance Applicative DefineOptions where
	pure a = DefineOptions a (\key -> (key, [])) (\key _ -> Right (key, a))
	(DefineOptions acc_default acc_getInfo acc_parse) <*> (DefineOptions defaultValue getInfo parse) = DefineOptions
		(acc_default defaultValue)
		(\key -> case acc_getInfo key of
			(key', infos) -> case getInfo key' of
				(key'', infos') -> (key'', infos ++ infos'))
		(\key tokens -> case acc_parse key tokens of
			Left err -> Left err
			Right (key', fn) -> case parse key' tokens of
				Left err -> Left err
				Right (key'', a) -> Right (key'', fn a))

-- | An options value containing only the default values for each option.
-- This is equivalent to the options value when parsing an empty argument
-- list.
defaultOptions :: Options opts => opts
defaultOptions = case defineOptions of
	(DefineOptions def _ _) -> def

class IsOptionType a where
	builtinOptionType :: OptionType a

instance IsOptionType Bool where
	builtinOptionType = optionType_bool

instance IsOptionType a => IsOptionType (Maybe a) where
	builtinOptionType = optionType_maybe builtinOptionType

instance IsOptionType String where
	builtinOptionType = optionType_string

instance IsOptionType Integer where
	builtinOptionType = optionType_integer

defineOption :: IsOptionType a
             => String -- long flag
             -> a -- default value
             -> String -- description
             -> DefineOptions a
defineOption flag def desc = defineOptionWith builtinOptionType (\o -> o
	{ optionLongFlags = [flag]
	, optionDefault = def
	, optionDescription = desc
	})

-- | Defines a new option in the current options type.
--
-- All options must have a one or more /flags/. Options may also have a
-- default value, a description, and a group.
--
-- The /flags/ are how the user specifies an option on the command line. Flags
-- may be /short/ or /long/. See 'optionShortFlags' and 'optionLongFlags' for
-- details.
--
-- @
--'defineOptionWith' 'optionType_word16' (\\o -> o
--    { 'optionLongFlags' = [\"port\"]
--    , 'optionDefault' = \"80\"
--    })
-- @
defineOptionWith :: OptionType a -> (Option a -> Option a) -> DefineOptions a
defineOptionWith t fn = DefineOptions (optionDefault opt) getInfo parser where
	opt = fn (Option
		{ optionShortFlags = []
		, optionLongFlags = []
		, optionDefault = optionTypeDefault t
		, optionDescription = ""
		, optionGroup = Nothing
		, optionLocation = Nothing
		})
	
	getInfo key = (key+1, [OptionInfo
		{ optionInfoKey = OptionKeyGenerated key
		, optionInfoShortFlags = optionShortFlags opt
		, optionInfoLongFlags = optionLongFlags opt
		, optionInfoDefault = optionTypeShow t (optionDefault opt)
		, optionInfoDescription = optionDescription opt
		, optionInfoGroup = optionGroup opt
		, optionInfoLocation = optionLocation opt
		, optionInfoTypeName = optionTypeName t
		, optionInfoUnary = isJust (optionTypeUnary t)
		}])
	
	parser key tokens = case Map.lookup (OptionKeyGenerated key) tokens of
		Nothing -> Right (key+1, optionDefault opt)
		Just tok -> case tok of
			TokenUnary flagName -> case optionTypeUnary t of
				Nothing -> Left ("The flag " ++ flagName ++ " requires an argument.")
				Just val -> Right (key+1, val)
			Token flagName rawValue -> case optionTypeParse t rawValue of
				Left err -> Left ("Value for flag " ++ flagName ++ " is invalid: " ++ err)
				Right val -> Right (key+1, val)

data Option a = Option
	{
	-- | Short flags are a single character. When entered by a user,
	-- they are preceded by a dash and possibly other short flags.
	--
	-- Short flags must be a letter or a number.
	--
	-- Example: An option with @optionShortFlags = [\'p\']@ may be set using:
	--
	-- >$ ./app -p 443
	-- >$ ./app -p443
	  optionShortFlags :: [Char]
	
	-- | Long flags are multiple characters. When entered by a user, they
	-- are preceded by two dashes.
	--
	-- Long flags may contain letters, numbers, @\'-\'@, and @\'_\'@.
	--
	-- Example: An option with @optionLongFlags = [\"port\"]@ may be set using:
	--
	-- >$ ./app --port 443
	-- >$ ./app --port=443
	, optionLongFlags :: [String]
	
	-- | Options may have a default value. This will be parsed as if the
	-- user had entered it on the command line.
	, optionDefault :: a
	
	-- | An option's description is used with the default implementation
	-- of @--help@. It should be a short string describing what the option
	-- does.
	, optionDescription :: String
	
	-- | Which group the option is in. See the \"Option groups\" section
	-- for details.
	, optionGroup :: Maybe Group
	
	-- | TODO docs
	, optionLocation :: Maybe Location
	}

-- * Each option defines at least one short or long flag.
-- * There are no duplicate short or long flags, except between subcommands.
-- * All short or long flags have a reasonable name.
-- * All subcommands have unique names.
validateOptionDefs :: [OptionInfo] -> [(String, [OptionInfo])] -> Either String OptionDefinitions
validateOptionDefs cmdInfos subInfos = Right (addHelpFlags (OptionDefinitions cmdInfos subInfos)) -- TODO

-- | See @'parseOptions'@ and @'parseSubcommand'@.
class Parsed a where
	parsedError_ :: a -> Maybe String
	parsedHelp_ :: a -> String

-- | See @'parseOptions'@.
data ParsedOptions opts = ParsedOptions (Maybe opts) (Maybe String) String [String]

-- | See @'parseSubcommand'@.
data ParsedSubcommand action = ParsedSubcommand (Maybe action) (Maybe String) String

instance Parsed (ParsedOptions a) where
	parsedError_ (ParsedOptions _ x _ _) = x
	parsedHelp_ (ParsedOptions _ _ x _) = x

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
parsedOptions (ParsedOptions x _ _ _) = x

-- | Get command-line arguments remaining after parsing options. The arguments
-- are unchanged from the original argument list, and have not been decoded
-- or otherwise transformed.
parsedArguments :: ParsedOptions opts -> [String]
parsedArguments (ParsedOptions _ _ _ x) = x

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
-- The argument list must be in the same encoding as the result of
-- 'System.Environment.getArgs'.
--
-- Use @'parsedOptions'@, @'parsedArguments'@, @'parsedError'@, and
-- @'parsedHelp'@ to inspect the result of @'parseOptions'@.
--
-- Example:
--
-- @
--getOptionsOrDie :: Options a => IO a
--getOptionsOrDie = do
--    argv <- System.Environment.getArgs
--    let parsed = 'parseOptions' argv
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
parseOptions argv = parsed where
	(DefineOptions _ getInfos parser) = defineOptions
	(_, optionInfos) = getInfos 0
	parseTokens = parser 0
	
	parsed = case validateOptionDefs optionInfos [] of
		Left err -> ParsedOptions Nothing (Just err) "" []
		Right optionDefs -> case tokenize (addHelpFlags optionDefs) argv of
			(_, Left err) -> ParsedOptions Nothing (Just err) (helpFor HelpSummary optionDefs Nothing) []
			(_, Right tokens) -> case checkHelpFlag tokens of
				Just helpFlag -> ParsedOptions Nothing Nothing (helpFor helpFlag optionDefs Nothing) []
				Nothing -> case parseTokens (tokensMap tokens) of
					Left err -> ParsedOptions Nothing (Just err) (helpFor HelpSummary optionDefs Nothing) []
					Right (_, opts) -> ParsedOptions (Just opts) Nothing (helpFor HelpSummary optionDefs Nothing) (tokensArgv tokens)

-- | Retrieve 'System.Environment.getArgs', and attempt to parse it into a
-- valid value of an 'Options' type plus a list of left-over arguments. The
-- options and arguments are then passed to the provided computation.
--
-- If parsing fails, this computation will print an error and call
-- 'exitFailure'.
--
-- If parsing succeeds, and the user has passed a @--help@ flag, and the
-- developer is using the default help flag definitions, then this computation
-- will print documentation and call 'exitSuccess'.
--
-- See 'runSubcommand' for details on subcommand support.
runCommand :: (MonadIO m, Options opts) => (opts -> [String] -> m a) -> m a
runCommand io = do
	argv <- liftIO System.Environment.getArgs
	let parsed = parseOptions argv
	case parsedOptions parsed of
		Just opts -> io opts (parsedArguments parsed)
		Nothing -> liftIO $ case parsedError parsed of
			Just err -> do
				hPutStrLn stderr (parsedHelp parsed)
				hPutStrLn stderr err
				exitFailure
			Nothing -> do
				hPutStr stdout (parsedHelp parsed)
				exitSuccess

data Subcommand cmdOpts action = Subcommand String (Integer -> ([OptionInfo], (cmdOpts -> Tokens -> Either String action), Integer))

subcommand :: (Options cmdOpts, Options subcmdOpts)
           => String -- ^ The subcommand name.
           -> (cmdOpts -> subcmdOpts -> [String] -> action) -- ^ The action to run.
           -> Subcommand cmdOpts action
subcommand name fn = Subcommand name (\initialKey -> let
	(DefineOptions _ getInfos parser) = defineOptions
	(nextKey, optionInfos) = getInfos initialKey
	parseTokens = parser initialKey
	
	runAction cmdOpts tokens = case parseTokens (tokensMap tokens) of
		Left err -> Left err
		Right (_, subOpts) -> Right (fn cmdOpts subOpts (tokensArgv tokens))
	in (optionInfos, runAction, nextKey))

-- | Attempt to convert a list of command-line arguments into a subcommand
-- action. This can be used by application developers who want finer control
-- over error handling, or who want subcommands that run in an unusual monad.
--
-- The argument list must be in the same encoding as the result of
-- 'System.Environment.getArgs'.
--
-- Use @'parsedSubcommand'@, @'parsedError'@, and @'parsedHelp'@ to inspect the
-- result of @'parseSubcommand'@.
--
-- Example:
--
-- @
--runSubcommand :: Options cmdOpts => [Subcommand cmdOpts (IO a)] -> IO a
--runSubcommand subcommands = do
--    argv <- System.Environment.getArgs
--    let parsed = 'parseSubcommand' subcommands argv
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
parseSubcommand subcommands argv = parsed where
	(DefineOptions _ getInfos parser) = defineOptions
	(cmdNextKey, cmdInfos) = getInfos 0
	cmdParseTokens = parser 0
	
	subcmdInfos = do
		Subcommand name fn <- subcommands
		let (infos, _, _) = fn cmdNextKey
		return (name, infos)
	
	subcmdRunners = Map.fromList $ do
		Subcommand name fn <- subcommands
		let (_, runner, _) = fn cmdNextKey
		return (name, runner)
	
	parsed = case validateOptionDefs cmdInfos subcmdInfos of
		Left err -> ParsedSubcommand Nothing (Just err) ""
		Right optionDefs -> case tokenize (addHelpFlags optionDefs) argv of
			(subcmd, Left err) -> ParsedSubcommand Nothing (Just err) (helpFor HelpSummary optionDefs subcmd)
			(subcmd, Right tokens) -> case checkHelpFlag tokens of
				Just helpFlag -> ParsedSubcommand Nothing Nothing (helpFor helpFlag optionDefs subcmd)
				Nothing -> case findAction tokens subcmd of
					Left err -> ParsedSubcommand Nothing (Just err) (helpFor HelpSummary optionDefs subcmd)
					Right action -> ParsedSubcommand (Just action) Nothing (helpFor HelpSummary optionDefs subcmd)
	
	findAction _ Nothing = Left "No subcommand specified"
	findAction tokens (Just subcmdName) = case cmdParseTokens (tokensMap tokens) of
		Left err -> Left err
		Right (_, cmdOpts) -> case Map.lookup subcmdName subcmdRunners of
			Nothing -> Left ("Unknown subcommand " ++ show subcmdName ++ ".")
			Just getRunner -> case getRunner cmdOpts tokens of
				Left err -> Left err
				Right action -> Right action

-- | Used to run applications that are split into subcommands.
--
-- Use 'subcommand' to define available commands and their actions, then pass
-- them to this computation to select one and run it. If the user specifies
-- an invalid subcommand, this computation will print an error and call
-- 'exitFailure'. In handling of invalid flags or @--help@, 'runSubcommand'
-- acts like 'runCommand'.
--
-- @
--import Control.Applicative
--import Control.Monad (unless)
--import Options
--
--data MainOptions = MainOptions { optQuiet :: Bool }
--instance 'Options' MainOptions where
--    'defineOptions' = pure MainOptions
--        \<*\> 'defineOption' \"quiet\" False \"Whether to be quiet.\"
--
--data HelloOpts = HelloOpts { optHello :: String }
--instance 'Options' HelloOpts where
--    'defineOptions' = pure HelloOpts
--        \<*\> 'defineOption' \"hello\" \"Hello!\" \"How to say hello.\"
--
--data ByeOpts = ByeOpts { optName :: String }
--instance 'Options' ByeOpts where
--    'defineOptions' = pure ByeOpts
--        \<*\> 'defineOption' \"name\" \"\" \"The user's name.\"
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
-- >$ ./app bye --name='Alice'
-- >Good bye Alice
runSubcommand :: (Options opts, MonadIO m) => [Subcommand opts (m a)] -> m a
runSubcommand subcommands = do
	argv <- liftIO System.Environment.getArgs
	let parsed = parseSubcommand subcommands argv
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
