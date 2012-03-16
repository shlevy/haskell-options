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
-- >{-# LANGUAGE TemplateHaskell #-}
-- >
-- >import Options
-- >
-- >defineOptions "MainOptions" $ do
-- >    stringOption "optMessage" "message" "Hello world!"
-- >      "A message to show the user."
-- >
-- >    boolOption "optQuiet" "quiet" False
-- >      "Whether to be quiet."
-- >
-- >main :: IO ()
-- >main = do
-- >    opts <- getOptionsOrDie
-- >    if optQuiet opts
-- >        then return ()
-- >        else putStrLn (optMessage opts)
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
	
	-- * Parsing options
	, getOptionsOrDie
	
	-- * Defining options
	, defineOptions
	
	-- ** Simple option definitions
	, stringOption
	, boolOption
	
	-- ** Using imported options
	, options
	
	-- ** Advanted option definitions
	, option
	
	, Option
	, optionShortFlags
	, optionLongFlags
	, optionDefault
	, optionType
	, optionDescription
	, optionGroup
	
	-- ** Option types
	, OptionType
	, optionTypeString
	, optionTypeBool
	, optionTypeEnum
	, optionTypeList
	
	-- * Option groups
	, Group
	, group
	, groupTitle
	, groupDescription
	
	-- * Subcommands
	, Subcommand
	, subcommand
	, runSubcommand
	) where

import           Control.Monad.IO.Class
import           Control.Monad.Reader
import           Control.Monad.Writer
import           Data.List (foldl')
import qualified System.Environment
import           System.Exit (exitFailure, exitSuccess)
import           System.IO

import           Language.Haskell.TH

import           Options.Types
import           Options.Tokenize
import           Options.Help

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

-- | An option's type determines how the option will be parsed, and which
-- Haskell type the parsed value will be stored as. There are many types
-- available, covering most basic types and a few more advanced types.
data OptionType a = OptionType Type Bool (Q Exp)

-- | Store an option in a String. The option is stored as-is, with no further
-- transformations.
optionTypeString :: OptionType String
optionTypeString = OptionType (ConT ''String) False [| Right |]

-- | Store an option in a Bool. The option's value must be either @\"true\"@
-- or @\"false\"@.
--
-- Boolean options are unary, which means that their value is optional when
-- specified on the command line. If a flag is present, the option is set to
-- True.
--
-- >./app -q
-- >./app --quiet
--
-- Boolean options may still be specified explicitly by using long flags with
-- the @--flag=value@ format. This is the only way to set a unary flag to
-- @\"false\"@.
--
-- >./app --quiet=true
-- >./app --quiet=false
optionTypeBool :: OptionType Bool
optionTypeBool = OptionType (ConT ''Bool) True [| \s -> case s of
	-- Use list notation so GHC 7.0 won't emit bogus warnings when the
	-- module defining options has an OverloadedStrings pragma.
	[] -> Right True
	['t', 'r', 'u', 'e'] -> Right True
	['f', 'a', 'l', 's', 'e'] -> Right False
	-- TODO: include option flag
	_ -> Left ("invalid boolean value: " ++ show s) |]

-- | Store an option as one of a set of enumerated constructors. The option
-- type must be defined in a separate file, otherwise the compiler will be
-- unable to use it when defining options.
--
-- >-- Types.hs
-- >data Mode = ModeFoo | ModeBar
-- >
-- >optionTypeMode :: OptionType Mode
-- >optionTypeMode = optionTypeEnum ''Mode
-- >    [ ("foo", 'ModeFoo)
-- >    , ("bar", 'ModeBar)
-- >    ]
--
-- >-- Main.hs
-- >import Types
-- >
-- >defineOptions "MainOptions" $ do
-- >    option "optMode" (\o -> o
-- >        { optionLongFlags = ["mode"]
-- >        , optionDefault = "foo"
-- >        , optionType = optionTypeMode
-- >        })
optionTypeEnum :: Name -> [(String, Name)] -> OptionType a
optionTypeEnum typeName values =
	-- TODO: check whether vName is a valid constructor name, and use either ConE or VarE
	let qExprs = return (ListE [TupE [LitE (StringL key), ConE vName] | (key, vName) <- values]) in
	OptionType (ConT typeName) False
		[| let exprs = $qExprs in \s -> case lookup s exprs of
			Just v -> Right v
			-- TODO: include option flag and available values
			Nothing -> Left ("invalid enum value: " ++ show s) |]

-- | Store an option as a list, using another option type as the elements.
-- The separator should be a character that will not occur within the values,
-- such as a comma or semicolon.
--
-- >    option "optNames" (\o -> o
-- >        { optionLongFlags = ["names"]
-- >        , optionDefault = "Alice;Bob;Charles"
-- >        , optionType = optionTypeList ';' optionTypeString
-- >        })
optionTypeList :: Char -- ^ Separator
               -> OptionType a -- ^ Element type
               -> OptionType [a]
optionTypeList sep (OptionType valType _ valParseExp) = OptionType (AppT ListT valType) False [| \s -> parseList $valParseExp (split sep s) |]

parseList :: (String -> Either String a) -> [String] -> Either String [a]
parseList p = loop where
	loop [] = Right []
	loop (x:xs) = case p x of
		Left err -> Left err
		Right v -> case loop xs of
			Left err -> Left err
			Right vs -> Right (v:vs)

split :: Char -> String -> [String]
split _ [] = []
split sep s0 = loop s0 where
	loop s = let
		(chunk, rest) = break (== sep) s
		cont = chunk : loop (tail rest)
		in if null rest then [chunk] else cont

-- | See 'option'.
data Option a = Option
	{
	-- | Short flags are a single character. When entered by a user,
	-- they are preceded by a dash and possibly other short flags.
	--
	-- Short flags may not be @\'-\'@, non-printable, or whitespace.
	--
	-- Example: An option with @optionShortFlags = [\'n\']@ may be set using:
	--
	-- >./app -n John
	-- >./app -nJohn
	  optionShortFlags :: [Char]
	
	-- | Long flags are multiple characters. When entered by a user, they
	-- are preceded by two dashes.
	--
	-- Long flags may not contain @\'=\'@, non-printable characters, or
	-- whitespace characters.
	--
	-- Example: An option with @optionLongFlags = [\"name\"]@ may be set using:
	--
	-- >./app --name John
	-- >./app --name=John
	, optionLongFlags :: [String]
	
	-- | Options may have a default value. This will be parsed as if the
	-- user had entered it on the command line.
	, optionDefault :: String
	
	-- | There are many types which an application or library might want
	-- to use when designing their options. By default, options are
	-- strings, but 'optionType' may be set to any supported type. See
	-- the \"Option types\" section for a list of supported types.
	, optionType :: OptionType a
	
	-- | An option's description is used with the default implementation
	-- of @--help@. It should be a short string describing what the option
	-- does.
	, optionDescription :: String
	
	-- | Which group the option is in. See the \"Option groups\" section
	-- for details.
	, optionGroup :: Group
	}

newtype OptionsM a = OptionsM { unOptionsM :: ReaderT Loc (Writer [(Name, Type, Q Exp, Q Exp)]) a }

instance Monad OptionsM where
	return = OptionsM . return
	m >>= f = OptionsM (unOptionsM m >>= (unOptionsM . f))

runOptionsM :: Loc -> OptionsM () -> [(Name, Type, Q Exp, Q Exp)]
runOptionsM loc (OptionsM m) = execWriter (runReaderT m loc)

-- | Defines a new data type, containing fields for application or library
-- options. The new type will be an instance of 'Options'.
--
-- Example: this use of @defineOptions@:
--
-- >defineOptions "MainOptions" $ do
-- >    stringOption "optMessage" "message" "Hello world!" ""
-- >    boolOption "optQuiet" "quiet" False ""
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
	let fields = runOptionsM loc optionsM
	
	-- TODO: check 'fields' for duplicate field names
	-- TODO: check 'fields' for duplicate keys (should be impossible)
	-- TODO: check 'fields' for duplicate flags
	
	let dataDec = DataD [] dataName [] [RecC dataName
		[(fName, NotStrict, t) | (fName, t, _, _) <- fields]
		][]
	
	exp_optionsDefs <- getOptionsDefs fields
	exp_optionsParse <- getOptionsParse dataName fields
	let instanceDec = InstanceD [] (AppT (ConT ''Options) (ConT dataName))
		[ ValD (VarP 'optionsDefs) (NormalB exp_optionsDefs) []
		, ValD (VarP 'optionsParse) (NormalB exp_optionsParse) []
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

newtype ParserM optType a = ParserM { unParserM :: TokensFor optType -> Either String a }

instance Monad (ParserM optType) where
	return x = ParserM (\_ -> Right x)
	m >>= f = ParserM (\env -> case unParserM m env of
		Left err -> Left err
		Right x -> unParserM (f x) env)

putOptionDecl :: Name -> Type -> Q Exp -> Q Exp -> OptionsM ()
putOptionDecl name qtype infoExp parseExp = OptionsM (tell [(name, qtype, infoExp, parseExp)])

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
-- >option "optFoo" (\o -> o
-- >    { optionLongFlags = ["names"]
-- >    , optionType = optionTypeList ',' optionTypeString
-- >    }
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
	
	-- TODO: check that 'fieldName' is a valid Haskell field name
	-- TODO: check that 'shorts' contains only non-repeated ASCII letters
	--       and digits
	-- TODO: check that 'longs' contains only non-repeated, non-empty
	--       strings containing [A-Z] [a-z] [0-9] - _ and starting with a
	--       letter or digit.
	-- TODO: check that at least one flag is defined (in either 'shorts'
	--       or 'longs').
	
	let OptionType thType unary parseExp = optionType opt
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

-- | Include options defined elsewhere into the current options definition.
--
-- This is typically used by application developers to include options defined
-- in third-party libraries. For example, the author of the \"foo\" library
-- would define and export @FooOptions@:
--
-- >module Foo (FooOptions, foo) where
-- >
-- >import Options
-- >
-- >defineOptions "FooOptions" $ do
-- >    boolOption "optFrob" "frob" True "Enable frobnication."
-- >
-- >foo :: FooOptions -> IO ()
--
-- and the author of an application would use @options@ to let users specify
-- @--frob@:
--
-- >module Main where
-- >
-- >import Options
-- >import Foo
-- >
-- >defineOptions "MainOptions" $ do
-- >   boolOption "optVerbose" "verbose" False "Be really loud."
-- >   options "optFoo" ''FooOptions
-- >
-- >main :: IO ()
-- >main = do
-- >    opts <- getOptionsOrDie
-- >    foo (optFoo opts)
--
-- Use of 'options' may be arbitrarily nested. Library authors are encouraged
-- to aggregate their options into a single top-level type, so application
-- authors can include it easily in their own option definitions.
options :: String -> Name -> OptionsM ()
options fieldName optionTypeName = do
	-- TODO: check that 'fieldName' is a valid Haskell field name
	putOptionDecl
		(mkName fieldName)
		(ConT optionTypeName)
		[| suboptsDefs $(varE (mkName fieldName)) |]
		[| parseSubOptions |]

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

-- | Define an option of type @String@. This is a simple wrapper around
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

-- | Define an option of type @Bool@. This is a simple wrapper around
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

-- | See 'group'.
data Group = Group
	{ groupName :: Maybe String
	
	-- | A short title for the group, which is used when printing
	-- @--help@ output.
	, groupTitle :: String
	
	-- | A description of the group, which is used when printing
	-- @--help@ output.
	, groupDescription :: String
	}

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
	let defs = addHelpFlags optionsDefs
	case tokenize defs args of
		(_, Left err) -> liftIO $ do
			hPutStr stderr (helpFor HelpSummary defs Nothing)
			hPutStrLn stderr err
			exitFailure
		(_, Right tokens) -> case optionsParse tokens of
			Left err -> liftIO $ do
				hPutStr stderr (helpFor HelpSummary defs Nothing)
				hPutStrLn stderr err
				exitFailure
			Right opts -> case checkHelpFlag tokens of
				Just helpFlag -> liftIO $ do
					hPutStr stdout (helpFor helpFlag defs Nothing)
					exitSuccess
				Nothing -> return opts

-- | See 'runSubcommand'.
data Subcommand cmdOpts m = Subcommand String [OptionInfo] (TokensFor cmdOpts -> Either String (m ()))

-- | See 'runSubcommand'.
subcommand :: (Options cmdOpts, Options subcmdOpts)
           => String -- ^ The subcommand name.
           -> (cmdOpts -> subcmdOpts -> [String] -> m ()) -- ^ The action to run.
           -> Subcommand cmdOpts m
subcommand name fn = Subcommand name opts checkTokens where
	opts = optInfosFromOptType fn optionsDefs
	
	optInfosFromOptType :: Options subcmdOpts => (cmdOpts -> subcmdOpts -> [String] -> m ()) -> OptionDefinitions subcmdOpts -> [OptionInfo]
	optInfosFromOptType _ (OptionDefinitions infos _) = infos
	
	checkTokens tokens = case optionsParse tokens of
		Left err -> Left err
		Right cmdOpts -> case optionsParse (castTokens tokens) of
			Left err -> Left err
			Right subcmdOpts -> case tokens of
				TokensFor _ args -> Right (fn cmdOpts subcmdOpts args)

subcommandInfo :: Subcommand cmdOpts m -> (String, [OptionInfo])
subcommandInfo (Subcommand name opts _) = (name, opts)

addSubcommands :: [Subcommand cmdOpts m] -> OptionDefinitions cmdOpts -> OptionDefinitions cmdOpts
addSubcommands subcommands defs = case defs of
	OptionDefinitions mainOpts subcmdOpts -> OptionDefinitions mainOpts (subcmdOpts ++ map subcommandInfo subcommands)

findSubcmd :: [Subcommand cmdOpts m] -> String -> TokensFor cmdOpts -> Either String (m ())
findSubcmd subcommands name tokens = subcmd where
	asoc = [(n, cmd) | cmd@(Subcommand n _ _) <- subcommands]
	subcmd = case lookup name asoc of
		Nothing -> Left ("Unknown subcommand: " ++ show name)
		Just (Subcommand _ _ checkTokens) -> checkTokens tokens

-- | Used to run applications that are split into subcommands.
--
-- Use 'subcommand' to define available commands and their actions, then pass
-- them to this computation to select one and run it. If the user specifies
-- an invalid subcommand, this computation will print an error and call
-- 'exitFailure'. In handling of invalid flags or @--help@, 'runSubcommand'
-- acts like 'getOptionsOrDie'.
--
-- >defineOptions "MainOptions" $ do
-- >    boolOption "optQuiet" "quiet" False "Whether to be quiet."
-- >
-- >defineOptions "HelloOpts" $ do
-- >    stringOption "optHello" "hello" "Hello!" "How to say hello."
-- >
-- >defineOptions "ByeOpts" $ do
-- >    stringOption "optName" "name" "" "The user's name."
-- >
-- >hello :: MainOptions -> HelloOpts -> [String] -> IO ()
-- >hello mainOpts opts args = unless (optQuiet mainOpts) $ do
-- >    putStrLn (optHello opts)
-- >
-- >bye :: MainOptions -> ByeOpts -> [String] -> IO ()
-- >bye mainOpts opts args = unless (optQuiet mainOpts) $ do
-- >    putStrLn ("Good bye " ++ optName opts)
-- >
-- >main :: IO ()
-- >main = runSubcommands
-- >    [ subcommand "hello" hello
-- >    , subcommand "bye" bye
-- >    ]
--
-- >$ ./app hello
-- >Hello!
-- >$ ./app hello --hello='Allo!'
-- >Allo!
-- >$ ./app bye
-- >Good bye 
-- >$ ./app bye --name='John'
-- >Good bye John
runSubcommand :: (Options cmdOpts, MonadIO m) => [Subcommand cmdOpts m] -> m ()
runSubcommand subcommands = do
	args <- liftIO System.Environment.getArgs
	let defs = addHelpFlags (addSubcommands subcommands optionsDefs)
	case tokenize defs args of
		(subcmd, Left err) -> liftIO $ do
			hPutStr stderr (helpFor HelpSummary defs subcmd)
			hPutStrLn stderr err
			exitFailure
		(Nothing, Right tokens) -> case checkHelpFlag tokens of
			Just helpFlag -> liftIO $ do
				hPutStr stdout (helpFor helpFlag defs Nothing)
				exitSuccess
			Nothing -> liftIO $ do
				hPutStr stderr (helpFor HelpSummary defs Nothing)
				hPutStrLn stderr "No subcommand specified"
				exitFailure
		(Just subcmdName, Right tokens) -> do
			case findSubcmd subcommands subcmdName tokens of
				Left err -> liftIO $ do
					hPutStr stderr (helpFor HelpSummary defs (Just subcmdName))
					hPutStrLn stderr err
					exitFailure
				Right io -> case checkHelpFlag tokens of
					Just helpFlag -> liftIO $ do
						hPutStr stdout (helpFor helpFlag defs (Just subcmdName))
						exitSuccess
					Nothing -> io
