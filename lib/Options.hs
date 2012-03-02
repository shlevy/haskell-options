{-# LANGUAGE TemplateHaskell #-}

-- |
-- Module: Options
-- License: MIT
--
-- TODO: documentation here
module Options
	( Options
	, OptionType
	, optionTypeString
	, optionTypeBool
	, Option
	, optionShortFlags
	, optionLongFlags
	, optionDefault
	, optionType
	, defineOptions
	, option
	, options
	, stringOption
	, boolOption
	, getOptionsOrDie
	) where

import           Control.Monad.IO.Class
import           Control.Monad.Reader
import           Control.Monad.Writer
import           Data.List (foldl')
import qualified System.Environment
import           System.Exit (exitFailure)
import           System.IO

import           Language.Haskell.TH

import           Options.Types
import           Options.Tokenize
-- import           Options.Help

class Options a where
	optionsDefs :: OptionDefinitions a
	optionsParse :: TokensFor a -> Either String a -- TODO: OptionError

data OptionType a = OptionType Type Bool (Q Exp)

optionTypeString :: OptionType String
optionTypeString = OptionType (ConT ''String) False [| Right |]

optionTypeBool :: OptionType Bool
optionTypeBool = OptionType (ConT ''Bool) True [| \s -> case s of
	"" -> Right True
	"true" -> Right True
	"false" -> Right False
	-- TODO: include option flag
	_ -> Left ("invalid boolean value: " ++ show s) |]

data Option a = Option
	{ optionShortFlags :: [Char]
	, optionLongFlags :: [String]
	, optionDefault :: String
	, optionType :: OptionType a
	}

newtype OptionsM a = OptionsM { unOptionsM :: ReaderT Loc (Writer [(Name, Type, Q Exp, Q Exp)]) a }

instance Monad OptionsM where
	return = OptionsM . return
	m >>= f = OptionsM (unOptionsM m >>= (unOptionsM . f))

runOptionsM :: Loc -> OptionsM () -> [(Name, Type, Q Exp, Q Exp)]
runOptionsM loc (OptionsM m) = execWriter (runReaderT m loc)

defineOptions :: String -> OptionsM () -> Q [Dec]
defineOptions rawName optionsM = do
	loc <- location
	let dataName = mkName rawName
	let fields = runOptionsM loc optionsM
	
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
	[| OptionDefinitions $(return (ListE infoExps)) [] |] -- TODO: subcommands

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

option :: String -> (Option String -> Option a) -> OptionsM ()
option fieldName f = do
	let opt = f (Option
		{ optionShortFlags = []
		, optionLongFlags = []
		, optionDefault = ""
		, optionType = optionTypeString
		})
	
	-- TODO: should options data type name be part of the key?
	loc <- OptionsM ask
	let key = loc_package loc ++ ":" ++ loc_module loc ++ ":" ++ fieldName
	
	let shorts = optionShortFlags opt
	let longs = optionLongFlags opt
	let def = optionDefault opt
	
	let OptionType thType unary parseExp = optionType opt
	putOptionDecl
		(mkName fieldName)
		thType
		[| OptionInfo key shorts longs def unary |]
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

options :: String -> Name -> OptionsM ()
options attrName optionTypeName = do
	-- TODO: add a field (attrName :: optionTypeName)
	undefined

stringOption :: String -> [Char] -> [String] -> String -> OptionsM ()
stringOption name shorts longs def = option name (\o -> o
	{ optionShortFlags = shorts
	, optionLongFlags = longs
	, optionDefault = def
	})

boolOption :: String -> [Char] -> [String] -> Bool -> OptionsM ()
boolOption name shorts longs def = option name (\o -> o
	{ optionShortFlags = shorts
	, optionLongFlags = longs
	, optionDefault = if def then "true" else "false"
	, optionType = optionTypeBool
	})

getOptionsOrDie :: (MonadIO m, Options a) => m a
getOptionsOrDie = do
	args <- liftIO System.Environment.getArgs
	case tokenize optionsDefs args of
		-- TODO: subcommands
		-- TODO: --help
		(_, Left err) -> liftIO $ do
			hPutStrLn stderr err
			exitFailure
		(_, Right tokens) -> case optionsParse tokens of
			Left err -> liftIO $ do
				hPutStrLn stderr err
				exitFailure
			Right opts -> return opts
