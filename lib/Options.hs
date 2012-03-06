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
	, optionTypeEnum
	, optionTypeList
	, Option
	, optionShortFlags
	, optionLongFlags
	, optionDefault
	, optionType
	, optionDescription
	, optionGroup
	, defineOptions
	, option
	, options
	, stringOption
	, boolOption
	, Group
	, group
	, groupDescription
	, groupHelpDescription
	, getOptionsOrDie
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

optionTypeEnum :: Name -> [(String, Name)] -> OptionType a
optionTypeEnum typeName values =
	-- TODO: check whether vName is a valid constructor name, and use either ConE or VarE
	let qExprs = return (ListE [TupE [LitE (StringL key), ConE vName] | (key, vName) <- values]) in
	OptionType (ConT typeName) False
		[| let exprs = $qExprs in \s -> case lookup s exprs of
			Just v -> Right v
			-- TODO: include option flag and available values
			Nothing -> Left ("invalid enum value: " ++ show s) |]

optionTypeList :: Char -> OptionType a -> OptionType [String]
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

data Option a = Option
	{ optionShortFlags :: [Char]
	, optionLongFlags :: [String]
	, optionDefault :: String
	, optionType :: OptionType a
	, optionDescription :: String
	, optionGroup :: Group
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
	let emptyGroup = Group
		{ groupName = Nothing
		, groupDescription = ""
		, groupHelpDescription = ""
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
	let optGroupDesc = groupDescription optGroup
	let optGroupHelpDesc = groupHelpDescription optGroup
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
		[| OptionInfo key shorts longs def unary desc $groupInfoExp |]
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

stringOption :: String -> String -> String -> String -> OptionsM ()
stringOption name flag def desc = option name (\o -> o
	{ optionLongFlags = [flag]
	, optionDefault = def
	, optionDescription = desc
	})

boolOption :: String -> String -> Bool -> String -> OptionsM ()
boolOption name flag def desc = option name (\o -> o
	{ optionLongFlags = [flag]
	, optionDefault = if def then "true" else "false"
	, optionType = optionTypeBool
	, optionDescription = desc
	})

data Group = Group
	{ groupName :: Maybe String
	, groupDescription :: String
	, groupHelpDescription :: String
	}

group :: String -> (Group -> Group) -> Group
group name f = f (Group
	{ groupName = Just name
	, groupDescription = ""
	, groupHelpDescription = ""
	})

getOptionsOrDie :: (MonadIO m, Options a) => m a
getOptionsOrDie = do
	args <- liftIO System.Environment.getArgs
	let defs = addHelpFlags optionsDefs
	case tokenize defs args of
		-- TODO: subcommands
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
