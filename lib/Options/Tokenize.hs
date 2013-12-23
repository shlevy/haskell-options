{-# LANGUAGE TypeFamilies #-}

-- |
-- Module: Options.Tokenize
-- License: MIT
module Options.Tokenize
	( Token(..)
	, tokenFlagName
	, Tokens(..)
	, tokenize
	) where

import           Control.Monad.Error hiding (throwError)
import qualified Control.Monad.Error
import           Control.Monad.State
import           Data.Functor.Identity
import qualified Data.Map

import           Options.Types
import           Options.Util

data Token
	= TokenUnary String -- flag name
	| Token String String -- flag name, flag value
	deriving (Eq, Show)

tokenFlagName :: Token -> String
tokenFlagName (TokenUnary s) = s
tokenFlagName (Token s _) = s

data Tokens = Tokens
	{ tokensMap :: Data.Map.Map OptionKey Token
	, tokensArgv :: [String]
	}
	deriving (Show)

data TokState = TokState
	{ stArgv :: [String]
	, stArgs :: [String]
	, stOpts :: Data.Map.Map OptionKey Token
	, stShortKeys :: Data.Map.Map Char (OptionKey, Bool)
	, stLongKeys :: Data.Map.Map String (OptionKey, Bool)
	, stSubcommands :: [(String, [OptionInfo])]
	, stSubCmd :: Maybe String
	}

newtype Tok a = Tok { unTok :: ErrorT String (StateT TokState Identity) a }

instance Monad Tok where
	return = Tok . return
	m >>= f = Tok (unTok m >>= unTok . f)

instance MonadState Tok where
	type StateType Tok = TokState
	get = Tok get
	put = Tok . put

tokenize :: OptionDefinitions -> [String] -> (Maybe String, Either String Tokens)
tokenize (OptionDefinitions options subcommands) argv = runIdentity $ do
	let st = TokState
		{ stArgv = argv
		, stArgs = []
		, stOpts = Data.Map.empty
		, stShortKeys = toShortKeys options
		, stLongKeys = toLongKeys options
		, stSubcommands = subcommands
		, stSubCmd = Nothing
		}
	(err, st') <- runStateT (runErrorT (unTok loop)) st
	return (stSubCmd st', case err of
		Left err' -> Left err'
		Right _ -> Right (Tokens (stOpts st') (stArgs st')))

loop :: Tok ()
loop = do
	ms <- nextItem
	st <- get
	case ms of
		Nothing -> return ()
		Just s -> (>> loop) $ case stringToGhc704 s of
			'-':'-':[] -> put (st { stArgv = [], stArgs = stArgs st ++ stArgv st })
			'-':'-':opt -> parseLong opt
			'-':optChar:optValue -> parseShort optChar optValue
			'-':[] -> addArg s
			decoded -> case (stSubcommands st, stSubCmd st) of
				([], _) -> addArg s
				(_, Just _) -> addArg s
				(_, Nothing) -> case lookup decoded (stSubcommands st) of
					Nothing -> throwError ("Unknown subcommand " ++ show decoded ++ ".")
					Just subOptions -> mergeSubcommand decoded subOptions

nextItem :: Tok (Maybe String)
nextItem = do
	st <- get
	case stArgv st of
		[] -> return Nothing
		(x:xs) -> do
			put (st { stArgv = xs })
			return (Just x)

addArg :: String -> Tok ()
addArg s = modify (\st -> st { stArgs = stArgs st ++ [s] })

addOpt :: OptionKey -> Token  -> Tok ()
addOpt key val = do
	oldOpts <- gets stOpts
	case Data.Map.lookup key oldOpts of
		Nothing -> modify (\st -> st { stOpts = Data.Map.insert key val (stOpts st) })
		-- TODO: include old and new values?
		Just _ -> throwError ("Multiple values for flag " ++ tokenFlagName val ++ " were provided.")

mergeSubcommand :: String -> [OptionInfo] -> Tok ()
mergeSubcommand name opts = modify $ \st -> st
	{ stSubCmd = Just name
	, stShortKeys = Data.Map.union (stShortKeys st) (toShortKeys opts)
	, stLongKeys = Data.Map.union (stLongKeys st) (toLongKeys opts)
	}

parseLong :: String -> Tok ()
parseLong optName = do
	longKeys <- gets stLongKeys
	case break (== '=') optName of
		(before, after) -> case after of
			'=' : value -> case Data.Map.lookup before longKeys of
				Nothing -> throwError ("Unknown flag --" ++ before)
				Just (key, _) -> addOpt key (Token ("--" ++ before) value)
			_ -> case Data.Map.lookup optName longKeys of
				Nothing -> throwError ("Unknown flag --" ++ optName)
				Just (key, unary) -> if unary
					then addOpt key (TokenUnary ("--" ++ optName))
					else do
						next <- nextItem
						case next of
							Nothing -> throwError ("The flag --" ++ optName ++ " requires an argument.")
							Just value -> addOpt key (Token ("--" ++ optName) value)

parseShort :: Char -> String -> Tok ()
parseShort optChar optValue = do
	let optName = '-' : [optChar]
	shortKeys <- gets stShortKeys
	case Data.Map.lookup optChar shortKeys of
		Nothing -> throwError ("Unknown flag " ++ optName)
		Just (key, unary) -> if unary
			then do
				addOpt key (TokenUnary optName)
				case optValue of
					[] -> return ()
					nextChar:nextValue -> parseShort nextChar nextValue
			else case optValue of
				"" -> do
					next <- nextItem
					case next of
						Nothing -> throwError ("The flag " ++ optName ++ " requires an argument.")
						Just value -> addOpt key (Token optName value)
				_ -> addOpt key (Token optName optValue)

toShortKeys :: [OptionInfo] -> Data.Map.Map Char (OptionKey, Bool)
toShortKeys opts = Data.Map.fromList $ do
	opt <- opts
	flag <- optionInfoShortFlags opt
	return (flag, (optionInfoKey opt, optionInfoUnary opt))

toLongKeys :: [OptionInfo] -> Data.Map.Map String (OptionKey, Bool)
toLongKeys opts = Data.Map.fromList $ do
	opt <- opts
	flag <- optionInfoLongFlags opt
	return (flag, (optionInfoKey opt, optionInfoUnary opt))

throwError :: String -> Tok a
throwError = Tok . Control.Monad.Error.throwError
