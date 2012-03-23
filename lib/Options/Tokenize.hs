{-# LANGUAGE TypeFamilies #-}

-- |
-- Module: Options.Tokenize
-- License: MIT
module Options.Tokenize
	( tokenize
	) where

import           Control.Monad.Error hiding (throwError)
import qualified Control.Monad.Error
import           Control.Monad.State
import           Data.Functor.Identity
import qualified Data.Map

import           Options.Types

data TokState = TokState
	{ stArgv :: [String]
	, stArgs :: [String]
	, stOpts :: [(String, (String, String))]
	, stShortKeys :: Data.Map.Map Char (String, Bool)
	, stLongKeys :: Data.Map.Map String (String, Bool)
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

tokenize :: OptionDefinitions a -> [String] -> (Maybe String, Either String (TokensFor a))
tokenize (OptionDefinitions options subcommands) argv = runIdentity $ do
	let st = TokState
		{ stArgv = argv
		, stArgs = []
		, stOpts = []
		, stShortKeys = toShortKeys options
		, stLongKeys = toLongKeys options
		, stSubcommands = subcommands
		, stSubCmd = Nothing
		}
	(err, st') <- runStateT (runErrorT (unTok loop)) st
	return (stSubCmd st', case err of
		Left err' -> Left err'
		Right _ -> Right (TokensFor (stOpts st') (stArgs st')))

loop :: Tok ()
loop = do
	ms <- nextItem
	st <- get
	case ms of
		Nothing -> return ()
		Just s -> (>> loop) $ case s of
			'-':'-':[] -> put (st { stArgv = [], stArgs = stArgs st ++ stArgv st })
			'-':'-':opt -> parseLong opt
			'-':optChar:optValue -> parseShort optChar optValue
			'-':[] -> addArg s
			_ -> case (stSubcommands st, stSubCmd st) of
				([], _) -> addArg s
				(_, Just _) -> addArg s
				(_, Nothing) -> case lookup s (stSubcommands st) of
					Nothing -> throwError ("Unknown subcommand " ++ show s ++ ".")
					Just subOptions -> mergeSubcommand s subOptions

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

addOpt :: String -> String -> String -> Tok ()
addOpt flag key val = do
	oldOpts <- gets stOpts
	case lookup key oldOpts of
		Nothing -> modify (\st -> st { stOpts = stOpts st ++ [(key, (flag, val))] })
		-- TODO: include old and new values?
		Just _ -> throwError ("Multiple values for flag " ++ flag ++ " were provided.")

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
				Just (key, _) -> addOpt ("--" ++ before) key value
			_ -> case Data.Map.lookup optName longKeys of
				Nothing -> throwError ("Unknown flag --" ++ optName)
				Just (key, unary) -> if unary
					then addOpt ("--" ++ optName) key "true"
					else do
						next <- nextItem
						case next of
							Nothing -> throwError ("The flag --" ++ optName ++ " requires an argument.")
							Just value -> addOpt ("--" ++ optName) key value

parseShort :: Char -> String -> Tok ()
parseShort optChar optValue = do
	let optName = '-' : [optChar]
	shortKeys <- gets stShortKeys
	case Data.Map.lookup optChar shortKeys of
		Nothing -> throwError ("Unknown flag " ++ optName)
		Just (key, unary) -> if unary
			then do
				addOpt optName key "true"
				case optValue of
					[] -> return ()
					nextChar:nextValue -> parseShort nextChar nextValue
			else case optValue of
				"" -> do
					next <- nextItem
					case next of
						Nothing -> throwError ("The flag " ++ optName ++ " requires an argument.")
						Just value -> addOpt optName key value
				_ -> addOpt optName key optValue

toShortKeys :: [OptionInfo] -> Data.Map.Map Char (String, Bool)
toShortKeys opts = Data.Map.fromList $ do
	opt <- opts
	flag <- optionInfoShortFlags opt
	return (flag, (optionInfoKey opt, optionInfoUnary opt))

toLongKeys :: [OptionInfo] -> Data.Map.Map String (String, Bool)
toLongKeys opts = Data.Map.fromList $ do
	opt <- opts
	flag <- optionInfoLongFlags opt
	return (flag, (optionInfoKey opt, optionInfoUnary opt))

throwError :: String -> Tok a
throwError = Tok . Control.Monad.Error.throwError
