-- |
-- Module: Options.Types
-- License: MIT
module Options.Types
	( OptionDefinitions(..)
	, GroupInfo(..)
	, OptionKey(..)
	, OptionInfo(..)
	, Token(..)
	, tokenFlagName
	, Tokens(..)
	) where

import qualified Data.Map as Map

data OptionDefinitions a = OptionDefinitions [OptionInfo] [(String, [OptionInfo])]

data GroupInfo = GroupInfo
	{ groupInfoName :: String
	, groupInfoTitle :: String
	, groupInfoDescription :: String
	}
	deriving (Eq, Show)

data OptionKey
	= OptionKey String
	| OptionKeyHelpSummary
	| OptionKeyHelpGroup String
	deriving (Eq, Ord, Show)

data OptionInfo = OptionInfo
	{ optionInfoKey :: OptionKey
	, optionInfoShortFlags :: [Char]
	, optionInfoLongFlags :: [String]
	, optionInfoDefault :: String
	, optionInfoUnary :: Bool
	, optionInfoDescription :: String
	, optionInfoGroup :: Maybe GroupInfo
	}
	deriving (Eq, Show)

data Token
	= TokenUnary String -- flag name
	| Token String String -- flag name, flag value
	deriving (Eq, Show)

tokenFlagName :: Token -> String
tokenFlagName (TokenUnary s) = s
tokenFlagName (Token s _) = s

data Tokens = Tokens
	{ tokensMap :: Map.Map OptionKey Token
	, tokensArgv :: [String]
	}
	deriving (Show)
