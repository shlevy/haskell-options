-- |
-- Module: Options.Types
-- License: MIT
module Options.Types
	( OptionDefinitions(..)
	, GroupInfo(..)
	, OptionInfo(..)
	, TokensFor(..)
	) where

data OptionDefinitions a = OptionDefinitions [OptionInfo] [(String, [OptionInfo])]

data GroupInfo = GroupInfo
	{ groupInfoName :: String
	, groupInfoTitle :: String
	, groupInfoDescription :: String
	}
	deriving (Eq, Show)

data OptionInfo = OptionInfo
	{ optionInfoKey :: String
	, optionInfoShortFlags :: [Char]
	, optionInfoLongFlags :: [String]
	, optionInfoDefault :: String
	, optionInfoUnary :: Bool
	, optionInfoDescription :: String
	, optionInfoGroup :: Maybe GroupInfo
	}
	deriving (Eq, Show)

data TokensFor a = TokensFor [(String, (String, String))] [String]
