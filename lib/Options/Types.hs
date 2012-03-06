-- |
-- Module: Options.Types
-- License: MIT
--
-- TODO: documentation here
module Options.Types
	( OptionDefinitions(..)
	, GroupInfo(..)
	, OptionInfo(..)
	, TokensFor(..)
	) where

data OptionDefinitions a = OptionDefinitions [OptionInfo] [(String, [OptionInfo])]

data GroupInfo = GroupInfo
	{ groupInfoName :: String
	, groupInfoDescription :: String
	, groupInfoHelpDescription :: String
	}

data OptionInfo = OptionInfo
	{ optionInfoKey :: String
	, optionInfoShortFlags :: [Char]
	, optionInfoLongFlags :: [String]
	, optionInfoDefault :: String
	, optionInfoUnary :: Bool
	, optionInfoDescription :: String
	, optionInfoGroup :: Maybe GroupInfo
	}

data TokensFor a = TokensFor [(String, String)] [String]
