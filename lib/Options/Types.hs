-- |
-- Module: Options.Types
-- License: MIT
--
-- TODO: documentation here
module Options.Types
	( OptionDefinitions(..)
	, OptionGroupInfo(..)
	, OptionInfo(..)
	, TokensFor(..)
	) where

data OptionDefinitions a = OptionDefinitions [OptionInfo] [(String, [OptionInfo])]

data OptionGroupInfo = OptionGroupInfo
	{ optionGroupInfoName :: String
	, optionGroupInfoDescription :: String
	, optionGroupInfoHelpDescription :: String
	}

data OptionInfo = OptionInfo
	{ optionInfoKey :: String
	, optionInfoShortFlags :: [Char]
	, optionInfoLongFlags :: [String]
	, optionInfoDefault :: String
	, optionInfoUnary :: Bool
	, optionInfoDescription :: String
	, optionInfoGroup :: Maybe OptionGroupInfo
	}

data TokensFor a = TokensFor [(String, String)] [String]
