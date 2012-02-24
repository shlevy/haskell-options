-- |
-- Module: Options.Types
-- License: MIT
--
-- TODO: documentation here
module Options.Types
	( OptionDefinitions(..)
	, OptionInfo(..)
	, TokensFor(..)
	) where

data OptionDefinitions a = OptionDefinitions [OptionInfo] [(String, [OptionInfo])]

data OptionInfo = OptionInfo
	{ optionInfoKey :: String
	, optionInfoShortFlags :: [Char]
	, optionInfoLongFlags :: [String]
	, optionInfoDefault :: String
	, optionInfoUnary :: Bool
	}

data TokensFor a = TokensFor [(String, String)] [String]
