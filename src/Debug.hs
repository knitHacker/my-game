module Debug
( DebugLevel(..)
, PrintLevel(..)
, shouldShow
) where


data DebugLevel = All | Normal | OnlyCritical deriving (Show, Eq)

data PrintLevel = Debug | Log | Critical deriving (Show, Eq, Ord)


shouldShow :: DebugLevel -> PrintLevel -> Bool
shouldShow All _ = True
shouldShow Normal l = l > Debug
shouldShow OnlyCritical l = l >= Critical
