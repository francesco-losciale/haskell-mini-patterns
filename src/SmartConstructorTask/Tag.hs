module SmartConstructorTask.Tag where

-- | Tag is a non-empty string.
newtype Tag = Tag String deriving Show

mkTag :: String -> Maybe Tag
mkTag tag
    | null tag = Nothing
    | otherwise = Just $ Tag tag