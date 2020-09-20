module SmartConstructorTask.Tag where

-- | Tag is a non-empty string.
newtype Tag = Tag String

mkTag :: String -> Tag
mkTag tag
    | null tag = error "Empty tag!"
    | otherwise = Tag tag