module SmartConstructorTask.TagsList where

import Data.List.NonEmpty (NonEmpty (..))
import SmartConstructorTask.Tag (Tag, mkTag)


-- | Non-empty list of non-empty tags.
newtype TagsList = TagsList (NonEmpty Tag)

mkTagsList :: [String] -> TagsList
mkTagsList [] = error "Empty list of tags"
mkTagsList (tag:tags) = TagsList $ mkTag tag :| map mkTag tags