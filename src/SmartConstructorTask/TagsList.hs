module SmartConstructorTask.TagsList where

import Control.Applicative (liftA2)
import Data.List.NonEmpty (NonEmpty (..))
import SmartConstructorTask.Tag (Tag, mkTag)

-- Pattern
-- Smart constructor

-- Description
-- Providing idiomatic ways for constructing values.

-- When to use
-- When a data type restricts some values (e.g. not every ByteString is a valid Password).
-- When you want to make construction of big data types easier.
-- To avoid runtime errors.
-- To make illegal states unrepresentable.
-- Benefits
-- More structured and maintainable code.
-- Separation of concepts.
-- Control of erroneous data inputs.
-- Costs
-- Some extra code.
-- Decide on the approach details.
-- Inability to define instances outside of the module.


-- | Non-empty list of non-empty tags.
newtype TagsList = TagsList (NonEmpty Tag) deriving Show

mkTagsList :: [String] -> Maybe TagsList
mkTagsList [] = Nothing
mkTagsList (tag:tags) = TagsList <$> liftA2 (:|) (mkTag tag) (traverse mkTag tags)