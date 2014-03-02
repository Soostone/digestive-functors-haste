--------------------------------------------------------------------------------
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE Rank2Types                #-}
-- | End-user interface - provides the main functionality for
-- form creation and validation. For an interface for front-end
-- implementation, see "View".
module Text.Digestive.Form
    ( Formlet
    , Form
    , SomeForm (..)
    , (.:)

      -- * Basic forms
    , text
    , string
    , stringRead
    , choice
    , choice'
    , choiceWith
    , choiceWith'
    , groupedChoice
    , groupedChoice'
    , groupedChoiceWith
    , groupedChoiceWith'
    , bool
    , file

      -- * Optional forms
    , optionalText
    , optionalString
    , optionalStringRead

      -- * Validation and transformation
    , check
    , checkM
    , validate
    , validateM
    , disable

      -- * Lifting forms
    , monadic

      -- * Dynamic list forms
    , listOf
    ) where


--------------------------------------------------------------------------------
import           Control.Monad                      (liftM, liftM2)
import           Data.List                          (findIndex)
import           Data.Maybe                         (fromMaybe)
import           Data.Text                          (Text)
import qualified Data.Text                          as T
--------------------------------------------------------------------------------
import           Text.Digestive.Form.Internal
import           Text.Digestive.Form.Internal.Field
import           Text.Digestive.Form.List
import           Text.Digestive.Ref
import           Text.Digestive.Types
import           Text.Digestive.Util
-------------------------------------------------------------------------------


--------------------------------------------------------------------------------
-- | A 'Form' with a set, optional default value
type Formlet v m a = Maybe a -> Form v m a


--------------------------------------------------------------------------------
-- | Returns a 'Formlet' which may optionally take a default text
text :: Formlet v m Text
text def = Pure $ Text $ fromMaybe "" def


--------------------------------------------------------------------------------
-- | Identical to "text" but takes a String
string :: Monad m => Formlet v m String
string = fmap T.unpack . text . fmap T.pack


--------------------------------------------------------------------------------
-- | Returns a 'Formlet' for a parseable and serializable value type
stringRead :: (Monad m, Read a, Show a) => v -> Formlet v m a
stringRead err = transform (readTransform err) . string . fmap show


--------------------------------------------------------------------------------
-- | Returns a 'Formlet' for a value restricted to
-- the provided list of value-message tuples
choice :: (Eq a, Monad m) => [(a, v)] -> Formlet v m a
choice items def = choiceWith (zip makeRefs items) def


--------------------------------------------------------------------------------
-- | Sometimes there is no good 'Eq' instance for 'choice'. In this case, you
-- can use this function, which takes an index in the list as default.
choice' :: Monad m => [(a, v)] -> Maybe Int -> Form v m a
choice' items def = choiceWith' (zip makeRefs items) def


--------------------------------------------------------------------------------
-- | Allows you to assign your own values: these values will be used in the
-- resulting HTML instead of the default @[0 ..]@. This fixes some race
-- conditions that might otherwise appear, e.g. if new choice items are added to
-- some database while a user views and submits the form...
choiceWith :: (Eq a, Monad m) => [(Text, (a, v))] -> Formlet v m a
choiceWith items def = choiceWith' items def'
  where
    def' = def >>= (\d -> findIndex ((== d) . fst . snd) items)


--------------------------------------------------------------------------------
-- | A version of 'choiceWith' for when there is no good 'Eq' instance.
choiceWith' :: Monad m => [(Text, (a, v))] -> Maybe Int -> Form v m a
choiceWith' items def = fmap fst $ Pure $ Choice [("", items)] def'
  where
    def' = fromMaybe 0 def


--------------------------------------------------------------------------------
-- | Returns a 'Formlet' for named groups of choices.
groupedChoice :: (Eq a, Monad m) => [(Text, [(a, v)])] -> Formlet v m a
groupedChoice items def =
    groupedChoiceWith (mkGroupedRefs items makeRefs) def


--------------------------------------------------------------------------------
-- | Sometimes there is no good 'Eq' instance for 'choice'. In this case, you
-- can use this function, which takes an index in the list as default.
groupedChoice' :: Monad m => [(Text, [(a, v)])] -> Maybe Int -> Form v m a
groupedChoice' items def =
    groupedChoiceWith' (mkGroupedRefs items makeRefs) def


mkGroupedRefs :: [(Text, [a])]
              -> [Text]
              -> [(Text, [(Text, a)])]
mkGroupedRefs [] _ = []
mkGroupedRefs (g:gs) is = cur : mkGroupedRefs gs b
  where
    (a,b) = splitAt (length $ snd g) is
    cur = (fst g, zip a (snd g))


--------------------------------------------------------------------------------
-- | Allows you to assign your own values: these values will be used in the
-- resulting HTML instead of the default @[0 ..]@. This fixes some race
-- conditions that might otherwise appear, e.g. if new choice items are added to
-- some database while a user views and submits the form...
groupedChoiceWith :: (Eq a, Monad m)
                  => [(Text, [(Text, (a, v))])]
                  -> Formlet v m a
groupedChoiceWith items def = groupedChoiceWith' items def'
  where
    def' = def >>= (\d -> findIndex ((== d) . fst . snd) $
                            concat $ map snd items)


--------------------------------------------------------------------------------
-- | Low-level support for grouped choice.
groupedChoiceWith' :: Monad m
                   => [(Text, [(Text, (a, v))])]
                   -> Maybe Int
                   -> Form v m a
groupedChoiceWith' items def = fmap fst $ Pure $ Choice items def'
  where
    def' = fromMaybe 0 def


--------------------------------------------------------------------------------
-- | Returns a 'Formlet' for binary choices
bool :: Formlet v m Bool
bool = Pure . Bool . fromMaybe False


--------------------------------------------------------------------------------
-- | Returns a 'Formlet' for file selection
file :: Form v m (Maybe FilePath)
file = Pure File


--------------------------------------------------------------------------------
-- | Validate the results of a form with a simple predicate
--
-- Example:
--
-- > check "Can't be empty" (not . null) (string Nothing)
check :: Monad m
      => v            -- ^ Error message (if fail)
      -> (a -> Bool)  -- ^ Validating predicate
      -> Form v m a   -- ^ Form to validate
      -> Form v m a   -- ^ Resulting form
check err = checkM err . (return .)


--------------------------------------------------------------------------------
-- | Version of 'check' which allows monadic validations
checkM :: Monad m => v -> (a -> m Bool) -> Form v m a -> Form v m a
checkM err predicate form = validateM f form
  where
    f x = do
        r <- predicate x
        return $ if r then return x else Error err


--------------------------------------------------------------------------------
-- | This is an extension of 'check' that can be used to apply transformations
-- that optionally fail
--
-- Example: taking the first character of an input string
--
-- > head' :: String -> Result String Char
-- > head' []      = Error "Is empty"
-- > head' (x : _) = Success x
-- >
-- > char :: Monad m => Form m String Char
-- > char = validate head' (string Nothing)
validate :: Monad m => (a -> Result v b) -> Form v m a -> Form v m b
validate = validateM . (return .)


--------------------------------------------------------------------------------
-- | Version of 'validate' which allows monadic validations
validateM :: Monad m => (a -> m (Result v b)) -> Form v m a -> Form v m b
validateM = transform


--------------------------------------------------------------------------------
-- | Disables a form
disable :: Form v m a -> Form v m a
disable f = Metadata [Disabled] f


--------------------------------------------------------------------------------
-- | Create a text form with an optional default text which
-- returns nothing if no optional text was set, and no input
-- was retrieved.
optionalText :: Monad m => Maybe Text -> Form v m (Maybe Text)
optionalText def = validate opt (text def)
  where
    opt t
        | T.null t  = return Nothing
        | otherwise = return $ Just t


--------------------------------------------------------------------------------
-- | Identical to 'optionalText', but uses Strings
optionalString :: Monad m => Maybe String -> Form v m (Maybe String)
optionalString = fmap (fmap T.unpack) . optionalText . fmap T.pack


--------------------------------------------------------------------------------
-- | Identical to 'optionalText' for parseable and serializable values.
optionalStringRead :: (Monad m, Read a, Show a)
                   => v -> Maybe a -> Form v m (Maybe a)
optionalStringRead err = transform readTransform' . optionalString . fmap show
  where
    readTransform' (Just s) = liftM (fmap Just) $ readTransform err s
    readTransform' Nothing  = return (return Nothing)


--------------------------------------------------------------------------------
-- Helper function for attempted parsing, with custom error messages
readTransform :: (Monad m, Read a) => v -> String -> m (Result v a)
readTransform err = return . maybe (Error err) return . readMaybe


--------------------------------------------------------------------------------
-- | Dynamic lists
listOf :: Monad m
       => Formlet v m a
       -> Formlet v m [a]
listOf single def =
    List (fmap single defList) (indicesRef .: listIndices ixs)
  where
    ixs = case def of
        Nothing -> [0]
        Just xs -> [0 .. length xs - 1]

    defList = DefaultList Nothing $ maybe [] (map Just) def


--------------------------------------------------------------------------------
-- Manipulatable indices
listIndices :: Monad m => [Int] -> Form v m [Int]
listIndices = fmap parseIndices . text . Just . unparseIndices




