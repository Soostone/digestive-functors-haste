--------------------------------------------------------------------------------
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE OverloadedStrings #-}
module Text.Digestive.SimpleBlaze
    ( inputText
    , inputTextArea
    , inputPassword
    , inputHidden
    , inputSelect
    , inputRadio
    , inputCheckbox
    , inputFile
    , inputSubmit
    , mkLabel
    , mkForm
    , errorList
    , childErrorList
    ) where


--------------------------------------------------------------------------------
import           Control.Monad       (forM_, when)
import           Data.Maybe          (fromMaybe)
import           Data.Monoid         (mappend, mempty)
import           Data.Text           (Text)
import           Text.Blaze          hiding ((!?))
import           Text.Blaze.Internal hiding ((!?))
--------------------------------------------------------------------------------
import           Text.Digestive.View
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
-- | Html Combinators
-------------------------------------------------------------------------------


type Html = Markup

toHtml :: ToMarkup a => a -> Html
toHtml = toMarkup

input :: Html  -- ^ Resulting HTML.
input = Leaf "input" "<input" ">"

form :: Html  -- ^ Inner HTML.
     -> Html  -- ^ Resulting HTML.
form = Parent "form" "<form" "</form>"

br :: Html  -- ^ Resulting HTML.
br = Leaf "br" "<br" ">"

li :: Html  -- ^ Inner HTML.
   -> Html  -- ^ Resulting HTML.
li = Parent "li" "<li" "</li>"

ul :: Html  -- ^ Inner HTML.
   -> Html  -- ^ Resulting HTML.
ul = Parent "ul" "<ul" "</ul>"

label :: Html  -- ^ Inner HTML.
      -> Html  -- ^ Resulting HTML.
label = Parent "label" "<label" "</label>"

select :: Html  -- ^ Inner HTML.
       -> Html  -- ^ Resulting HTML.
select = Parent "select" "<select" "</select>"

option :: Html  -- ^ Inner HTML.
       -> Html  -- ^ Resulting HTML.
option = Parent "option" "<option" "</option>"

textarea :: Html  -- ^ Inner HTML.
         -> Html  -- ^ Resulting HTML.
textarea = Parent "textarea" "<textarea" "</textarea>"

rows :: AttributeValue  -- ^ Attribute value.
     -> Attribute       -- ^ Resulting attribute.
rows = attribute "rows" " rows=\""

cols :: AttributeValue  -- ^ Attribute value.
     -> Attribute       -- ^ Resulting attribute.
cols = attribute "cols" " cols=\""

selected :: AttributeValue  -- ^ Attribute value.
         -> Attribute       -- ^ Resulting attribute.
selected = attribute "selected" " selected=\""

enctype :: AttributeValue  -- ^ Attribute value.
        -> Attribute       -- ^ Resulting attribute.
enctype = attribute "enctype" " enctype=\""


-------------------------------------------------------------------------------
attr :: String -> AttributeValue -> Attribute
attr nm = customAttribute (stringTag nm)
idAttr = attr "id"
nameAttr = attr "name"
valueAttr = attr "value"
typeAttr = attr "type"
classAttr = attr "class"
checkedAttr = attr "checked"
forAttr = attr "for"
methodAttr = attr "method"
actionAttr = attr "action"

--------------------------------------------------------------------------------
(!?) :: Attributable h => h -> (Bool, Attribute) -> h
(!?) h (False, _) = h
(!?) h (True,  a) = h ! a



-------------------------------------------------------------------------------
-- | Input Elements
-------------------------------------------------------------------------------


--------------------------------------------------------------------------------
inputText :: Text -> View v -> Html
inputText ref view = input
    ! typeAttr "text"
    ! idAttr    (toValue ref')
    ! nameAttr  (toValue ref')
    ! valueAttr (toValue $ fieldInputText ref view)
  where
    ref' = absoluteRef ref view


--------------------------------------------------------------------------------
inputTextArea :: Maybe Int  -- ^ Rows
              -> Maybe Int  -- ^ Columns
              -> Text       -- ^ Form path
              -> View Html  -- ^ View
              -> Html       -- ^ Resulting HTML
inputTextArea r c ref view = mkRows r $ mkCols c $ textarea
    ! idAttr     (toValue ref')
    ! nameAttr   (toValue ref')
    $ toHtml (fieldInputText ref view)
  where
    ref'            = absoluteRef ref view
    mkRows (Just x) = (! rows (toValue x))
    mkRows _        = id
    mkCols (Just x) = (! cols (toValue x))
    mkCols _        = id


--------------------------------------------------------------------------------
inputPassword :: Text -> View v -> Html
inputPassword ref view = input
    ! typeAttr "password"
    ! idAttr    (toValue ref')
    ! nameAttr  (toValue ref')
    ! valueAttr (toValue $ fieldInputText ref view)
  where
    ref' = absoluteRef ref view


--------------------------------------------------------------------------------
inputHidden :: Text -> View v -> Html
inputHidden ref view = input
    ! typeAttr "hidden"
    ! idAttr    (toValue ref')
    ! nameAttr  (toValue ref')
    ! valueAttr (toValue $ fieldInputText ref view)
  where
    ref' = absoluteRef ref view


--------------------------------------------------------------------------------
inputSelect :: Text -> View Html -> Html
inputSelect ref view = select
    ! idAttr    (toValue ref')
    ! nameAttr  (toValue ref')
    $ forM_ choices $ \(i, c, sel) -> option
        !  valueAttr (value i)
        !? (sel, selected "selected")
        $ c
  where
    ref'    = absoluteRef ref view
    value i = toValue ref' `mappend` "." `mappend` toValue i
    choices = fieldInputChoice ref view


--------------------------------------------------------------------------------
inputRadio :: Bool       -- ^ Add @br@ tags?
           -> Text       -- ^ Form path
           -> View Html  -- ^ View
           -> Html       -- ^ Resulting HTML
inputRadio brs ref view = forM_ choices $ \(i, c, sel) -> do
    let val = value i
    input ! typeAttr "radio" ! valueAttr val ! idAttr val ! nameAttr (toValue ref')
        !? (sel, checkedAttr "checked")
    label ! forAttr val $ c
    when brs br
  where
    ref'    = absoluteRef ref view
    value i = toValue ref' `mappend` "." `mappend` toValue i
    choices = fieldInputChoice ref view


--------------------------------------------------------------------------------
inputCheckbox :: Text -> View Html -> Html
inputCheckbox ref view = input
    !  typeAttr "checkbox"
    !  idAttr    (toValue ref')
    !  nameAttr  (toValue ref')
    !? (selected, checkedAttr "checked")
  where
    ref'     = absoluteRef ref view
    selected = fieldInputBool ref view


--------------------------------------------------------------------------------
inputFile :: Text -> View Html -> Html
inputFile ref view = input
    ! typeAttr "file"
    ! idAttr    (toValue ref')
    ! nameAttr  (toValue ref')
    ! valueAttr (toValue value)
  where
    ref'  = absoluteRef ref view
    value = fromMaybe "" $ fieldInputFile ref view


--------------------------------------------------------------------------------
inputSubmit :: Text -> Html
inputSubmit value = input
    ! typeAttr "submit"
    ! valueAttr (toValue value)


--------------------------------------------------------------------------------
mkLabel :: Text -> View v -> Html -> Html
mkLabel ref view value = label
    ! forAttr (toValue ref')
    $ value
  where
    ref' = absoluteRef ref view


--------------------------------------------------------------------------------
mkForm :: View Html -> Text -> Html -> Html
mkForm view action = form
    ! methodAttr  "POST"
    ! enctype (toValue $ show $ viewEncType view)
    ! actionAttr  (toValue action)


--------------------------------------------------------------------------------
errorList :: Text -> View Html -> Html
errorList ref view = case errors ref view of
    []   -> mempty
    errs -> ul ! classAttr "digestive-functors-error-list" $ forM_ errs $ \e ->
        li ! classAttr "digestive-functors-error" $ e


--------------------------------------------------------------------------------
childErrorList :: Text -> View Html -> Html
childErrorList ref view = case childErrors ref view of
    []   -> mempty
    errs -> ul ! classAttr "digestive-functors-error-list" $ forM_ errs $ \e ->
        li ! classAttr "digestive-functors-error" $ e
