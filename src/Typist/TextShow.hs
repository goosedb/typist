module Typist.TextShow where
import TextShow
import GHC.OverloadedLabels
import GHC.TypeLits
import Typist
import qualified Data.Text as Text
import qualified Data.Text.Lazy as Text.Lazy
import qualified Data.Text.Lazy.Builder as Builder

data Name (s :: Symbol) = Name

instance IsLabel s (Name s) where
  fromLabel = Name

-- | Set parameter for template
--
-- @
-- ghci> :{
-- fmt \@"Hello, #{name}, do you like #{thing}?" $ 
--  (#name \@= Unquoted \"Kitty\") . (#thing \@= Unquoted \"cheese\")
-- :}
-- "Hello, Kitty, do you like cheese?"
-- @
{-# INLINE (#=) #-}
(#=) :: (TextShow a) => Name s -> a -> (Rec as -> Rec (Arg n s ': as))
(#=) Name a = (Arg (showb a) :&)

-- | Use to render string as is without quotation
--
-- @
-- ghci> "> " <> showt "hello" <> " <"
-- "> \\"hello\\" <"
-- ghci> "> " <> showt (Unquoted "hello") <> " <"
-- "> hello <"
-- @
newtype Unquoted a = Unquoted a

instance TextShow (Unquoted String) where
  showb (Unquoted s) = fromString s

instance TextShow (Unquoted Text.Text) where
  showb (Unquoted s) = Builder.fromText s

instance TextShow (Unquoted Text.Lazy.Text) where
  showb (Unquoted s) = Builder.fromLazyText s

instance TextShow (Unquoted Builder.Builder) where
  showb (Unquoted s) = s
