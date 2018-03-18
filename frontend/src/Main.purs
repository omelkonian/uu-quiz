module Main where

import Control.Monad.Aff
import Control.Monad.Except.Trans
import Control.Monad.Reader.Trans
import Data.Argonaut.Generic.Aeson
import Data.Either
import Data.Generic
import Data.Maybe
import Prelude
import Servant.PureScript.Affjax
import Servant.PureScript.Settings
import Data.Array as Array
import Control.Bind ((<=<))
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Exception (EXCEPTION)
import DOM.Node.Node (baseURI)
import Data.Argonaut.Core (Json)
import Data.Argonaut.Parser (jsonParser)
import Data.Bifunctor (lmap)
import Data.Foldable (foldr, fold)
import Data.List (List(Nil, Cons))
import Data.Tuple (Tuple(Tuple))
import Network.HTTP.Affjax (AJAX, get)
import Pux (renderToDOM, fromSimple, start, EffModel, noEffects)
import Pux.Html (Html, text, div)
import Servant.Subscriber (Subscriber, makeSubscriber, SubscriberEff, Config, makeSubscriptions)
import Servant.Subscriber.Request (HttpRequest(..))
import Servant.Subscriber.Types (Path(Path))
import Unsafe.Coerce (unsafeCoerce)


data Action = Nop

type State = { num :: Int }

type APIEffect eff = ReaderT MySettings (ExceptT AjaxError (Aff ( ajax :: AJAX, channel :: CHANNEL, err :: EXCEPTION  | eff)))

type ServantModel =
    { state :: State
    , effects :: Array (APIEffect () Action)
    }

update :: Action -> State -> EffModel State Action (ajax :: AJAX)
update Nop state = noEffects state

view :: Html Action
view =
  div [text "Hello"]

main :: forall eff. Eff (ajax :: AJAX, err :: EXCEPTION | eff) Unit
main = do
  app <- coerceEffects <<< start $
    { initialState: { num : 0 }
    , update: update
    , view: view
    , inputs : []
    }
  renderToDOM "#app" app.html

coerceEffects :: forall eff0 eff1 a. Eff eff0 a -> Eff eff1 a
coerceEffects = unsafeCoerce
