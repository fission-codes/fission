module Fission.CLI.Handler.User.Setup.UI where

import           Brick
import           Brick.Focus
import           Brick.Forms

import qualified Brick.Widgets.Border  as Border
import qualified Brick.Widgets.Center  as Center
import qualified Brick.Widgets.Edit    as Edit


import qualified Graphics.Vty          as V

import           Fission.Prelude       hiding (on)

import           Fission.User.Email    as Email
import           Fission.User.Username as Username

data Field
  = UsernameField
  | EmailField
  deriving (Show, Eq, Ord)

data Registration = Registration
  { regUsername :: !Username
  , regEmail    :: !Email
  } deriving (Show, Eq)

usernameL :: Functor f => (Username -> f Username) -> Registration -> f Registration
usernameL = lens regUsername \reg newUsername -> reg { regUsername = newUsername }

emailL :: Functor f => (Email -> f Email) -> Registration -> f Registration
emailL = lens regEmail \reg newEmail -> reg { regEmail = newEmail }

toForm :: Registration -> Form Registration event Field
toForm = newForm
  [ withLabel "Username" @@=
      editTextField (usernameL . Username.rawL) UsernameField (Just 1)

  , withLabel "Email" @@=
      editTextField (emailL . Email.rawL) EmailField (Just 1)
  ]

  where
    withLabel lbl w =
      padBottom (Pad 1) $
       (vLimit 1 $ hLimit 15 $ str lbl <+> fill ' ') <+> w

attrs :: AttrMap
attrs = attrMap V.defAttr
  [ (Edit.editAttr,        V.white `on` V.black)
  , (Edit.editFocusedAttr, V.black `on` V.yellow)
  , (invalidFormInputAttr, V.white `on` V.red)
  , (focusedFormInputAttr, V.black `on` V.yellow)
  ]

draw :: Form Registration event Field -> [Widget Field]
draw appForm = [Center.vCenter $ Center.hCenter form <=> Center.hCenter help]
    where
        form =
          appForm
            |> renderForm
            |> hLimit 50
            |> padTop (Pad 1)
            |> Border.border

        help = padTop (Pad 1) $ Border.borderWithLabel (str "Help") body
        body = str $ mconcat
          [ "- Free form text\n"
          , "- Enter/Esc quit, mouse interacts with fields"
          ]

app :: App (Form Registration event Field) event Field
app =
    App { appDraw         = draw
        , appChooseCursor = focusRingCursor formFocus
        , appStartEvent   = pure
        , appAttrMap      = \_ -> attrs
        , appHandleEvent  = eventHandler
        }

eventHandler ::
     Form Registration event Field
  -> BrickEvent Field event
  -> EventM Field (Next (Form Registration event Field))
eventHandler state = \case
  VtyEvent (V.EvResize {})     ->
    continue state

  VtyEvent (V.EvKey V.KEsc []) ->
    halt state

  VtyEvent (V.EvKey V.KEnter []) ->
    halt state

  event -> do
    s' <- handleFormEvent event state
    continue s'


run :: (MonadIO m, MonadLogger m) => m ()
run = do
  initialVty <- liftIO buildVty
  f'         <- liftIO $ customMain initialVty buildVty Nothing app $ toForm initialForm

  logInfo @Text "The starting form state was:"
  logInfo $ displayShow initialForm

  logInfo @Text "The final form state was:"
  logInfo . displayShow $ formState f'

  if allFieldsValid f'
     then logInfo @Text "The final form inputs were valid."
     else logInfo $ "The final form had invalid inputs: " <> show (invalidFields f')

  where
    buildVty = do
      v <- V.mkVty =<< V.standardIOConfig
      V.setMode (V.outputIface v) V.Mouse True
      return v

    initialForm =
      Registration
        { regUsername = Username ""
        , regEmail    = Email    ""
        }
