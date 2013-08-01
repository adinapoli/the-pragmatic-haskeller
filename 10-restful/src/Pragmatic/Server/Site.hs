{-# LANGUAGE OverloadedStrings #-}

module Pragmatic.Server.Site (app) where

import Data.Aeson
import Data.AesonBson
import Data.ByteString (ByteString)
import Data.Configurator
import qualified Data.Text as T
import Database.MongoDB
import Pragmatic.Server.Application
import Pragmatic.Types
import Snap
import Snap.Util.FileServe
import Snap.Snaplet.MongoDB
import Snap.Snaplet.Heist
import qualified Heist.Interpreted as I
import qualified Data.ByteString.Lazy as BL
import qualified Snap.Extras as Extras
import Debug.Trace (traceIO)


handleIndex :: AppHandler ()
handleIndex = render "index"

-------------------------------------------------------------------------------
-- Show the underlying Haskell data structure of recipe.json
handleShow :: AppHandler ()
handleShow = do
    toParse <- liftIO $ BL.readFile "recipe.json"
    writeText $ eitherParse toParse
  where eitherParse tp = case (eitherDecode' tp :: Either String Object) of
                           Left e -> T.pack e
                           Right r -> T.pack . show $ r


-------------------------------------------------------------------------------
-- Here we try to store the recipe.json with the new Data.AesonBson
handleStore :: AppHandler ()
handleStore = do
    toParse <- liftIO $ BL.readFile "recipe.json"
    result <- storeRecipe toParse
    writeText $ T.pack . show $ result

handleRegister :: AppHandler ()
handleRegister = undefined


-------------------------------------------------------------------------------
parseRecipe :: BL.ByteString -> Either String Object
parseRecipe = eitherDecode'

-------------------------------------------------------------------------------
storeRecipe :: BL.ByteString -> AppHandler (Either String Object)
storeRecipe recipe = case parseRecipe recipe of
      Left f -> return $ Left f
      Right r -> do
        res <- eitherWithDB $ insert "recipes" $ toBson r
        case res of
          Left _ -> return $ Left "Failed to store the recipe."
          Right _ -> return $ Right r


-------------------------------------------------------------------------------
routes :: [(ByteString, AppHandler ())]
routes = basicRoutes

type StatusCode = Int

data AuthenticationRejection = AuthenticationRejection
    { rejectionCode :: StatusCode
    , rejectionMessage :: ByteString } deriving (Show)


tokenAuthentication :: Handler b v a -> Either AuthenticationRejection (Handler b v a)
tokenAuthentication hdlr = return hdlr


alwaysFailAuthenticator :: Handler b v a -> Either AuthenticationRejection (Handler b v a)
alwaysFailAuthenticator hdlr = Left (AuthenticationRejection 404 "You shall not pass!")


-------------------------------------------------------------------------------
-- Restful Authenticator, inspired to [Spray](http://spray.io/)
withRestAuth :: (Handler b v a -> Either AuthenticationRejection (Handler b v a))
                -> Handler b v a
                -> Handler b v a
withRestAuth authFunction hdlr = either
    (\rej -> Extras.finishEarly (rejectionCode rej) (rejectionMessage rej))
    id (authFunction hdlr)

-------------------------------------------------------------------------------
basicRoutes :: [(ByteString, AppHandler ())]
basicRoutes = [ ("/", withRestAuth tokenAuthentication handleIndex)
              , ("/secret", withRestAuth alwaysFailAuthenticator handleIndex)
              , ("/register/:userid/", handleRegister)
              , ("/store", handleStore)
              , ("/static", serveDirectory "static")]


-------------------------------------------------------------------------------
app :: SnapletInit Pragmatic Pragmatic
app = makeSnaplet "pragmatic" "Pragmatic web service" Nothing $ do
    conf <- getSnapletUserConfig
    dbName <- liftIO $ require conf "pragmatic.db"
    dbHost <-  liftIO $ require conf "pragmatic.host"
    h <- nestSnaplet "" heist $ heistInit "templates"
    d <- nestSnaplet "db" db $ mongoDBInit 10 (host dbHost) dbName
    addRoutes routes
    return $ Pragmatic h d
