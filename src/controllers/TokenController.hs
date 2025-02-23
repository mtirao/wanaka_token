{-# LANGUAGE OverloadedStrings #-}

module TokenController(createUserToken, validateUserToken) where

import Web.Scotty ( body, header, status, ActionM )
import Web.Scotty.Internal.Types (ActionT)
import Web.Scotty.Trans (ScottyT, get, json, put)
import Network.Wai
import Network.HTTP.Types.Status
import Data.Aeson
import Data.Time
import Data.Time.Clock.POSIX
import Data.Text (Text, unpack, pack)
import qualified Data.Text.Lazy as TL
import qualified Data.ByteString.Lazy.Internal as BL
import qualified Data.ByteString.Internal as BI
import qualified Data.Text.Encoding as T

import Control.Monad.IO.Class

import GHC.Int

import Jose.Jws
import Jose.Jwa
import Jose.Jwt (Jwt(Jwt), JwsHeader(JwsHeader))
import Network.Wai.Middleware.HttpAuth


import ErrorMessage
import Views
import Token
import TokenModel
import Control.Monad.Trans.Class (MonadTrans(lift))


--- AUTH
createUserToken conn =  do
        curTime <- liftIO getPOSIXTime
        let userText = "dev"
        let expDate = tokenExpiration curTime
        let token = createToken userText expDate
        result <- liftIO $ findToken userText conn
        case result of
            Left _ -> do
                    jsonResponse (ErrorMessage "Error creating token")
                    status internalServerError500
            Right a -> case a of 
                    [] -> do
                        liftIO $ insertToken token userText conn
                        jsonResponse (TokenResponse (createToken userText expDate) "JWT" "" )
                    _ -> do
                        i <- liftIO $ updateToken token userText conn
                        jsonResponse (TokenResponse (createToken userText expDate) "JWT" "" )


validateUserToken conn = do
                jsonResponse (ErrorMessage "User not found")
                status forbidden403


-- Token helpers
convertToString :: Text -> Int64 -> [Char]
convertToString u t = BL.unpackChars (encode $ Payload u t)

createToken :: Text -> Int64 -> Text
createToken u  t = case token of
                        Left _ -> ""
                        Right (Jwt jwt) -> T.decodeUtf8 jwt
                    where
                        payload = BI.packChars $ convertToString u t
                        token = hmacEncode HS256 "eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9" payload


--- Helper Functions
tokenExpiration :: NominalDiffTime -> Int64
tokenExpiration u = toInt64 u + 864000

toInt64 :: NominalDiffTime -> Int64
toInt64 = floor . nominalDiffTimeToSeconds