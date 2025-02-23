{-# LANGUAGE OverloadedStrings #-}

module TokenController(createUserToken, validateUserToken) where

import Web.Scotty ( body, header, status, ActionM )
import Web.Scotty.Internal.Types (ActionT)
import Web.Scotty.Trans (ScottyT, get, json, put)
import Network.Wai
import Network.HTTP.Types.Status
import Data.Time
import Data.Aeson (FromJSON, ToJSON, encode, decode)
import Data.Time.Clock.POSIX
import Data.Text (Text, unpack, pack)
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TL
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
import qualified Token as Tkn
import Realm
import TokenModel
import Control.Monad.Trans.Class (MonadTrans(lift))


--- AUTH
createUserToken conn =  do
        maybeClientId<- header "x-client-id"
        maybeClientSecret <- header "x-client-secret"
        maybeGrantType <- header "x-grant-type"
        case (maybeClientId, maybeClientSecret, maybeGrantType) of
            (Just a, Just b, Just c) -> do 
                result <- liftIO $ findRealm (TL.toStrict b) conn
                liftIO $ print result
                case result of
                    Left _ -> do
                        jsonResponse (ErrorMessage "Error creating token")
                        status internalServerError500
                    Right realm -> case realm of 
                        [] -> do
                            jsonResponse (ErrorMessage "Invalid client secret")
                            status unauthorized401
                        [r] -> do
                            let grantType = getGrantType r
                            let clientid = getClientId r
                            if TL.toStrict a == getClientId r then 
                                createToken conn clientid grantType
                            else do
                                jsonResponse (ErrorMessage "Invalid client secret")
                                status unauthorized401
            _ -> do
                jsonResponse (ErrorMessage "Invalid request")
                status badRequest400


createToken conn clientid granttype= do
        curTime <- liftIO getPOSIXTime
        let expDate = tokenExpiration curTime
        let token = buildToken granttype expDate
        liftIO $ Tkn.insertToken token clientid conn
        jsonResponse (TokenResponse (buildToken granttype expDate) "JWT" "" )
                   

validateUserToken conn = do
                curTime <- liftIO getPOSIXTime
                authHeader <- header "Authorization"
                maybeClientId<- header "x-client-id"
                case  (authHeader >>= extractBearerAuth. T.encodeUtf8 . TL.toStrict, maybeClientId)  of
                        (Just auth, Just clientId) -> case decodeToken (T.decodeUtf8 auth) of
                                        Nothing -> do
                                                jsonResponse (ErrorMessage "Invalid token payload")
                                                status unauthorized401
                                        Just payload -> if tokenExperitionTime payload  >= toInt64 curTime then do
                                                            result <- liftIO $ Tkn.findToken (T.decodeUtf8 auth) conn
                                                            case result of
                                                                Left _ -> do
                                                                    jsonResponse (ErrorMessage "Error validating token")
                                                                    status internalServerError500
                                                                Right token -> case token of
                                                                    [] -> do
                                                                        jsonResponse (ErrorMessage "Invalid token")
                                                                        status unauthorized401
                                                                    [t] -> if Tkn.getClientId t == TL.toStrict clientId then do
                                                                                jsonResponse (TokenResponse (T.decodeUtf8 auth) "JWT" "")
                                                                                status ok200
                                                                            else do
                                                                                jsonResponse $ ErrorMessage "Token expired"
                                                                                status unauthorized401
                                        else do
                                            jsonResponse $ ErrorMessage "Token expired"
                                            status unauthorized401
                        _ -> do
                            jsonResponse (ErrorMessage "Invalid request")
                            status unauthorized401
                        


-- Token helpers
convertToString :: Text -> Int64 -> [Char]
convertToString u t = BL.unpackChars (encode $ Payload u t)

buildToken :: Text -> Int64 -> Text
buildToken u  t = case token of
                        Left _ -> ""
                        Right (Jwt jwt) -> T.decodeUtf8 jwt
                    where
                        payload = BI.packChars $ convertToString u t
                        token = hmacEncode HS256 "eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9" payload

decodeToken :: Text -> Maybe Payload
decodeToken t = case token of
                    Left _ -> Nothing
                    Right (_, jwt) -> convertToPayload jwt
                where
                    token = hmacDecode "eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9" $ tokenFromHeader (TL.fromStrict t)
                    tokenFromHeader t = BL.toStrict $ TL.encodeUtf8 t


--- Helper Functions
convertToPayload :: BI.ByteString -> Maybe Payload
convertToPayload t = ( decode $  BL.packChars $ BI.unpackChars t ) :: Maybe Payload

tokenExpiration :: NominalDiffTime -> Int64
tokenExpiration u = toInt64 u + 864000

toInt64 :: NominalDiffTime -> Int64
toInt64 = floor . nominalDiffTimeToSeconds

tokenExperitionTime :: Payload -> Int64
tokenExperitionTime (Payload u e) = e