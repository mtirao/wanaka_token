{-# LANGUAGE OverloadedStrings #-}

import Web.Scotty
import Network.Wai.Middleware.RequestLogger (logStdoutDev, logStdout)
import TokenController

import Connection

main :: IO ()
main = do
    conn <- getConnection
    case conn of
        Left _ -> putStrLn "Error connecting to database"
        Right c -> scotty 3000 $ do
            middleware logStdoutDev
            post "/api/wanaka/token" $ createUserToken c
            post "/api/wanaka/token/validate" $ validateUserToken c

