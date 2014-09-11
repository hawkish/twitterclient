{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}

-- Due to cabal dependency issues install like so: 
-- cabal install lens authenticate-oauth-1.4.0.8 aeson http-conduit


import Network.HTTP (getResponseBody, getRequest, simpleHTTP, urlEncode)
import Data.List (isInfixOf, or)
-- import Data.Text
import Data.Text hiding (pack)
import System.Environment    
import System.IO hiding (putStrLn)   
import System.IO.Error
import Control.Exception
import Prelude hiding (catch, lookup, putStrLn)
import Control.Applicative
import Data.ByteString.Lazy.Char8 as LB8
import Data.ByteString.Char8 as B8
import Control.Monad
import GHC.Generics
import Network.HTTP.Conduit
import Web.Authenticate.OAuth
import Data.Aeson
import Data.Maybe
import Data.Time.Clock (UTCTime)
import GHC.Generics
import Data.Configurator
import Data.Configurator.Types


data Tweet = Tweet { text :: Text
                   , createdAt :: Text
                   , user :: User
                   } deriving (Eq, Show)

data User = User { followersCount :: Int
                 , friendsCount :: Int
                 } deriving (Eq, Show)


instance FromJSON Tweet where
         parseJSON (Object v) =
                   Tweet <$> v .: "text"
                         <*> v .: "created_at"
                         <*> v .: "user"
                   
         parseJSON _ = mzero


instance FromJSON User where
         parseJSON (Object v) =
                   User <$> v .: "followers_count"
                        <*> v .: "friends_count"
         parseJSON _ = mzero

getFollowersCount :: Tweet -> Int 
getFollowersCount = followersCount . user

-- Nasty bit of convolution.
output :: Show c => (Tweet -> c) -> [Tweet] -> IO()
output a b = mapM_ (\x -> print $ a x) b 


printTimeline :: String -> IO ()
printTimeline a  = do
    result <- userTimeline a  
    case result of
        Left ex -> print ex
        Right val -> output getFollowersCount val

userTimeline :: String -> IO (Either String [Tweet])
userTimeline a = do 
   json <- userTimelineRequest a 
   let result = eitherDecode $ json :: Either String [Tweet]
   return result

{--
userTimeline' :: String -> IO (Either SomeException [Tweet])
userTimeline' a = do 
   result <- userTimelineRequest' a
   case result of
        Left ex -> return $ Left ex
        Right val -> do
                  let decoded = eitherDecode $ val :: Either String [Tweet]
                  case decoded of
                       Left ex -> return $ Left 
                       Right val -> return $ Right val
--}

userTimelineRequest :: String -> IO LB8.ByteString
userTimelineRequest a = do
                    (cred, oauthApp) <- parseCredentials $ loadConfigFile
                    -- Firstly, we create a HTTP request with method GET (it is the default so we don't have to change that).
                    req <- parseUrl $ "https://api.twitter.com/1.1/statuses/user_timeline.json?screen_name=" ++ a ++ "&exclude_replies=true"
                    -- Using a HTTP manager, we authenticate the request and send it to get a response.
                    resp <- withManager $ \m -> do
                         -- OAuth Authentication. 'signOAuth' modifies the HTTP header adding the
                         -- appropriate authentication.
                         signedreq <- signOAuth oauthApp cred req
                         -- Send request.
                         httpLbs signedreq m
                    return $ responseBody resp


userTimelineRequest' :: String -> IO (Either SomeException LB8.ByteString)
userTimelineRequest' a = do
                    b <- parseCredentials' $ loadConfigFile'
                    case b of
                         Left ex -> return $ Left ex
                         Right val -> do
                                      let cred = fst(val)
                                      let oauthApp = snd(val)
                                      -- Firstly, we create a HTTP request with method GET (it is the default so we don't have to change that).
                                      req <- parseUrl $ "https://api.twitter.com/1.1/statuses/user_timeline.json?screen_name=" ++ a ++ "&exclude_replies=true"
                                      -- Using a HTTP manager, we authenticate the request and send it to get a response.
                                      resp <- withManager $ \m -> do
                                           -- OAuth Authentication. 'signOAuth' modifies the HTTP header adding the
                                           -- appropriate authentication.
                                           signedreq <- signOAuth oauthApp cred req
                                           -- Send request.
                                           httpLbs signedreq m
                                      return $ Right(responseBody resp)



parseCredentials :: IO Config -> IO (Credential, OAuth)
parseCredentials b = do
                 a <- b
                 serverName <- lookup a "config.oauthServerName" :: IO (Maybe String)
                 key <- lookup a "config.oauthConsumerKey" :: IO (Maybe String)
                 secretKey <- lookup a "config.oauthConsumerSecret" :: IO (Maybe String)
                 accessToken <- lookup a "config.accessToken" :: IO (Maybe String)
                 accessTokenSecret <- lookup a "config.accessTokenSecret" :: IO (Maybe String)
                 let cred = newCredential (B8.pack $ fromJust accessToken) (B8.pack $ fromJust accessTokenSecret) 
                 let oauthApp = def { oauthServerName = (fromJust serverName)
                     , oauthConsumerKey = (B8.pack $ fromJust key)
                     , oauthConsumerSecret = (B8.pack $ fromJust secretKey)
                     }
                
                 return (cred, oauthApp)


parseCredentials' :: IO (Either SomeException Config) -> IO (Either SomeException (Credential, OAuth))
parseCredentials' a = do
                config <- a
                case config of
                     Left ex -> return $ Left ex
                     Right val -> do 
                           serverName <- lookup val "config.oauthServerName" :: IO (Maybe String)
                           key <- lookup val "config.oauthConsumerKey" :: IO (Maybe String)
                           secretKey <- lookup val "config.oauthConsumerSecret" :: IO (Maybe String)
                           accessToken <- lookup val "config.accessToken" :: IO (Maybe String)
                           accessTokenSecret <- lookup val "config.accessTokenSecret" :: IO (Maybe String)
                           let cred = newCredential (B8.pack $ fromJust accessToken) (B8.pack $ fromJust accessTokenSecret) 
                           let oauthApp = def { oauthServerName = (fromJust serverName)
                                        , oauthConsumerKey = (B8.pack $ fromJust key)
                                        , oauthConsumerSecret = (B8.pack $ fromJust secretKey)
                                        }
                
                           return $ Right(cred, oauthApp)


loadConfigFile :: IO Config
loadConfigFile = do 
               result <- load [Required "/Users/morten/git/twitterclient/app.cfg"]
               return result


loadConfigFile' :: IO (Either SomeException Config)
loadConfigFile' = do 
               result <- try (load [Required "/Users/morten/git/twitterclient/app.cfg"]) :: IO (Either SomeException Config)
               return result

