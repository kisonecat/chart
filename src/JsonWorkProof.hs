{-# LANGUAGE OverloadedStrings #-}

module JsonWorkProof (decodeJWP, verify) where

import Data.Aeson
import Data.Text ( Text ) 
import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteString.Internal as BSI
import qualified Data.ByteString as BS
import Crypto.Hash
import Data.Data
import Data.ByteArray
import Data.Bits
import Data.Time.Clock
import Data.Time.Clock.POSIX
import Data.Fixed

maybeToRight :: b -> Maybe a -> Either b a
maybeToRight _ (Just x) = Right x
maybeToRight y Nothing  = Left y
  
data Header = Header { getJwpType :: !Text
                     , getAlgorithm :: !Text
                     , getDifficulty :: Int
                     }
            deriving Show


instance FromJSON Header where
    parseJSON (Object v) = Header <$> v .: "typ"
                                  <*> v .: "alg"
                                  <*> v .: "dif" 
    parseJSON _ = pure $ Header { getJwpType = "", getAlgorithm = "", getDifficulty = 0 }

instance ToJSON Header where
    toJSON (Header jwpType algorithm difficulty) = object ["typ" .= jwpType,
                                                           "alg" .= algorithm,
                                                           "dif" .= difficulty]
    
data Payload = Payload { getExpiration :: Double
                       , getSubject :: !(Maybe Text)
                       , getAudience :: !(Maybe Text) }
            deriving Show

instance FromJSON Payload where
    parseJSON (Object v) = Payload <$> v .: "exp"
                                   <*> v .:? "sub"
                                   <*> v .:? "aud"
    parseJSON _ = pure $ Payload { getExpiration = 0 , getSubject = Nothing, getAudience = Nothing }

instance ToJSON Payload where
    toJSON (Payload exp sub aud) = object ["exp" .= exp, "sub" .= sub, "aud" .= aud]

data JWP = JWP { header :: Header,
                 payload :: Payload,
                 token :: BS.ByteString }
           deriving Show
    
decodeJWP :: BS.ByteString -> Either String JWP
decodeJWP b = do
  let splitted = BS.split (BSI.c2w '.') b

  h <- case splitted of
    (h:_:_) -> Right $ B64.decodeLenient h
    _ -> Left "A JSON work proof must have three components and a header"
  h <- maybeToRight "Could not parse JWP header" $ decodeStrict h
  
  p <- case splitted of
    (_:p:_) -> Right $ B64.decodeLenient p
    _ -> Left "A JSON work proof must have three components and a header"
  p <- maybeToRight "Could not parse JWP payload" $ decodeStrict p

  pure JWP { header = h, payload = p, token = b }

countZeros :: BS.ByteString -> Int
countZeros bs = sum $ takeWhile (> 0) $ map countLeadingZeros $ BS.unpack bs
  
difficulty :: BS.ByteString -> Int
difficulty b =
  let hashed = hashWith SHA256 b
      bs = Data.ByteArray.convert hashed in
  countZeros bs 

verifyExpiration :: JWP -> IO Bool
verifyExpiration jwp = do
  let expiry = ( realToFrac $ getExpiration $ payload jwp ) :: POSIXTime
  now <- getPOSIXTime 
  let duration = nominalDiffTimeToSeconds now - nominalDiffTimeToSeconds expiry
  let seconds = round duration :: Integer
  pure $ (seconds < 0) && (seconds > (-30 * 60))

verify :: JWP -> Int -> Text -> IO (Either String ())
verify jwp d s = do
  unexpired <- verifyExpiration jwp
  let actualDifficulty = difficulty $ token jwp
  let claimedDifficulty = getDifficulty $ header jwp
  let maybeSubject = getSubject $ payload jwp

  if Just s /= maybeSubject then
    pure $ Left "Subject mismatch"
  else if claimedDifficulty > d then
    pure $ Left "Claimed difficulty is too high"
  else if actualDifficulty < claimedDifficulty then
    pure $ Left "Actual difficulty is too low"
  else if not unexpired then
    pure $ Left "Token expired"
  else
    pure $ Right ()
