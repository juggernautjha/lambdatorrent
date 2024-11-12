
{-# LANGUAGE OverloadedStrings #-}

import Network.Wreq
import Control.Lens ((&), (.~), (^.))
import Network.HTTP.Client (newManager)
import Network.HTTP.Client.TLS (mkManagerSettings)
import Network.TLS (defaultParamsClient, supportedVersions, supportedCiphers)
import Network.TLS (Version(..))
import Network.Connection (TLSSettings(..))
import Data.Default (def)
import Data.ByteString(ByteString)
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString as BS
import Bencode

-- url :: String
-- url = "http://torrent.ubuntu.com/announce?info_hash=%3F%9A%AC%15%8C%7D%E8%DF%CA%B1q%EAX%A1z%AB%DF%7F%BC%93&peer_id=%1C%95%76%9B%AA%35%60%3B%87%AD%44%CD%91%04%0D%07%0D%9D%00%9F&port=1&uploaded=0&downloaded=0&left=1&event=started"

main :: IO BS.ByteString
main = do
  -- Create TLS settings specifying TLS 1.2 as the supported version
  let tlsSettings = TLSSettingsSimple
        { settingDisableCertificateValidation = False  -- Enable cert validation
        , settingDisableSession = False                -- Enable session resumption
        , settingUseServerName = True
        , settingClientSupported = def { supportedVersions = [TLS11] }
    
        }



  -- Create manager settings with TLS 1.2 using mkManagerSettings
  let managerSettings = mkManagerSettings tlsSettings Nothing

  -- Create the manager with the custom settings
  mgr <- newManager managerSettings

  -- Configure Wreq with the custom manager
  let opts = defaults & manager .~ Right mgr

  -- Perform the HTTP request
  r <- getWith opts url
  
  pure (BL.toStrict (r ^. responseBody))

url = "http://kiryuu-test.mywaifu.best:6969/announce?info_hash=%3F%9A%AC%15%8C%7D%E8%DF%CA%B1q%EAX%A1z%AB%DF%7F%BC%93&peer_id=%1C%95%76%9B%AA%35%60%3B%87%AD%44%CD%91%04%0D%07%0D%9D%00%9F&port=1&uploaded=0&downloaded=0&left=1&event=completed"


-- main :: IO ()
-- main = do
--     let tlsSettings = TLSSettings $ def { clientSupported = def
--           { supportedVersions = [TLS12]
--           , supportedCiphers = ciphersuite_all
--           }
--       }
--     let managerSettings = mkManagerSettings tlsSettings Nothing
--     opts <- defaults & manager .~ Left (newManager managerSettings)
--     r <- getWith opts url
--     print r
-- main :: IO ()
-- main = runReq defaultHttpConfig $ do
--     r <- req GET (https url /: "path") NoReqBody bsResponse mempty
--     liftIO $ print (responseBody r :: ByteString)
