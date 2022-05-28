module Configuration where

data Configuration = Configuration { getGithubRoot :: String ,
                                     getGithubAccessToken :: String,
                                     getHostname :: String
                                   }

defaultConfiguration :: Configuration
defaultConfiguration = Configuration { getGithubRoot = "http://localhost:4000/github/" ,
                                       getGithubAccessToken = "",
                                       getHostname = "localhost"
                                     }
  
updateGithubRoot :: Maybe String -> Configuration -> Configuration
updateGithubRoot (Just s) config = config { getGithubRoot = s }
updateGithubRoot Nothing config = config

updateGithubAccessToken :: Maybe String -> Configuration -> Configuration
updateGithubAccessToken (Just s) config = config { getGithubAccessToken = s }
updateGithubAccessToken Nothing config = config

updateHostname :: Maybe String -> Configuration -> Configuration
updateHostname (Just s) config = config { getHostname = s }
updateHostname Nothing config = config
