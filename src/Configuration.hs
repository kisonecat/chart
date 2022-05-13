module Configuration where


data Configuration = Configuration { githubRoot :: String ,
                                     githubAccessToken :: String
                                   }

defaultConfiguration :: Configuration
defaultConfiguration = Configuration { githubRoot = "http://localhost:4000/github/" ,
                                       githubAccessToken = ""
                                     }
  
updateGithubRoot :: Maybe String -> Configuration -> Configuration
updateGithubRoot (Just s) config = config { githubRoot = s }
updateGithubRoot Nothing config = config

updateGithubAccessToken :: Maybe String -> Configuration -> Configuration
updateGithubAccessToken (Just s) config = config { githubAccessToken = s }
updateGithubAccessToken Nothing config = config
