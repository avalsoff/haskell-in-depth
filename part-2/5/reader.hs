{-# LANGUAGE NamedFieldPuns #-}

import Control.Monad.Reader

data Config = Config {
    verbose :: Bool
  , slow :: Bool
}

type ConfigM = Reader Config

getConfiguration :: IO Config
getConfiguration = pure Config { verbose = True, slow = False }

main :: IO ()
main = do
  config <- getConfiguration
  let result = runReader work config
  print result

work :: ConfigM ()
work = do
  -- ...
  doSomething
  -- ...

doSomething :: ConfigM ()
doSomething = do
  -- ...
  doSomethingSpecial
  -- ...

doSomethingSpecial :: ConfigM ()
doSomethingSpecial = do
  -- ...
  Config {verbose} <- ask
  -- vrb <- asks verbose
  when verbose beVerbose
  -- ...

beVerbose :: ConfigM ()
beVerbose = pure ()

silent :: Config -> Config
silent config = config {verbose = False}

doSomethingSpecialSilently :: ConfigM ()
doSomethingSpecialSilently = local silent doSomethingSpecial
