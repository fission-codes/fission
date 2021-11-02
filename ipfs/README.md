# ipfs-haskell

[![Build Status](https://travis-ci.org/fission-suite/PROJECTNAME.svg?branch=master)](https://travis-ci.org/fission-suite/ipfs-haskell)
[![License](https://img.shields.io/badge/License-Apache%202.0-blue.svg)](https://github.com/fission-suite/blob/master/LICENSE)
[![Maintainability](https://api.codeclimate.com/v1/badges/44fb6a8a0cfd88bc41ef/maintainability)](https://codeclimate.com/github/fission-suite/ipfs-haskell/maintainability)
[![Built by FISSION](https://img.shields.io/badge/⌘-Built_by_FISSION-purple.svg)](https://fission.codes)
[![Discord](https://img.shields.io/discord/478735028319158273.svg)](https://discord.gg/zAQBDEq)
[![Discourse](https://img.shields.io/discourse/https/talk.fission.codes/topics)](https://talk.fission.codes)

Documentation: [ipfs on hackage](http://hackage.haskell.org/package/ipfs)

A library for integrating IPFS into your haskell applications. Interact with the IPFS network by shelling out to a local IPFS node or communicating via the HTTP interface of a remote node. 

# QuickStart

Define instances for `MonadLocalIPFS` and/or `MonadRemoteIPFS`. Each requires only one function:

```haskell
class Monad m => MonadRemoteIPFS m where
  runRemote :: Servant.ClientM a -> m (Either Servant.ClientError a)

class Monad m => MonadLocalIPFS m where
  runLocal ::
       [IPFS.Opt]
    -> Lazy.ByteString
    -> m (Either Process.Error Process.RawMessage)
```

We use RIO processes to shell out to a local IPFS node and Servant for HTTP requests to a remote node.

After that, simply add `MonadLocalIPFS m` as a constraint to a function and you'll be able to call IPFS within it.
For instance:
```haskell
import           Network.IPFS
import qualified Network.IPFS.Add        as IPFS
import           Network.IPFS.File.Types as File

add ::
  MonadLocalIPFS  m
  => File.Serialzed
  -> m ()
add (Serialized rawData) = IPFS.addRaw rawData >>= \case
  Right newCID -> 
    -- ...
  Left err ->
    -- ...

```

You can see example instances below:
```haskell
instance
  ( HasProcessContext cfg
  , HasLogFunc cfg
  , Has IPFS.BinPath cfg
  , Has IPFS.Timeout cfg
  )
  => MonadLocalIPFS (RIO cfg) where
    runLocal opts arg = do
      IPFS.BinPath ipfs <- view hasLens
      IPFS.Timeout secs <- view hasLens
      let opts' = ("--timeout=" <> show secs <> "s") : opts

      runProc readProcess ipfs (byteStringInput arg) byteStringOutput opts' >>= \case
        (ExitSuccess, contents, _) ->
          return $ Right contents
        (ExitFailure _, _, stdErr)
          | Lazy.isSuffixOf "context deadline exceeded" stdErr ->
              return . Left $ Process.Timeout secs
          | otherwise ->
            return . Left $ Process.UnknownErr stdErr

instance
  ( Has IPFS.URL     cfg
  , Has HTTP.Manager cfg
  )
  => MonadRemoteIPFS (RIO cfg) where
    runRemote query = do
      IPFS.URL url <- view hasLens
      manager      <- view hasLens

      url
        & mkClientEnv manager
        & runClientM query
        & liftIO
```
