{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module Fission.Web.Client.V2
  ( appIndex
  , createApp
  , updateApp
  , destroyApp
  --
  , getIPFSPeers
  --
  , createUser
  , whoAmI
  --
  , getDataRoot
  , setDataRoot
  --
  , setDIDViaUCAN
  , setDIDViaChallenge
  --
  , recoverViaChallenge
  --
  , verifyViaEmail
  , resendVerificationEmail
  , recoverViaEmail
  --
  , verifyUCAN
  ) where

import           Servant.Client.Generic

import           Fission.Web.API.Prelude

import qualified Fission.Web.API.App.Types                        as App
import qualified Fission.Web.API.Auth.UCAN.Types                  as Auth.UCAN
import qualified Fission.Web.API.IPFS.Peer.Types                  as IPFS.Peer
import qualified Fission.Web.API.IPFS.Types                       as IPFS
import qualified Fission.Web.API.Types                            as Fission

import qualified Fission.Web.API.User.DID.Types                   as User.DID
import qualified Fission.Web.API.User.DataRoot.Types              as User.DataRoot
import qualified Fission.Web.API.User.Challenge.Types             as User.Challenge
import qualified Fission.Web.API.User.Email.Types                 as User.Email
import qualified Fission.Web.API.User.Types                       as User
import qualified Fission.Web.API.User.WhoAmI.Types                as User.WhoAmI

import           Fission.Web.Client.Internal.Orphanage.Blaze.HTML ()
import           Fission.Web.Client.Internal.Orphanage.WebSocket  ()

Fission.Routes
  { v2 = fromServant @_ @(AsClientT ClientM)->
      Fission.RoutesV2
        { api = fromServant @_ @(AsClientT ClientM) ->
            Fission.V2
              { app = fromServant @_ @(AsClientT ClientM) ->
                  App.RoutesV2
                    { index   = appIndex
                    , create  = createApp
                    , update  = updateApp
                    , destroy = destroyApp
                    }

              , auth = fromServant @_ @(AsClientT ClientM) ->
                  Auth.UCAN.Routes {verify = verifyUCAN}

              , ipfs = fromServant @_ @(AsClientT ClientM) ->
                  IPFS.RoutesV2
                    { peers = fromServant @_ @(AsClientT ClientM) ->
                        IPFS.Peer.Routes {index = getIPFSPeers}
                    }

            , user = fromServant @_ @(AsClientT ClientM) ->
                User.RoutesV2
                  { create   = createUser
                  , dataRoot = fromServant @_ @(AsClientT ClientM) ->
                      User.DataRoot.RoutesV2
                        { get    = getDataRoot
                        , update = setDataRoot
                        }

                  , whoAmI = fromServant @_ @(AsClientT ClientM) ->
                      User.WhoAmI.Routes { whoAmI }

                  , did = fromServant @_ @(AsClientT ClientM) ->
                      User.DID.RoutesV_
                        { setAuthenticated = setDIDViaUCAN
                        , setViaChallenge  = setDIDViaChallenge
                        }

                  , challenge = fromServant @_ @(AsClientT ClientM) ->
                      User.Challenge.Routes
                        { recover = recoverViaChallenge
                        }

                  , email = fromServant @_ @(AsClientT ClientM) ->
                      User.Email.Routes
                        { verify  = verifyViaEmail
                        , resend  = resendVerificationEmail
                        , recover = recoverViaEmail
                        }
                  }
            }
        }
  } = genericClient
