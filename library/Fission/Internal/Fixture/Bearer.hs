{-# OPTIONS_GHC -fno-warn-incomplete-uni-patterns #-}

module Fission.Internal.Fixture.Bearer
  ( jsonRSA2048
  , tokenRSA2048
  , jwtRSA2048
  , rawContent
  , validTime
  ) where

import           Servant.API

import           Fission.Prelude
import qualified Fission.Web.Auth.Token.Bearer.Types as Bearer
import           Fission.Web.Auth.Token.JWT

validTime :: UTCTime
validTime = fromSeconds 0 -- i.e. waaaay before the expiry :P

rawContent :: Text
rawContent = "eyJhbGciOiJSUzI1NiIsInR5cCI6IkpXVCJ9.eyJpc3MiOiJkaWQ6a2V5OnoxM1YzU29nMllhVUtoZEdDbWd4OVVadVcxbzFTaEZKWWM2RHZHWWU3TlR0Njg5Tm9MM3pCeG9kU2FZeVg1VjZ6VjVTNGlZbWJ3WWRxbWlGWlhMaVp0VWFnY2JBSnZQNURQYzNaUGE5MmJmTWRwUEVMWXhMckhETWJBSDhUdzVZeGVSZVU1TkRIRWRjSjl5d0RFTnlZWExwMTNWYXFXMzF2bjhtMmZLOUQ0Z0Z2dUNVSHFMa3RaYVgxQjdBNXF4eXN1WXR3YVNGbW5nU2lKaUtHRzc5d0FpMXkxc3FwWXVhWDUyOXd0QVpOUXJBempiaGVwRUtwYjhNZHE5V2tvc3ZQdDNxb2JYQzZ1UnVodkFaZ2h5MnRaREFrU05LOWFIRWtuWDV5TGRoOEFFQVZXRUU5b3ZQbzl0VVpMRFpXWXJtY255SjdrU2RnN2JGa0hmb2dwYlpTb2JVZFVLcVR0QWhGeTdZSGkxWEg0c2pMS0Izbkhaa3hKdGthY2VtTWdFY1ZNdkMxR1hLWnVxTTFQZlJURzlxdHpDIiwiZXhwIjoxNTg2ODk1NDg1fQ"

jsonRSA2048 :: Text
jsonRSA2048 = "Bearer " <> rawContent <> ".FAFBniucAd-Tyab4Y04Mcreeqo7Yenc3AQMrscU5_R2s8ZViRVaadMW77WHjzuEkOdSIteA2_5tP62LIrew5ZJmLYPCkDZujZ2fFo2R-QIg3rxuCduN29Wvb9p0iko57v56w_io3Rpb9O296mmLdfUqcV9zxjG_AlpSLY03iYgfNTivoRXJCyUNPV5kfpm0vvhyyL7D-mxTM1Ntd65qVfVJSBV_5jpEy82p1EGIjQUv-hkyGq4Lj-g2t_M98QommYQl1vVcjVkRsjdmgibzpIPPBYNOuRbJ2JPWMHqtPwQpwwlfUgznzqPerLUIRLh2WYExU9pwdADIq1zuGEFrecw"

tokenRSA2048 :: Bearer.Token
jwtRSA2048   :: JWT

Right tokenRSA2048@(Bearer.Token jwtRSA2048 _) = parseUrlPiece jsonRSA2048
