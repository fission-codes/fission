-- | 

module Fission.Internal.RSA2048.Pair.Types where

import Crypto.PubKey.RSA
import Prelude (read)

-- import Fission.Prelude

-- data Pair = Pair
--   { pk :: Text
--   , sk :: PrivateKey
--   }

-- instance Arbitrary Pair where
--   arbitrary = elements pregenerated

-- pregenerated :: [Pair]
-- pregenerated =
--   [ Pair pk1 sk1
--   , Pair pk1 sk1
--   ]

-- pk1 :: Text
-- pk1 = mconcat
--   [ "AAAAB3NzaC1yc2EAAAADAQABAAABgQDR47+K9gO0dgLGBaqYIzPXfm63K7NGH3utELhhXkj"
--   , "fes8ioWjeKQqUW354mBhGvA7jaVJ++Y6MqN5HXcCnDv2bSZ2uApDzmGIczcj1icAaP4PYty"
--   , "coV2Vqp0royw9RQ6qOHhIpf/39CzZb6sE85qBC6mAqqmM7VaIOs3ifasRjF1cR+GTKlRDZt"
--   , "ffCRntg++oK800t0esGdLeo2R8SP5/LyjYMGBLm/Wop8WcJwJ+V1BfXP4kE6g1L0eRD+mtV"
--   , "q7yMG1qPWVsY14FerxaAn/AH+/egYf3nfAFMP9ww494T2BP0jlqf2v+y6CNMwHUZRJF3chp"
--   , "ukqtlOE/bLhSk8gCQrJL9CYWds+CyPgjMuUV+kQGR08DOzT2fWmmSbyl6aE3Vedc9E16c26"
--   , "UbtW3hPwp5TyehU+HW2cGru4UHHt3hB7jv1wxCZWOEaHqadzU/TtDEOc9ewGk7q++lPkRvd"
--   , "xLAlpL7Z8Qow0wcwOQ3JIWQD5ubnwgifAntvGx2iPZFDj8="
--   ]

-- sk1 :: PrivateKey
-- sk1 = read $ mconcat
--   [ "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"
--   ]
