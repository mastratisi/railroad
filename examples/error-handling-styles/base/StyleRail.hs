{-# LANGUAGE GHC2021, DataKinds #-}
module StyleRail where
import qualified Rail as R
import H
railH :: Handlers
railH = Handlers R.getUser R.register R.checkWrite R.findUser R.grantRead R.uploadNames
                  R.prefsFor R.displayName R.useQuota R.canonical R.shareWithMany
