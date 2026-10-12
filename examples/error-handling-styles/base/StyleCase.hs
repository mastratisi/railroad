{-# LANGUAGE GHC2021, DataKinds #-}
module StyleCase where
import qualified Case as C
import H
caseH :: Handlers
caseH = Handlers C.getUser C.register C.checkWrite C.findUser C.grantRead C.uploadNames
                  C.prefsFor C.displayName C.useQuota C.canonical C.shareWithMany
