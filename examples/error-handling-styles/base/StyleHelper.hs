{-# LANGUAGE GHC2021, DataKinds #-}
module StyleHelper where
import qualified Helper as Hp
import H
helperH :: Handlers
helperH = Handlers Hp.getUser Hp.register Hp.checkWrite Hp.findUser Hp.grantRead Hp.uploadNames
                  Hp.prefsFor Hp.displayName Hp.useQuota Hp.canonical Hp.shareWithMany
