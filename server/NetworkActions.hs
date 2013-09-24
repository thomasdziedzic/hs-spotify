{-# LANGUAGE DeriveGeneric #-}

module NetworkActions (
      NetworkAction(..)
) where

import Data.Binary (Binary)
import GHC.Generics (Generic)
import qualified Data.ByteString.Lazy.Char8 as C

type Username = C.ByteString
type Password = C.ByteString

data NetworkAction = Login Username Password
    deriving (Show, Generic)

instance Binary NetworkAction
