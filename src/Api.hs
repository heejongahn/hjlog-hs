
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE TypeOperators     #-}

module Api where

import Data.Proxy
import Data.Text

import Database.Persist

import Models

import Servant.API

type Api =
       "users" :> Get '[JSON] [User]
  :<|> "user" :> ReqBody '[JSON] User :> Post '[JSON] (Maybe (Key User))
  :<|> "user" :> Capture "name" Text  :> Get  '[JSON] (Maybe User)

api :: Proxy Api
api = Proxy
