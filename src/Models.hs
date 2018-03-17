{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE RecordWildCards            #-}

module Models where

import Data.Aeson
import Data.Text
import Data.Time

import Database.Persist.TH

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
User
  name Text maxlen=20
  loginId Text maxlen=20
  password Text
  UniqueLoginId loginId
  deriving Eq Read Show
BlogPost
  title Text
  body Text
  createdAt UTCTime default=CURRENT_TIME
  lastEditedAt UTCTime default=CURRENT_TIME
  private Bool
  authorId UserId
  photos [PhotoId] default = []
  tags [TagId] default = []
  deriving Eq Read Show
Tag
  name Text
  deriving Eq Read Show
Photo
  filename Text
  blogPostId BlogPostId
  deriving Eq Read Show
TagPost
  blogPostId BlogPostId
  tagId TagId
  UniquePostTag blogPostId tagId
  deriving Eq Read Show
|]

heejong :: User
heejong = User "안희종" "heejongahn" "secret"

hajin :: User
hajin = User "심하진" "shimazing" "cannot guess"

users :: [User]
users = [heejong, hajin]

instance FromJSON User where
  parseJSON = withObject "User" $ \v -> do
    userName <- v .: "name"
    userLoginId <- v .: "loginId"
    userPassword <- v .: "password"

    return User{..}

instance ToJSON User where
  toJSON (User name loginId password) =
    object [ "name" .= name
           , "loginId" .= loginId
           , "password"  .= password]
