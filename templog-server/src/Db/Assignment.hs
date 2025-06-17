{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}

module Db.Assignment where

import           Database.Record.TH.SQLite3 (defineTable)

$(defineTable "db/database.db" "Assignment")
