{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}

module Db.Measurement where

import           Database.Record.TH.SQLite3 (defineTable)

$(defineTable "db/database.db3" "Measurement")
