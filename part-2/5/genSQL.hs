{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

import Data.Text as T hiding (zip)
import Data.Text.IO as TIO
import Control.Monad.Writer
import Data.Foldable

type SQL = Text
 
data ErrorMsg = WrongFormat Int Text
  deriving Show
genInsert s1 s2 =
  "INSERT INTO items VALUES ('" <> s1 <> "','" <> s2 <> "');\n"

processLine :: (Int, Text) -> Writer [ErrorMsg] SQL
processLine (_, T.splitOn ":" -> [s1, s2]) = pure $ genInsert s1 s2
processLine (i, s) = tell [WrongFormat i s] >> pure ""


genSQL :: Text -> Writer [ErrorMsg] SQL
genSQL txt = T.concat <$> traverse processLine (zip [1..] $ T.lines txt)

testData :: Text
testData = "Pen:Bob\nGlass:Mary:10\nPencil:Alice\nBook:Bob\nBottle"

testGenSQL :: IO ()
testGenSQL = do
  let (sql, errors) = runWriter (genSQL testData)
  TIO.putStrLn "SQL:"
  TIO.putStr sql
  TIO.putStrLn "Errors:"
  traverse_ print errors
