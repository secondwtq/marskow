{-# LANGUAGE OverloadedStrings          #-}

module Main where

-- import ModuleTest
import Text.Marskow (
  MarskowT(..),
  runMarskowT,
  blocks,
  liftInterpreter,
  runMarskowTWithInterpreter,
  foldBlocks)

import Data.Text (Text)
import Data.Default (def)
import Data.String (fromString)

import System.IO
import System.Environment

import qualified Cheapskate as C
import qualified Cheapskate.Types as C.T
import qualified Language.Haskell.Interpreter as I
import Language.Haskell.Interpreter (OptionVal(..))

import qualified Text.Blaze.Html
import qualified Text.Blaze.Renderer.String

parseMarkdown :: Text -> C.T.Blocks
parseMarkdown text = case C.markdown def text of
    (C.T.Doc options blocks) -> blocks

testInterpreter :: I.Interpreter Int
testInterpreter = do
  I.set [I.searchPath := ["/Users/ooolive/Desktop"]]
  I.loadModules ["TestModule"]
  I.setImportsQ [("Prelude", Nothing), ("TestModule", Just "Test")]
  I.interpret "a" (I.as :: Int)

main :: IO ()
main = do
    args <- getArgs
    withFile (head args) ReadMode executeFile
      where
          executeFile handle = do
              contents <- hGetContents handle
              let blocks = parseMarkdown (fromString contents)
              result <- runMarskowT (doMarskow blocks)
              case result of
                Left error -> print error
                Right (_, blocksProcessed) -> do
                  putStrLn . Text.Blaze.Renderer.String.renderMarkup . Text.Blaze.Html.toHtml $ C.T.Doc def blocksProcessed
                  return ()
          doMarskow blocks = do
            liftInterpreter $ do
              I.set [I.languageExtensions := [I.OverloadedStrings]]
              I.set [I.searchPath := ["/Users/ooolive/Desktop"]]
              I.loadModules ["TestModule"]
              I.setImportsQ modulesToLoad
            foldBlocks blocks
          modulesToLoad = [
            ("Prelude", Nothing),
            ("System.Exit", Nothing),
            ("System.Process.Typed", Nothing),
            ("TestModule", Nothing),
            ("Text.Marskow", Nothing)
                          ]
