{-
    File : Main.hs
    Copyright : (c) Hangyi Wang (hangyi)
    Md2pdf
-}

module Main where


import Block
import MdProc
import PdfProc

import System.Environment
import System.IO
import System.FilePath.Find
import System.FilePath.Posix

main :: IO ()
main = do
    args <- getArgs

    if length args == 0
        -- invalid arguments
        then error "Please provide correct input."
        else do
            let ext = takeExtension $ args !! 0
            if length ext == 0
                -- directory, get all md files in it
                then do 
                    mdFiles <- getAllMdFiles $ args !! 0
                    mdContents <- mapM readFile mdFiles
                    let mdLists = zip mdFiles mdContents
                    sequence_ $ fmap (\(path, content) -> generatePdfFile path (mdProcessor content)) mdLists
                -- list of files
                else do
                    let mdFiles = args
                    mdContents <- mapM readFile mdFiles
                    let mdLists = zip mdFiles mdContents
                    sequence_ $ fmap (\(path, content) -> generatePdfFile path (mdProcessor content)) mdLists


{-
    getAllMdFiles: return all markdown files in the directory
    reference: https://stackoverflow.com/questions/51712083/recursively-search-directories-for-all-files-matching-name-criteria-in-haskell
-}
getAllMdFiles :: FilePath -> IO [FilePath]
getAllMdFiles path = find isVisible (isMdFile &&? isVisible) path
    where
      isMdFile = extension ==? ".md"
      isVisible = fileName /~? ".?*"

