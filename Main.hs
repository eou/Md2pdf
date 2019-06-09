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

main :: IO ()
main = do
    args <- getArgs

    let inputMdFileName = args !! 0
    inputMdFile <- readFile inputMdFileName
    -- print inputMdFile

    let markdown = mdProcessor inputMdFile
    -- print markdown

    generatePdfFile inputMdFileName markdown

