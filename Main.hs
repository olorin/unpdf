{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import Pdf.Toolbox.Document
import Options.Applicative
import System.IO
import Data.Text (unpack)

data ExtractorOptions = ExtractorOptions {
    argFilename :: String
}

opts :: Parser ExtractorOptions
opts = ExtractorOptions <$> argument str (metavar "FILENAME")

extractorOptionParser :: ParserInfo ExtractorOptions
extractorOptionParser =
    info (helper <*> opts)
    (fullDesc <> 
        progDesc "Outputs the textual content of a PDF file" <>
        header "unpdf - extracts the textual content of a PDF file")

extractFile :: ExtractorOptions -> IO ()
extractFile ExtractorOptions{..} = do
    res <- withBinaryFile argFilename ReadMode $ \handle ->
        runPdfWithHandle handle knownFilters $ do
            pgRoot <- document >>= documentCatalog >>= catalogPageNode 
            nPages <- pageNodeNKids pgRoot
            pages <- mapM (pageNodePageByNum pgRoot) (enumFromTo 0 (nPages - 1))
            txts <- mapM pageExtractText pages
            liftIO $ mapM_ (putStrLn . unpack) txts
    case res of
        Left badness -> hPutStr stderr $ "PDF error: " ++ show badness
        Right _ -> return ()

main :: IO ()
main = execParser extractorOptionParser >>= extractFile
