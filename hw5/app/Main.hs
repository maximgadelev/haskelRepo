module Main where

import CSVParser
import Songs
import ParsingDataRows
import SongsFunctions

import Text.Megaparsec (errorBundlePretty)
import System.IO


main :: IO ()
main = do
  let filePath = "spotify_songs.csv"
      delimiter = ','
      settings = CSVParserSettings delimiter True

  csvFileContent <- readFile filePath

  case parseCSV delimiter settings csvFileContent of
    Left err -> putStrLn $ "Error parsing CSV:\n" ++ errorBundlePretty err
    Right csv -> do
      let dataRows = filterValidDataRows (map dataRowParser (rows csv))

      putStrLn $ "Total number of data rows: " ++ show (length dataRows)

      let albums = dataRowsToAlbums dataRows


      putStrLn $ "Total number of albums: " ++ show (length albums)

      let targetYear = 2002
      let albumsFromTargetYear = findAlbumsByYear targetYear albums


      putStrLn $ "Total number of albums released in " ++ show targetYear ++ ": " ++ show (length albumsFromTargetYear)

      let longestAlbum = findLongestAlbum albums
      case longestAlbum of
        Just album -> do
          putStrLn $ "\nLongest Album from " ++ show targetYear ++ " year with duration " ++ show (totalAlbumDuration album) ++ ": "
          putStrLn $ "Name: " ++ _albumName (albumRef album)
          putStrLn $ "Release Date: " ++ _albumReleaseDate (albumRef album)
        Nothing ->
          putStrLn $ "No longest album found for the year " ++ show targetYear
