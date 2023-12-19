{-# LANGUAGE TemplateHaskell #-}

module SongsFunctions where

import Songs
import Utils

import Control.Lens
import Data.Ord
import Data.List (sortBy, maximumBy, minimumBy)


data AlbumDurationInfo = AlbumDurationInfo
  { albumRef :: Album
  , totalAlbumDuration :: Int
  }
  deriving (Eq, Show)

makeLenses ''Track
makeLenses ''Album



findLongestAlbum :: [Album] -> Maybe AlbumDurationInfo
findLongestAlbum albums =
  case sortedAlbums of
    [] -> Nothing
    (info : _) -> Just info
  where
    albumInfos = fmap (\album -> AlbumDurationInfo album (sumOf (albumTracks.traverse.duration) album)) albums

    sortedAlbums = sortBy (comparing (Down . totalAlbumDuration)) albumInfos



averageDurationHelper :: Album -> Double
averageDurationHelper album =
  let trackDurations = map (fromIntegral . _duration) (_albumTracks album)
      totalDuration = sum trackDurations
      numTracks = length trackDurations
  in if numTracks == 0
       then 0.0
       else totalDuration / fromIntegral numTracks

findAlbumWithLongestAverageDuration :: [Album] -> Maybe Album
findAlbumWithLongestAverageDuration albums =
  case albums of
    [] -> Nothing
    _ ->
      let maxAlbum = maximumBy (comparing averageDurationHelper) albums
      in Just maxAlbum



findAlbumWithShortestTrack :: [Album] -> Maybe Album
findAlbumWithShortestTrack [] = Nothing
findAlbumWithShortestTrack albums =
  Just $ minimumBy (comparing (minimum . map _duration . _albumTracks)) albums



findAlbumsByYear :: Int -> [Album] -> [Album]
findAlbumsByYear targetYear = filter (\album -> maybe False (== targetYear) (parseYear (_albumReleaseDate album)))
