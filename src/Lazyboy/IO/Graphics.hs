{-|
    Module      : Lazyboy.IO.Graphics
    Description : Graphics manipulation library for Lazyboy
    Copyright   : (c) Rose 2019
    License     : BSD3
    Maintainer  : rose@lain.org.uk
    Stability   : experimental
    Portability : POSIX

    This module defines a low-level library for graphics manipulation for Lazyboy.
-}

module Lazyboy.IO.Graphics where

import           Control.Exception
import           Data.Matrix
import           Data.Word
import qualified Lazyboy.Constants as GB
import           Lazyboy.IO
import           Lazyboy.Types
import           Text.Printf
import           Type.Reflection   (Typeable)

-- | Type alias that represents a tile map as a Matrix.
type Tilemap = Matrix Integer

-- | An exception type for use in this module.
data GraphicsException a = ExceedsBounds a (a, a)

-- | An instance of Show for formatting exception messages.
instance (Show a) => Show (GraphicsException a) where
    show (ExceedsBounds value bounds) = mconcat ["A value of '", show value, "' was provided to a function accepting a value in the range of (", show $ fst bounds, ", ", show $ snd bounds, ")!"]

-- | An instance of Exception for the exceptional type.
instance (Show a, Typeable a) => Exception (GraphicsException a) where

-- [TODO] make this use a loop
-- | Update the whole background tilemap.
updateTilemap :: Tilemap -> Lazyboy ()
updateTilemap tiles = do
    mapM_ (\(ix, tile) -> setTileAtIndex ix tile) addressed
    where addressed = zip [0..] flat
          flat = toList tiles

-- | Write a Tile ID to the nth background tile.
setTileAtIndex :: Integer -> Integer -> Lazyboy ()
setTileAtIndex offset tile
    | tile   < minTile  ||   tile > maxTile  = throw $ ExceedsBounds tile (minTile, maxTile)
    | offset < minIndex || offset > maxIndex = throw $ ExceedsBounds offset (minIndex, maxIndex)
    | otherwise = write (Address $ GB.background + fromIntegral offset) $ fromIntegral tile
    where (minIndex, maxIndex) = (0, 1023)
          (minTile, maxTile)   = (0, 191)

-- Write a Tile ID to the background tile at some given coordinates.
setTile :: Integer -> Integer -> Integer -> Lazyboy ()
setTile x y tile
    | tile < minTile || tile > maxTile   = throw $ ExceedsBounds tile (minTile, maxTile)
    | x < minXY || x > maxXY = throw $ ExceedsBounds x (minXY, maxXY)
    | y < minXY || y > maxXY = throw $ ExceedsBounds y (minXY, maxXY)
    | otherwise = write address $ fromIntegral tile
    where address = Address $ GB.background + (32 * fromIntegral x + fromIntegral y)
          (minXY, maxXY) = (0, 31)
          (minTile, maxTile) = (0, 191)
