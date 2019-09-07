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
import           Control.Monad.Trans.RWS.Lazy
import           Data.Bits
import           Data.Bits.Extra
import           Data.Matrix
import           Data.Word
import qualified Lazyboy.Constants            as GB
import           Lazyboy.Control
import           Lazyboy.IO
import           Lazyboy.Prelude              ((/=), (==))
import           Lazyboy.Types
import           Prelude                      hiding (and, or, (/=), (==))
import           Text.Printf
import           Type.Reflection              (Typeable)

-- | Type alias that represents a tile map as a Matrix.
type Tilemap = Matrix Word8 

-- | An exception type for use in this module.
data GraphicsException a = ExceedsBounds a (a, a)

-- | An instance of Show for formatting exception messages.
instance Show a => Show (GraphicsException a) where
    show (ExceedsBounds value bounds) = mconcat ["A value of '", show value, "' was provided to a function accepting a value in the range of (", show $ fst bounds, ", ", show $ snd bounds, ")!"]

-- | An instance of Exception for the exceptional type.
instance (Show a, Typeable a) => Exception (GraphicsException a) where

-- | Update the whole background tilemap.
updateTilemap :: Tilemap -> Lazyboy ()
updateTilemap tiles = protecting [HL, DE, BC] $ do
    content <- getLocalLabel
    -- embed the tile data in the ROM
    content <- embedBytes bytes
    -- zero BC out for use as a counter
    tell [LDrn B 0, LDrn C 0]
    -- load the start of the background region into HL
    tell [LDrrnn HL $ Address GB.background]
    -- point DE to the start of the tile data
    tell [LDrrnn DE $ Name content]

    while ((B /= high) `or` (C /= low)) $ do
        tell [LDArr DE]
        tell [LDHLAI]
        inc DE
        inc BC

    where bytes = toList tiles
          (high, low) = split count
          count = fromIntegral $ length tiles - 1

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

-- | Set the X scroll co-ordinate.
setScrollX :: Word8 -> Lazyboy ()
setScrollX = write (Address GB.scx) 

-- | Set the Y scroll co-ordinate.
setScrollY :: Word8 -> Lazyboy ()
setScrollY = write (Address GB.scy)

-- | Set the X and Y scroll co-ordinates.
setScroll :: (Word8, Word8) -> Lazyboy ()
setScroll (x, y) = do
    setScrollX x
    setScrollY y