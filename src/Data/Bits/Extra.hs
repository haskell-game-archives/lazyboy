{-|
    Module      : Data.Bits.Extra
    Description : Additional bitwise functions used by Lazyboy
    Copyright   : (c) Rose 2019
    License     : BSD3
    Maintainer  : rose@lain.org.uk
    Stability   : experimental
    Portability : POSIX

    This module extends Data.Bits with additional functionality.
-}

module Data.Bits.Extra where

import           Data.Bits
import           Data.Word

-- | Split a 16 bit integer into its two 8 bit component parts.
split :: Word16 -> (Word8, Word8)
split n = (fromIntegral $ n `shiftR` 8, fromIntegral $ n .&. 0x00FF)
