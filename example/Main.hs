module Main where

import           Data.Matrix
import qualified Data.Text.Lazy.IO   as T
import           Data.Word
import           Lazyboy
import           Lazyboy.IO.Graphics
import           Lazyboy.Prelude
import           Lazyboy.Target.ASM
import           Prelude             hiding ((&&), (/=), (<), (==), (>), (||))

-- [TODO] it seems like the registers get overwritten and break the control flow in this program
-- to fix that, let's make use of the stack to store state before we make any changes
-- see here: http://gameboy.mongenel.com/dmg/lesson1.html (search: STACK)
main :: IO ()
main = rom >>= T.putStrLn
    where rom = compileROM $ do
            let tilemap = matrix 31 31 $ \_ -> 0
            let sprite = [0x00,0x00,0x00,0x00,0x24,0x24,0x00,0x00,0x81,0x81,0x7e,0x7e,0x00,0x00,0x00,0x00]
            sprite' <- embedBytes sprite
            setScroll (0, 0)
            setBackgroundPalette defaultPalette
            onVblank $ do
                disableLCD
                memcpy (Name sprite') (Address $ 0x9000) $ fromIntegral $ length sprite
                updateTilemap tilemap
                setLCDControl $ defaultLCDControl { lcdDisplayEnable = True, lcdBackgroundEnable = True }
            freeze
