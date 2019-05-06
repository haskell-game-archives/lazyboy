{-|
    Module      : Main (Test)
    Description : Test suite for Lazyboy
    Copyright   : (c) Rose 2019
    License     : BSD3
    Maintainer  : rose@lain.org.uk
    Stability   : experimental
    Portability : POSIX

    A test suite for Lazyboy.
-}

import           Control.Exception  (evaluate)
import           Lazyboy
import           Lazyboy.Target.ASM
import           Test.Hspec

disallow cmd = evaluate cmd `shouldThrow` anyException

main :: IO ()
main = hspec $ do
    describe "Lazyboy.IO" $ do
        context "when asked to pack an LCDState" $ do
            it "packs an LCDState of all False values to 0" $ do
                pack defaultLCDState `shouldBe` 0
            it "packs an LCDState with background and display enabled to 129" $ do
                pack (LCDState True False False False False False False True) `shouldBe` 129
            it "packs other LCDStates correctly" $ do
                pack (defaultLCDState { lcdEnableObjects = True }) `shouldBe` 2
        context "when asked to pack a BackgroundPalette" $ do
            it "packs a BackgroundPalette of all-black to 255" $ do
                pack (BackgroundPalette Black Black Black Black) `shouldBe` 255
            it "packs a BackgroundPalette of all-white to 0" $ do
                pack (BackgroundPalette White White White White) `shouldBe` 0
            it "correctly packs other BackgroundPalettes" $ do
                pack (BackgroundPalette White Light White White) `shouldBe` 16

    describe "Lazyboy.Types.execLazyboy" $ do
        it "compiles nested sequences in order" $ do
            let sequence = execLazyboy $ do
                    write 0x2000 0x97
                    write 0x1000 0x98
            sequence `shouldBe` [LDrrnn HL 0x2000, LDHLn 0x97, LDrrnn HL 0x1000, LDHLn 0x98]

    describe "Lazyboy.Control" $ do
        describe "cond" $ do
            it "correctly implements conditionals" $ do
                let program = execLazyboy $ do
                        cond NonZero $ do
                            freeze
                program `shouldBe` [JUMPif NonZero $ Local 1, LABEL $ Local 2, JUMP $ Local 2, LABEL $ Local 1]
            it "handles nested conditionals correctly" $ do
                let program = execLazyboy $ do
                        cond Zero $ do
                            cond NonZero $ do
                                freeze
                program `shouldBe` [ JUMPif Zero $ Local 1
                                , JUMPif NonZero $ Local 2
                                , LABEL $ Local 3
                                , JUMP $ Local 3
                                , LABEL $ Local 2
                                , LABEL $ Local 1 
                                ]
        describe "withLabel" $ do
            it "creates an appropriately formatted global label" $ do
                let program = map show $ execLazyboy $ do
                        withLabel $ \label -> do
                            write 0xC000 0x97
                program `shouldBe` [ "L1:"
                                   , "ld HL, $C000"
                                   , "ld [HL], 151"
                                   ]
        describe "withLocalLabel" $ do
            it "creates an appropriately formatted local label" $ do
                let program = map show $ execLazyboy $ do
                        withLocalLabel $ \label -> do
                            write 0xC000 0x97
                program `shouldBe` [ ".L1:"
                                   , "ld HL, $C000"
                                   , "ld [HL], 151"
                                   ]

    describe "Prelude.show" $ do
        it "disallows loading [AF] into A" $ do
            disallow (show $ LDArr AF)
        it "disallows loading [SP] into A" $ do
            disallow (show $ LDArr SP)
        it "disallows loading [PC] into A" $ do
            disallow (show $ LDArr PC)
        it "disallows loading A into [AF]" $ do
            disallow (show $ LDrrA AF)
        it "disallows loading A into [SP]" $ do
            disallow (show $ LDrrA SP)
        it "disallows loading A into [PC]" $ do
            disallow (show $ LDrrA PC)
        it "disallows loading a 16 bit value into AF" $ do
            disallow (show $ LDrrnn AF 0x00)
        it "disallows loading a 16 bit value into PC" $ do
            disallow (show $ LDrrnn PC 0x00)
        it "disallows pushing stack pointer" $ do
            disallow (show $ PUSH SP)
        it "disallows pushing program counter" $ do
            disallow (show $ PUSH PC)
        it "disallows popping stack pointer" $ do
            disallow (show $ POP SP)
        it "disallows popping program counter" $ do
            disallow (show $ POP PC)
        it "disallows an invalid RST vector value" $ do
            disallow (show $ RST 0x02)
        it "disallows adding AF to HL" $ do
            disallow (show $ ADDHLrr AF)
        it "disallows adding PC to HL" $ do
            disallow (show $ ADDHLrr PC)
        it "disallows incrementing AF" $ do
            disallow (show $ INCrr AF)
        it "disallows incrementing PC" $ do
            disallow (show $ INCrr PC)
        it "disallows decrementing AF" $ do
            disallow (show $ DECrr AF)
        it "disallows decrementing PC" $ do
            disallow (show $ DECrr PC)
        it "enforces only 3-bit values can be passed to BIT instructions" $ do
            disallow (show $ BITnr 0x80 A)
