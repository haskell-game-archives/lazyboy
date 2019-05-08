{-|
    Module      : Lazyboy.Types
    Description : Hardware type definitions for Lazyboy
    Copyright   : (c) Rose 2019
    License     : BSD3
    Maintainer  : rose@lain.org.uk
    Stability   : experimental
    Portability : POSIX

    This module defines datatypes for the various aspects of the target hardware
    including registers and instructions.
-}

module Lazyboy.Types where

import           Control.Monad                (replicateM)
import           Control.Monad.IO.Class       (liftIO)
import           Control.Monad.Trans.Class    (lift)
import           Control.Monad.Trans.RWS.Lazy
import           Data.Int
import           Data.Word

-- Rename and re-export RWS types and functions
type Lazyboy a = RWS () [Instruction] Integer a

execLazyboy :: Lazyboy a -> [Instruction]
execLazyboy m = snd $ execRWS m () 1

-- | An address or label
data Location = Address Word16 | Name Label
  deriving (Read, Show, Eq)

-- | Condition codes
data Condition = Zero | NonZero | Carry | NoCarry
  deriving (Read, Show, Eq)

-- | 8 bit registers
data Register8 = A | B | C | D | E | H | L
  deriving (Read, Show, Eq)

-- | 16 bit registers
data Register16 = BC | DE | HL | AF | SP | PC
  deriving (Read, Show, Eq)

data Label = Local Integer | Global Integer
  deriving (Read, Show, Eq)

-- | GB Opcodes and other special forms
data Instruction =
    LDrr Register8 Register8   -- load the value in one register8 into another
  | LDrn Register8 Word8       -- load the immediate value8 into a register8
  | LDrHL Register8            -- load the value8 stored at the address in HL into a register8
  | LDHLr Register8            -- load the value8 stored in a register8 into the address in HL
  | LDHLn Word8                -- load the immediate value8 into the address in HL
  | LDArr Register16           -- load the value at the address contained in a 16 bit register into A
  | LDrrA Register16           -- laod A into the address contained in a 16 bit register
  | LDAnn Location             -- load the value8 stored in the value16 address into A
  | LDnnA Location             -- load the value8 stored in A into the value16 address
  | LDAIO Word8                -- read into A from IO port n (FF00 + value8)
  | LDIOA Word8                -- store value8 in A into IO port n (FF00 + value8)
  | LDAIOC                     -- read from IO port FF00+c into A
  | LDIOCA                     -- store the value8 in A into IO port FF00+C
  | LDHLAI                     -- store value in register A into byte pointed by HL and post-increment HL
  | LDAHLI                     -- store value in address in HL in A and post-increment HL
  | LDHLAD                     -- store value in register A into byte pointed by HL and post-decrement HL.
  | LDAHLD                     -- store value in address in HL in A and post-decrement HL
  | LDrrnn Register16 Location -- load the value16 address into the register16
  | LDSPHL                     -- set the stack pointer to the value in HL
  | PUSH Register16            -- push register16 onto the stack
  | POP Register16             -- pop register16 from the stack

  -- Jump & Call instructions
  | JP Location               -- immediately jump to value16
  | JPHL                      -- immediately jump to the value contained in HL
  | JPif Condition Location   -- conditional jump to value16
  | CALL Location             -- call the address
  | CALLif Condition Location -- conditional call to address
  | RET                       -- return
  | RETif Condition           -- conditional return
  | RETi                      -- return and enable interrupts
  | RST Word8                 -- call a restart vector
 
  -- Arithmetic & Logical instructions
  | ADDAr Register8          -- add the value contained in a register to A
  | ADDAn Word8              -- add a value8 to the value contained in A
  | ADDHL                    -- add the value contained in the address stored in HL to A
  | ADCAr Register8          -- add the value in the register + the carry flag to A
  | ADCAn Word8              -- add the immediate value + the carry flag to A
  | ADCHL                    -- add the value contained in the address in HL + the carry flag to A
  | SUBAr Register8          -- subtract the value contained in a register from A
  | SUBAn Word8              -- subtract a value8 from A
  | SUBHL                    -- subtract from A the value contained in the address in HL
  | SBCAr Register8          -- subtract from A the value contained in the register + carry flag
  | SBCAn Word8              -- subtract from A the value + carry flag
  | SBCAHL                   -- subtract from A the value contained in the address in HL + carry flag
  | ANDr Register8           -- assign to A the value contained in a register & itself
  | ANDn Word8               -- assign to A a value8 & itself
  | ANDHL                    -- assign to A itself & the value in the address in HL
  | XORr Register8           -- assign to A the value contained in a register ^ itself
  | XORn Word8               -- assign to A a value8 ^ itself
  | XORHL                    -- assign to A itself ^ the value in the address in HL
  | ORr Register8            -- assign to A the value contained in a register | itself
  | ORn Word8                -- assign to A a value8 | itself
  | ORHL                     -- assign to A itself | the value in the address in HL
  | CPr Register8            -- substract from A the value in a register and set flags accordingly, don't store the result
  | CPn Word8                -- subtract from A a value8 and set flags accordingly, but don't store the result
  | CPHL                     -- subtract from A the value in the address in HL, set flags, don't store the result
  | INCr Register8           -- increment the value in a register
  | INCHL                    -- increment the value at the address in HL
  | DECr Register8           -- decrement the value in a register
  | DECHL                    -- decrement the value at the address in HL
  | DAA                      -- decimal-adjust register A
  | CPL                      -- complement accumulator (A = ~A)
  | ADDHLrr Register16       -- add the value contained in a 16 bit register to HL
  | INCrr Register16         -- increment the value in a 16 bit register
  | DECrr Register16         -- decrement the value in a 16 bit register
  | ADDSPn Int8              -- add the signed value8 to the stack pointer
  | LDHLSPn Int8             -- load into hl the stack pointer + a value8

  -- Single-bit instructions
  | BITnr Word8 Register8    -- test bit n in register8, set the zero flag if not set
  | BITnHL Word8             -- test bit n in the byte pointed by HL, set the zero flag if not set
  | SETnr Word8 Register8    -- set bit n in register8
  | SETnHL Word8             -- set bit n in the byte pointed by HL
  | RESnr Word8 Register8    -- unset bit n in register8
  | RESnHL Word8             -- unset bit n in the byte pointed by HL

  -- Rotate & shift instructions
  | RLCA -- rotate accumulator left
  | RLA  -- rotate accumulator left through carry
  | RRCA -- rotate accumulator right
  | RRA  -- rotate accumulator rit through carry
  | RLC Register8 -- rotate left
  | RLCHL -- rotate value contained at address in HL left
  | RL Register8 -- rotate left through carry
  | RLHL -- rotate value contained at address in HL left through carry
  | RRC Register8 -- rotate right
  | RRCHL -- rotate value contained at address in HL right
  | RR Register8 -- rotate right through carry
  | RRHL -- rotate value contained at address in HL right through carry
  | SLA Register8 -- shift left arithmetic
  | SLAHL -- shift left arithmetic (HL pointer)
  | SWAP Register8 -- exchange low and high nibbles
  | SWAPHL -- exchange low and high nibbles in HL pointer
  | SRA Register8 -- shift right arithmetic
  | SRAHL -- shift right arithmetic in HL pointer
  | SRL Register8 -- shift right logical
  | SRLHL -- shift right logical in HL pointer

  -- CPU control instructions
  | CCF                      -- complement carry flag
  | SCF                      -- set carry flag
  | NOP                      -- no operation
  | HALT                     -- halt until interrupt
  | STOP                     -- standby mode
  | DI                       -- disable interrupts
  | EI                       -- enable interrupts

  -- RGBASM-specific convenience stuff.
  -- these would need revamping if we were to start generating native machine code
  | LABEL Label               -- create a numbered label
  | INCLUDE FilePath          -- include a file
  | BYTES [Word8]             -- define some bytes with a global label

  deriving (Read, Eq)
