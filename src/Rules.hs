module Rules
  ( Rules  (..)
  , Expr
  , Action (..)
  , Type   (..)
  , Const  (..)
  , State  (..)
  ) where

import Common

-- Rules IR.
data Rules = Rules
  { state :: [(Path, State)]    -- ^ Global state elements.
  , named :: [(Path, Expr)]     -- ^ Named expressions of state.
  , rules :: [(Path, Action)]   -- ^ Guarded atomic actions.
  }

data Expr

data Action
  = Actions     Action Action        -- ^ Collecting actions together.
  | Guard       Expr                 -- ^ Guard condition on rule.
  | If          Expr Action Action   -- ^ Conditional.
  | RegUpdate   Path Expr            -- ^ Regisgter update.
  | FIFOPush    Path Expr            -- ^ Writing data to a FIFO.
  | FIFOPull    Path                 -- ^ Reading FIFO output is combinational.
  | BRAMWrite   Path Expr Expr       -- ^ Address and data.
  | BRAMReadReq Path Expr
  | BRAMReadRsp Path

data State
  = Reg (Either Type Const)  -- ^ Either uninitialized or initialized.
  | FIFO Type (Maybe Int)    -- ^ Optional specified depth.
  | BRAM Type Int            -- ^ Depth.  Address width determined from depth.

data Type = Integer | Double | Bits Int
data Const = IntegerC Integer | DoubleC Double | BitsC Int Integer

