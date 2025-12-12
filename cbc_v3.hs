{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE ConstraintKinds #-}

module Main where

import GHC.TypeLits
import Data.Kind (Type, Constraint)
import Prelude hiding ((>>=), (>>), return, fail)
import qualified Prelude as P

-- ==========================================
-- 1. TYPE LEVEL DATA (The "Vocabulary")
-- ==========================================

-- Promoted types for tracking state
data BuildPhase = InitPhase | NotFramePhase | CorePhase | OptionalPhase | FinalizedPhase
data ReactorType = Fusion | Antimatter
data ShieldType = Magnetic | Phase

-- ==========================================
-- 2. THE SHIP STATE (Type-Level Tracker)
-- ==========================================

-- This data structure exists ONLY at the type level to track rules.
-- p: Phase
-- s: Current Slots Used (Natural Number)
-- r, e, l, b: Booleans (Has Reactor, Engine, LifeSupport, Bridge?)
-- rList: List of installed reactor types (for dependency checking)
data ShipState (p :: BuildPhase) (s :: Nat) (r :: Bool) (e :: Bool) (l :: Bool) (b :: Bool) (rList :: [ReactorType])

-- Runtime Data (to actually store the values for printing)
data Spaceship = Spaceship {
    sName :: String,
    sUsedSlots :: Int,
    sComponents :: [String] -- Simplified for visualization
} deriving Show

-- ==========================================
-- 3. INDEXED MONAD (The "Builder")
-- ==========================================

-- i: Input State (Type), o: Output State (Type), a: Return Value
newtype ShipBuilder i o a = ShipBuilder { runBuilder :: Spaceship -> (a, Spaceship) }

-- Indexed Bind (>>=)
(>>=) :: ShipBuilder i m a -> (a -> ShipBuilder m o b) -> ShipBuilder i o b
(ShipBuilder f) >>= g = ShipBuilder $ \s ->
    let (a, s') = f s
        (ShipBuilder h) = g a
    in h s'

-- Indexed Return
return :: a -> ShipBuilder i i a
return x = ShipBuilder $ \s -> (x, s)

-- Indexed Then (>>)
(>>) :: ShipBuilder i m a -> ShipBuilder m o b -> ShipBuilder i o b
m >> k = m >>= \_ -> k

-- ==========================================
-- 4. TYPE FAMILIES (Compile-Time Logic)
-- ==========================================

-- [B-307] Slot Checker
type family CheckSlots (current :: Nat) (cost :: Nat) :: Constraint where
    CheckSlots c cost = (c + cost <=? 10) ~ 'True

-- [B-440] Incompatibility Logic
type family Elem (x :: ReactorType) (list :: [ReactorType]) :: Bool where
    Elem x '[] = 'False
    Elem x (x ': xs) = 'True
    Elem x (y ': xs) = Elem x xs

-- Generates a Compile Error if rules are violated
type family CheckShieldCompat (s :: ShieldType) (rList :: [ReactorType]) :: Constraint where
    CheckShieldCompat 'Phase rList = 
        (Elem 'Fusion rList ~ 'False) 
    CheckShieldCompat 'Magnetic rList = 
        (Elem 'Antimatter rList ~ 'False) 

-- Custom Error Messages for cleaner compiler output
type family (a :: Bool) ~ (b :: Bool) :: Constraint where
    'False ~ 'True = TypeError ('Text "Safety Violation: Check Failed")
    'True ~ 'True = ()
    
-- ==========================================
-- 5. SINGLETONS (Bridging Values to Types)
-- ==========================================
-- We need these GADTs to pass types like 'Fusion' as arguments

data SReactor (t :: ReactorType) where
    SFusion :: SReactor 'Fusion
    SAntimatter :: SReactor 'Antimatter

data SShield (t :: ShieldType) where
    SMagnetic :: SShield 'Magnetic
    SPhase :: SShield 'Phase

-- ==========================================
-- 6. DSL OPERATIONS (The API)
-- ==========================================

-- [A-103] Start Blueprint: Initializes state
start_blueprint :: String 
                -> ShipBuilder (ShipState 'InitPhase 0 'False 'False 'False 'False '[]) 
                               (ShipState 'NotFramePhase 0 'False 'False 'False 'False '[]) ()
start_blueprint name = ShipBuilder $ \_ -> ((), Spaceship name 0 [])

-- Set Frame
set_frame :: ShipBuilder (ShipState 'NotFramePhase 0 r e l b list) 
                         (ShipState 'CorePhase 0 r e l b list) ()
set_frame = ShipBuilder $ \s -> ((), s)

-- [A-305] Add Reactor (Only in CorePhase)
-- [B-307] Checks Slots
-- [B-209] Updates 'HasReactor' to True
-- [B-440] Adds type to list
add_reactor :: (CheckSlots slots 3) -- Compile-time slot check
            => SReactor rt 
            -> ShipBuilder (ShipState 'CorePhase slots r e l b rList)
                           (ShipState 'CorePhase (slots + 3) 'True e l b (rt ': rList)) ()
add_reactor rType = ShipBuilder $ \s -> 
    let rName = case rType of SFusion -> "Reactor (Fusion)"; SAntimatter -> "Reactor (Antimatter)"
    in ((), s { sUsedSlots = sUsedSlots s + 3, sComponents = rName : sComponents s })

-- Add Engine
add_engine :: (CheckSlots slots 2) 
           => ShipBuilder (ShipState 'CorePhase slots r e l b list)
                          (ShipState 'CorePhase (slots + 2) r 'True l b list) ()
add_engine = ShipBuilder $ \s -> ((), s { sUsedSlots = sUsedSlots s + 2, sComponents = "Engine" : sComponents s })

-- Add Life Support
add_life_support :: (CheckSlots slots 2) 
                 => ShipBuilder (ShipState 'CorePhase slots r e l b list)
                                (ShipState 'CorePhase (slots + 2) r e 'True b list) ()
add_life_support = ShipBuilder $ \s -> ((), s { sUsedSlots = sUsedSlots s + 2, sComponents = "LifeSupport" : sComponents s })

-- Add Bridge
add_bridge :: (CheckSlots slots 1) 
           => ShipBuilder (ShipState 'CorePhase slots r e l b list)
                          (ShipState 'CorePhase (slots + 1) r e l 'True list) ()
add_bridge = ShipBuilder $ \s -> ((), s { sUsedSlots = sUsedSlots s + 1, sComponents = "Bridge" : sComponents s })

-- [B-209] Lock Core Systems
-- Ensures all Core flags are 'True before transitioning phase
lock_core_systems :: ShipBuilder (ShipState 'CorePhase s 'True 'True 'True 'True list)
                                 (ShipState 'OptionalPhase s 'True 'True 'True 'True list) ()
lock_core_systems = ShipBuilder $ \s -> ((), s)

-- [B-440] Add Shield
-- Checks compatibility with rList at compile time
add_shield :: (CheckSlots slots 1, CheckShieldCompat st rList)
           => SShield st
           -> ShipBuilder (ShipState 'OptionalPhase slots r e l b rList)
                          (ShipState 'OptionalPhase (slots + 1) r e l b rList) ()
add_shield sType = ShipBuilder $ \s -> 
    let sName = case sType of SMagnetic -> "Shield (Magnetic)"; SPhase -> "Shield (Phase)"
    in ((), s { sUsedSlots = sUsedSlots s + 1, sComponents = sName : sComponents s })

-- [A-212] Finalize
finalize_blueprint :: ShipBuilder (ShipState 'OptionalPhase s r e l b list)
                                  (ShipState 'FinalizedPhase s r e l b list) Spaceship
finalize_blueprint = ShipBuilder $ \s -> (s, s)

-- ==========================================
-- 7. USAGE EXAMPLES
-- ==========================================

-- Helper to run the builder
buildShip :: ShipBuilder (ShipState 'InitPhase 0 'False 'False 'False 'False '[]) 
                         out a 
          -> a
buildShip builder = fst $ runBuilder builder (Spaceship "" 0 [])

-- SCENARIO 1: VALID SHIP
-- This compiles successfully.
validDesign :: Spaceship
validDesign = buildShip $ do
    start_blueprint "CBC Enterprise"
    set_frame
    
    -- Install Core
    add_reactor SFusion
    add_engine
    add_life_support
    add_bridge
    
    -- Lock
    lock_core_systems
    
    -- Install Optional
    -- Fusion + Magnetic is Allowed
    add_shield SMagnetic 
    
    finalize_blueprint

-- SCENARIO 2: INVALID (Uncomment lines to see compile errors)
invalidDesign :: Spaceship
invalidDesign = buildShip $ do
    start_blueprint "Broken Ship"
    set_frame
    
    -- ERROR 1: Slot Limit Exceeded
    -- If you add too many reactors, the compiler calculates (3+3+3+3 > 10) and fails.
    add_reactor SFusion
    add_reactor SFusion
    add_reactor SFusion 
    -- add_reactor SFusion -- <--- UNCOMMENT THIS LINE: "Couldn't match type 'False with 'True" (Slot check)

    add_engine
    add_life_support
    add_bridge
    lock_core_systems
    
    -- ERROR 2: Incompatible Shield
    -- We installed Fusion above. Installing Phase shield is illegal.
    -- add_shield SPhase -- <--- UNCOMMENT THIS LINE: "Couldn't match type 'False with 'True" (Dependency check)
    
    finalize_blueprint

-- Main just prints the result
main :: IO ()
main = do
    putStrLn "Compiling CBC Ship..."
    print validDesign
    putStrLn "Done."