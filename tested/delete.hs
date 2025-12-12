{- HLINT ignore "Use camelCase" -}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE FlexibleInstances #-}

module CBCAdvanced_Fixed where

import GHC.TypeLits
import Data.Kind (Constraint)
import Data.Type.Bool (If)
import Prelude hiding ((>>=), (>>), return, fail)
import qualified Prelude as P

-- ==========================================
-- 1. VALUE LEVEL (RUNTIME DATA)
-- ==========================================

data Component
  = CFrame
  | CReactor ReactorType
  | CEngine
  | CLifeSupport
  | CBridge
  | CShield ShieldType
  | CSensor
  deriving (P.Show)

data ReactorType = Fusion | Antimatter deriving (P.Show, P.Eq)
data ShieldType  = Magnetic | Phase    deriving (P.Show, P.Eq)

data ShipData = ShipData {
    sName :: P.String,
    sComps :: [Component],
    sMass :: P.Int,
    sPowerGen :: P.Int,
    sPowerDraw :: P.Int,
    sThrust :: P.Int
} deriving (P.Show)

-- ==========================================
-- 2. TYPE LEVEL (COMPILE-TIME LOGIC)
-- ==========================================

data BuildPhase = Init | Core | Optional | Finalized -- not-cbc code has 5 phases, why 4 phases here? Need to explain it. 

-- [FIX QUAN TRỌNG]: Đây là Type Constructor còn thiếu ở code trước.
-- Nó đóng vai trò là "Container" chứa toàn bộ trạng thái của Type System.
data ShipState (p :: BuildPhase) 
               (rMap :: (P.Bool, P.Bool)) -- Fusion, Antimatter
               (cores :: (P.Bool, P.Bool, P.Bool, P.Bool)) -- Reactor, Engine, LifeSupport, Bridge
               (slots :: Nat) -- Nat is Natural Number from GHC.TypeLits

-- Các hàm logic (Type Families)
type family CheckCompatibility (reactors :: (P.Bool, P.Bool)) (s :: ShieldType) :: Constraint where
    CheckCompatibility '( 'True, _ ) 'Phase    = TypeError ('Text "ERROR: Fusion Reactor conflicts with Phase Shield! [Rule B-440]")
    CheckCompatibility '( _, 'True ) 'Magnetic = TypeError ('Text "ERROR: Antimatter Reactor conflicts with Magnetic Shield! [Rule B-440]")
    CheckCompatibility _ _                     = ()

type family UpdateReactors (new :: ReactorType) (current :: (P.Bool, P.Bool)) :: (P.Bool, P.Bool) where
    UpdateReactors 'Fusion     '( _, a ) = '( 'True, a )
    UpdateReactors 'Antimatter '( f, _ ) = '( f, 'True )

type family CheckSlots (current :: Nat) (cost :: Nat) :: Constraint where
    CheckSlots current cost =
        If ((current + cost) <=? 10)
           (() :: Constraint)
           (TypeError ('Text "ERROR: Slot limit exceeded (Max 10)! [Rule B-307]"))

type family CheckCoreIntegrity (cores :: (P.Bool, P.Bool, P.Bool, P.Bool)) :: Constraint where
    CheckCoreIntegrity '( 'True, 'True, 'True, 'True) = ()
    CheckCoreIntegrity _ = TypeError ('Text "ERROR: Missing Core Systems (Reactor, Engine, LifeSupport, Bridge)! [Rule B-209]")

-- ==========================================
-- 3. INDEXED MONAD
-- ==========================================

newtype ShipBuilder i o a = ShipBuilder { runBuilder :: ShipData -> (a, ShipData) }

(>>=) :: ShipBuilder i j a -> (a -> ShipBuilder j k b) -> ShipBuilder i k b
(ShipBuilder f) >>= g = ShipBuilder $ \d ->
    let (a, d') = f d
        (ShipBuilder h) = g a
    in h d'

(>>) :: ShipBuilder i j a -> ShipBuilder j k b -> ShipBuilder i k b
m >> k = m >>= \_ -> k

return :: a -> ShipBuilder i i a
return x = ShipBuilder $ \d -> (x, d)

-- ==========================================
-- 4. DSL OPERATIONS (Đã cập nhật Type Signature)
-- ==========================================

data SReactor (t :: ReactorType) where
    SFusion     :: SReactor 'Fusion
    SAntimatter :: SReactor 'Antimatter

data SShield (t :: ShieldType) where
    SMagnetic :: SShield 'Magnetic
    SPhase    :: SShield 'Phase

-- Type Alias cho trạng thái rỗng ban đầu để code gọn hơn
type EmptyState = ShipState 'Init '( 'False, 'False) '( 'False, 'False, 'False, 'False) 0

-- [A-103] Start Blueprint
startBlueprint :: P.String
               -> ShipBuilder EmptyState EmptyState ()
startBlueprint name = ShipBuilder $ \_ ->
    ((), ShipData name [] 0 0 0 0)

-- Set Frame: Chuyển từ Init -> Core
setFrame :: ShipBuilder EmptyState 
                        (ShipState 'Core '( 'False, 'False) '( 'False, 'False, 'False, 'False) 0) ()
setFrame = ShipBuilder $ \d ->
    ((), d { sMass = 1000, sComps = [CFrame] })

-- Add Reactor
addReactor :: (CheckSlots s 3)
           => SReactor rt
           -> ShipBuilder (ShipState 'Core rMap '(hasR, hasE, hasL, hasB) s)
                          (ShipState 'Core (UpdateReactors rt rMap) '( 'True, hasE, hasL, hasB) (s + 3)) ()
addReactor rt = ShipBuilder $ \d ->
    let (m, p, nameVal) = case rt of
            SFusion     -> (300, 1000, CReactor Fusion)
            SAntimatter -> (450, 1000, CReactor Antimatter)
    in ((), d { sComps = nameVal : sComps d
              , sMass = sMass d + m
              , sPowerGen = sPowerGen d + p })

-- Add Engine
addEngine :: (CheckSlots s 2)
          => ShipBuilder (ShipState 'Core rMap '(hasR, hasE, hasL, hasB) s)
                         (ShipState 'Core rMap '(hasR, 'True, hasL, hasB) (s + 2)) ()
addEngine = ShipBuilder $ \d ->
    ((), d { sComps = CEngine : sComps d
           , sMass = sMass d + 100
           , sPowerDraw = sPowerDraw d + 250
           , sThrust = sThrust d + 500 })

-- Add Life Support
addLifeSupport :: (CheckSlots s 2)
               => ShipBuilder (ShipState 'Core rMap '(hasR, hasE, hasL, hasB) s)
                              (ShipState 'Core rMap '(hasR, hasE, 'True, hasB) (s + 2)) ()
addLifeSupport = ShipBuilder $ \d ->
    ((), d { sComps = CLifeSupport : sComps d, sMass = sMass d + 80, sPowerDraw = sPowerDraw d + 50 })

-- Add Bridge
addBridge :: (CheckSlots s 1)
          => ShipBuilder (ShipState 'Core rMap '(hasR, hasE, hasL, hasB) s)
                         (ShipState 'Core rMap '(hasR, hasE, hasL, 'True) (s + 1)) ()
addBridge = ShipBuilder $ \d ->
    ((), d { sComps = CBridge : sComps d, sMass = sMass d + 50, sPowerDraw = sPowerDraw d + 75 })

-- Lock Core Systems: Chuyển từ Core -> Optional
lockCoreSystems :: (CheckCoreIntegrity cores)
                => ShipBuilder (ShipState 'Core rMap cores s)
                               (ShipState 'Optional rMap cores s) ()
lockCoreSystems = ShipBuilder $ \d -> ((), d)

-- Add Shield
addShield :: (CheckSlots s 1, CheckCompatibility rMap st)
          => SShield st
          -> ShipBuilder (ShipState 'Optional rMap cores s)
                         (ShipState 'Optional rMap cores (s + 1)) ()
addShield st = ShipBuilder $ \d ->
    let val = case st of SMagnetic -> CShield Magnetic; SPhase -> CShield Phase
    in ((), d { sComps = val : sComps d, sMass = sMass d + 40, sPowerDraw = sPowerDraw d + 100 })

-- Finalize: Chuyển từ Optional -> Finalized
finalize :: ShipBuilder (ShipState 'Optional rMap cores s)
                        (ShipState 'Finalized rMap cores s) ShipData
finalize = ShipBuilder $ \d -> (d, d)

-- ==========================================
-- 5. UTILS & MAIN
-- ==========================================

-- ==========================================
-- 5. UTILS & MAIN (Fixed for RebindableSyntax)
-- ==========================================

-- Lưu ý: Vì RebindableSyntax đang bật, ta không dùng 'do' block cho IO được
-- Ta phải dùng (P.>>) để nối các lệnh IO.

printSpec :: ShipData -> P.IO ()
printSpec ship =
    P.putStrLn ("\n=== SPACESHIP SPECIFICATION: " P.++ sName ship P.++ " ===") P.>>
    P.putStrLn ("Mass:        " P.++ P.show (sMass ship)) P.>>
    P.putStrLn ("Power Gen:   " P.++ P.show (sPowerGen ship)) P.>>
    P.putStrLn ("Power Draw:  " P.++ P.show (sPowerDraw ship)) P.>>
    (let balance = sPowerGen ship P.- sPowerDraw ship
     in P.putStrLn ("Net Power:   " P.++ P.show balance)) P.>>
    P.putStrLn ("Thrust/Wt:   " P.++ P.show (P.fromIntegral (sThrust ship) P./ P.fromIntegral (sMass ship))) P.>>
    P.putStrLn "Components:" P.>>
    P.mapM_ (\c -> P.putStrLn (" - " P.++ P.show c)) (P.reverse (sComps ship)) P.>>
    P.putStrLn "========================================\n"

-- Helper function
build :: ShipBuilder EmptyState out a -> a
build b = P.fst $ runBuilder b (ShipData "" [] 0 0 0 0)

-- SCENARIO 1: Valid Ship (Vẫn dùng 'do' vì đây là ShipBuilder DSL)
validShip :: ShipData
validShip = build $ do
    startBlueprint "HMS Haskell Fixed"
    setFrame
    addReactor SFusion
    addEngine
    addLifeSupport
    addBridge
    lockCoreSystems
    addShield SMagnetic
    finalize

-- SCENARIO 2: Invalid Ship

invalidShip :: ShipData
invalidShip = build $ do
    startBlueprint "Titanic Error"
    setFrame
    addReactor SFusion
    addEngine
    addLifeSupport
    addBridge
    lockCoreSystems
    -- addShield SPhase  -- Lỗi ở đây
    finalize


main :: P.IO ()
main =
    P.putStrLn "Compiling Correct-By-Construction Spaceship..." P.>>
    printSpec validShip