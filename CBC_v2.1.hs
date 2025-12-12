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

module CBCFinal_FullSpec where

import GHC.TypeLits
import Data.Kind (Constraint)
import Data.Type.Bool (If)
import Prelude hiding ((>>=), (>>), return, fail)
import qualified Prelude as P
import Text.Printf (printf)

ifThenElse :: P.Bool -> a -> a -> a
ifThenElse P.True  t _ = t
ifThenElse P.False _ f = f

-- ==========================================
-- 1. VOCABULARY & RUNTIME DATA
-- ==========================================

-- Type
data ReactorType     = Fusion | Antimatter       deriving (P.Show, P.Eq)
data EngineType      = Ion | Plasma              deriving (P.Show, P.Eq)
data LifeSupportType = Standard | Advanced       deriving (P.Show, P.Eq)
data BridgeType      = Explorer | Command        deriving (P.Show, P.Eq)
data ShieldType      = Magnetic | Phase          deriving (P.Show, P.Eq)
data SensorType      = Basic | Enhanced          deriving (P.Show, P.Eq)

-- Component
data Component
  = CFrame
  | CReactor ReactorType
  | CEngine EngineType
  | CLifeSupport LifeSupportType
  | CBridge BridgeType
  | CShield ShieldType
  | CSensor SensorType
  deriving (P.Show)

-- Spaceship structure
data ShipData = ShipData {
    sName :: P.String,
    sComps :: [Component],
    sMass :: P.Int,
    sPowerGen :: P.Int,
    sPowerDraw :: P.Int,
    sThrust :: P.Int
} deriving (P.Show)

-- Phase
data BuildPhase = Init | Core | Optional | Finalized

-- ShipState: Bảng theo dõi trạng thái biên dịch
data ShipState (p :: BuildPhase) -- phase
               (rMap :: (P.Bool, P.Bool)) -- Fusion, Antimatter. Support for [B-440]
               (cores :: (P.Bool, P.Bool, P.Bool, P.Bool)) -- Reactor, Engine, LifeSupport, Bridge
               (slots :: Nat)

-- Type Families: Logic check
type family CheckCompatibility (reactors :: (P.Bool, P.Bool)) (s :: ShieldType) :: Constraint where
    CheckCompatibility '( 'True, _ ) 'Phase    = TypeError ('Text "[B-440] Fusion Reactor conflicts with Phase Shield!")
    CheckCompatibility '( _, 'True ) 'Magnetic = TypeError ('Text "[B-440] Antimatter Reactor conflicts with Magnetic Shield!")
    CheckCompatibility _ _                     = ()

-- Update reactors
type family UpdateReactors (new :: ReactorType) (current :: (P.Bool, P.Bool)) :: (P.Bool, P.Bool) where
    UpdateReactors 'Fusion     '( _, a ) = '( 'True, a )
    UpdateReactors 'Antimatter '( f, _ ) = '( f, 'True )

-- Check slots
type family CheckSlots (current :: Nat) (cost :: Nat) :: Constraint where
    CheckSlots current cost =
        If ((current + cost) <=? 10)
           (() :: Constraint)
           (TypeError ('Text "[B-307] Slot limit exceeded (Max 10)!"))

type family CheckCoreIntegrity (cores :: (P.Bool, P.Bool, P.Bool, P.Bool)) :: Constraint where
    CheckCoreIntegrity '( 'True, 'True, 'True, 'True) = ()
    CheckCoreIntegrity _ = TypeError ('Text "[B-209] Missing Core Systems! Need Reactor, Engine, LifeSupport, Bridge.")

-- BUILDER MONAD

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

-- SINGLETONS (Cầu nối Value -> Type), dùng để truyền tham số kiểu (Ion, Plasma...) vào hàm

data SReactor (t :: ReactorType) where
    SFusion     :: SReactor 'Fusion
    SAntimatter :: SReactor 'Antimatter

data SEngine (t :: EngineType) where
    SIon    :: SEngine 'Ion
    SPlasma :: SEngine 'Plasma

data SLifeSupport (t :: LifeSupportType) where
    SStandard :: SLifeSupport 'Standard
    SAdvanced :: SLifeSupport 'Advanced

data SBridge (t :: BridgeType) where
    SExplorer :: SBridge 'Explorer
    SCommand  :: SBridge 'Command

data SShield (t :: ShieldType) where
    SMagnetic :: SShield 'Magnetic
    SPhase    :: SShield 'Phase

data SSensor (t :: SensorType) where
    SBasic    :: SSensor 'Basic
    SEnhanced :: SSensor 'Enhanced

-- DSL OPERATIONS

-- 
type EmptyState = ShipState 'Init '( 'False, 'False) '( 'False, 'False, 'False, 'False) 0

-- start_blueprint
startBlueprint :: P.String -> ShipBuilder EmptyState EmptyState ()
startBlueprint name = ShipBuilder $ \_ -> ((), ShipData name [] 0 0 0 0)

-- set_frame: Mass 1000, Slots 10
setFrame :: ShipBuilder EmptyState 
                        (ShipState 'Core '( 'False, 'False) '( 'False, 'False, 'False, 'False) 0) ()
setFrame = ShipBuilder $ \d ->
    ((), d { sMass = 1000, sComps = [CFrame] })

-- add_reactor: Fusion (300m, 1000p) | Antimatter (450m, 1000p) - Cost 3
addReactor :: (CheckSlots s 3)
           => SReactor rt -- parameter: Reactor Type: Fusion or Antimatter
           -> ShipBuilder (ShipState 'Core rMap '(hasR, hasE, hasL, hasB) s) -- rMap: list of current reactors
                          (ShipState 'Core (UpdateReactors rt rMap) '( 'True, hasE, hasL, hasB) (s + 3)) ()
addReactor rt = ShipBuilder $ \d ->
    let (m, p, comp) = case rt of
            SFusion     -> (300, 1000, CReactor Fusion)
            SAntimatter -> (450, 1000, CReactor Antimatter)
    in ((), d { sComps = comp : sComps d
              , sMass = sMass d + m
              , sPowerGen = sPowerGen d + p })

-- add_engine: Ion (100m, 500t) | Plasma (120m, 750t) - Draw 250 - Cost 2
addEngine :: (CheckSlots s 2)
          => SEngine et
          -> ShipBuilder (ShipState 'Core rMap '(hasR, hasE, hasL, hasB) s)
                         (ShipState 'Core rMap '(hasR, 'True, hasL, hasB) (s + 2)) ()
addEngine et = ShipBuilder $ \d ->
    let (m, t, comp) = case et of
            SIon    -> (100, 500, CEngine Ion)
            SPlasma -> (120, 750, CEngine Plasma)
    in ((), d { sComps = comp : sComps d
              , sMass = sMass d + m
              , sPowerDraw = sPowerDraw d + 250
              , sThrust = sThrust d + t })

-- add_life_support: Standard (80m) | Advanced (70m) - Draw 50 - Cost 2
addLifeSupport :: (CheckSlots s 2)
               => SLifeSupport lt
               -> ShipBuilder (ShipState 'Core rMap '(hasR, hasE, hasL, hasB) s)
                              (ShipState 'Core rMap '(hasR, hasE, 'True, hasB) (s + 2)) ()
addLifeSupport lt = ShipBuilder $ \d ->
    let (m, comp) = case lt of
            SStandard -> (80, CLifeSupport Standard)
            SAdvanced -> (70, CLifeSupport Advanced)
    in ((), d { sComps = comp : sComps d
              , sMass = sMass d + m
              , sPowerDraw = sPowerDraw d + 50 })

-- add_bridge: Explorer (50m) | Command (60m) - Draw 75 - Cost 1
addBridge :: (CheckSlots s 1)
          => SBridge bt
          -> ShipBuilder (ShipState 'Core rMap '(hasR, hasE, hasL, hasB) s)
                         (ShipState 'Core rMap '(hasR, hasE, hasL, 'True) (s + 1)) ()
addBridge bt = ShipBuilder $ \d ->
    let (m, comp) = case bt of
            SExplorer -> (50, CBridge Explorer)
            SCommand  -> (60, CBridge Command)
    in ((), d { sComps = comp : sComps d
              , sMass = sMass d + m
              , sPowerDraw = sPowerDraw d + 75 })

-- Lock core
lockCoreSystems :: (CheckCoreIntegrity cores)
                => ShipBuilder (ShipState 'Core rMap cores s)
                               (ShipState 'Optional rMap cores s) ()
lockCoreSystems = ShipBuilder $ \d -> ((), d)

-- add_shield: Mass 40 - Draw 100 - Cost 1
addShield :: (CheckSlots s 1, CheckCompatibility rMap st) -- CheckCompatibility for [B-440]
          => SShield st
          -> ShipBuilder (ShipState 'Optional rMap cores s)
                         (ShipState 'Optional rMap cores (s + 1)) ()
addShield st = ShipBuilder $ \d ->
    let comp = case st of SMagnetic -> CShield Magnetic; SPhase -> CShield Phase
    in ((), d { sComps = comp : sComps d
              , sMass = sMass d + 40
              , sPowerDraw = sPowerDraw d + 100 })

-- add_sensors: Basic (30m) | Advanced (35m) - Draw 50 - Cost 1
addSensors :: (CheckSlots s 1)
           => SSensor st
           -> ShipBuilder (ShipState 'Optional rMap cores s)
                          (ShipState 'Optional rMap cores (s + 1)) ()
addSensors st = ShipBuilder $ \d ->
    let (m, comp) = case st of
            SBasic    -> (30, CSensor Basic)
            SEnhanced -> (35, CSensor Enhanced)
    in ((), d { sComps = comp : sComps d
              , sMass = sMass d + m
              , sPowerDraw = sPowerDraw d + 50 })

-- Finalize
finalize :: ShipBuilder (ShipState 'Optional rMap cores s)
                        (ShipState 'Finalized rMap cores s) ShipData
finalize = ShipBuilder $ \d -> (d, d)


-- VISUALIZATION & MAIN

-- printSpec :: ShipData -> P.IO ()
-- printSpec ship =
--     P.putStrLn ("\n=== SPACESHIP BLUEPRINT: " P.++ sName ship P.++ " ===") P.>>
--     P.putStrLn ("Mass:         " P.++ P.show (sMass ship) P.++ " tons") P.>>
--     P.putStrLn ("Power Gen:    " P.++ P.show (sPowerGen ship) P.++ " MW") P.>>
--     P.putStrLn ("Power Draw:   " P.++ P.show (sPowerDraw ship) P.++ " MW") P.>>
--     (let balance = sPowerGen ship P.- sPowerDraw ship
--      in P.putStrLn ("Net Power:    " P.++ P.show balance P.++ " MW")) P.>>
--     P.putStrLn ("Total Thrust: " P.++ P.show (sThrust ship) P.++ " kN") P.>>
--     (let twr = P.fromIntegral (sThrust ship) P./ P.fromIntegral (sMass ship)
--      in P.putStrLn ("Thrust/Weight:" P.++ P.show twr)) P.>>
--     P.putStrLn "Installed Components:" P.>>
--     P.mapM_ (\c -> P.putStrLn (" - " P.++ P.show c)) (P.reverse (sComps ship)) P.>>
--     P.putStrLn "========================================\n"

-- Hàm tính chi phí slot cho từng component (Runtime)
getSlotCost :: Component -> P.Int
getSlotCost comp = case comp of
    CFrame          -> 0
    CReactor _      -> 3
    CEngine _       -> 2
    CLifeSupport _  -> 2
    CBridge _       -> 1
    CShield _       -> 1
    CSensor _       -> 1

printSpec :: ShipData -> P.IO ()
printSpec ship =
    -- 1. Tính toán các chỉ số hiển thị
    let uSlots     = P.sum (P.map getSlotCost (sComps ship))
        tSlots     = 10
        slotBar    = P.replicate uSlots '|' P.++ P.replicate (tSlots P.- uSlots) '.'
        -- Trong CBC, sMass đã được cộng dồn sẵn, không cần cộng lại frameMass
        totalMass  = sMass ship 
        totalThrust = sThrust ship
        balance    = sPowerGen ship P.- sPowerDraw ship
        twr        = P.fromIntegral totalThrust P./ P.fromIntegral totalMass :: P.Double
        
        balanceSign = if balance P.>= 0 then "+" else ""
        statusText  = if balance P.>= 0 then "[OK]" else "[OVERLOAD]"
    in
    
    -- 2. Chuỗi lệnh in ấn (Dùng P.>> để nối lệnh thay vì do-block)
    P.putStrLn "" P.>>
    P.putStrLn "[BUILDING SUCCESSFULLY]" P.>>
    P.putStrLn "" P.>>
    P.putStrLn "+--------------------------------------------------+" P.>>
    P.putStrLn "|               SPACESHIP BLUEPRINT                |" P.>>
    P.putStrLn "+--------------------------------------------------+" P.>>
    printf     "| NAME    : %-15s                        |\n" (sName ship) P.>>
    printf     "| STATUS  : %-15s                        |\n" "Finalized" P.>> -- CBC chạy xong nghĩa là Finalized
    P.putStrLn "+--------------------------------------------------+" P.>>

    P.putStrLn "| [ENERGY SYSTEM]                                  |" P.>>
    printf     "| Generation    : %5d MW                         |\n" (sPowerGen ship) P.>>
    printf     "| Consumption   : %5d MW                         |\n" (sPowerDraw ship) P.>>
    printf     "| BALANCE       : %s%d MW %-12s             |\n" balanceSign balance statusText P.>>

    P.putStrLn "|                                                  |" P.>>
    P.putStrLn "| [PERFORMANCE]                                    |" P.>>
    printf     "| Total Mass    : %5d tons                       |\n" totalMass P.>>
    printf     "| Total Thrust  : %5d kN                         |\n" totalThrust P.>>
    printf     "| TWR Ratio     : %5.2f                            |\n" twr P.>>

    P.putStrLn "|                                                  |" P.>>
    P.putStrLn "| [CAPACITY]                                       |" P.>>
    printf     "| Slots Used    : [%s] %2d/%d               |\n" slotBar uSlots tSlots P.>>

    P.putStrLn "+--------------------------------------------------+" P.>>
    P.putStrLn "| INSTALLED MODULES:                               |" P.>>
    (if P.null (sComps ship)
        then P.putStrLn "| (None)                                           |"
        else P.mapM_ (\c -> printf "| - %-47s|\n" (P.show c)) (P.reverse (sComps ship))) P.>>
    P.putStrLn "+--------------------------------------------------+\n"

build :: ShipBuilder EmptyState out a -> a
build b = P.fst $ runBuilder b (ShipData "" [] 0 0 0 0)

-- SCENARIO: Tàu chiến đấu hạng nặng (Plasma Engine + Command Bridge)
combatShip :: ShipData
combatShip = build $ do
    startBlueprint "Battle Cruiser CBC"
    setFrame
    
    -- Core Modules (Chi tiết từng loại)
    addReactor SFusion      -- 300 mass, 1000 pwr
    addEngine SPlasma       -- 120 mass, 750 thrust (Mạnh hơn Ion)
    addLifeSupport SStandard
    addBridge SCommand      -- 60 mass (Nặng hơn Explorer)
    
    lockCoreSystems
    
    -- Optional Modules
    addShield SMagnetic     -- Fusion + Magnetic = OK
    addSensors SEnhanced    -- Thêm sensors loại xịn
    
    finalize
    
-- A[103]
test_A103_Success :: ShipData
test_A103_Success = build $ do
    startBlueprint "A-103 OK"
    setFrame -- Đúng quy trình
    -- finalize -- (Giả sử đi tắt để test) -> Lỗi B-209 chặn lại, nhưng logic A-103 đã qua.
             -- (Để code biên dịch được thì ta phải đi hết quy trình, nhưng ở đây
             -- ta chỉ quan tâm dòng setFrame có lỗi hay không).
             -- Tạm thời để code biên dịch được, ta làm đúng quy trình tối thiểu:
    addReactor SFusion
    addEngine SIon
    addLifeSupport SStandard
    addBridge SExplorer
    lockCoreSystems
    finalize

-- test_A103_Fail :: ShipData
-- test_A103_Fail = build $ do
--     startBlueprint "A-103 Fail"
--     addReactor SFusion -- LỖI: Cố lắp Reactor ngay khi chưa setFrame
--     finalize

main :: P.IO ()
main =
    P.putStrLn "Compiling Full-Spec CBC Spaceship..." P.>>
    printSpec combatShip