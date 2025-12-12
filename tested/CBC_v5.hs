{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE ConstraintKinds #-}

module CBCSpaceship where

import GHC.TypeLits
import Data.Proxy
import Data.Kind (Constraint)
import Data.Type.Bool (If)
import GHC.TypeLits (ErrorMessage(..), TypeError)
import Text.Printf (printf)
import Data.List (intercalate)

-- ==========================================
-- 1. TYPE-LEVEL DEFINITIONS & SINGLETONS
-- ==========================================

-- Type-level enums
data ReactorTypeTL = FusionTL | AntimatterTL
data ShieldTypeTL = MagneticTL | PhaseTL
data EngineTypeTL = IonTL | PlasmaTL
data LifeSupportTypeTL = StandardTL | AdvancedTL
data BridgeTypeTL = ExplorerTL | CommandTL
data SensorTypeTL = BasicTL | EnhancedTL

data Phase = Init | Core | Optional | Finalized

-- Runtime value types (cho visualization)
data ReactorType = Fusion | Antimatter deriving (Show, Eq)
data ShieldType = Magnetic | Phase deriving (Show, Eq)
data EngineType = Ion | Plasma deriving (Show, Eq)
data LifeSupportType = Standard | Advanced deriving (Show, Eq)
data BridgeType = Explorer | Command deriving (Show, Eq)
data SensorType = Basic | Enhanced deriving (Show, Eq)

-- Runtime component data
data Component = 
    CReactor ReactorType
    | CEngine EngineType
    | CLifeSupport LifeSupportType
    | CBridge BridgeType
    | CShield ShieldType
    | CSensor SensorType
    deriving (Show)

-- ==========================================
-- 2. SINGLETONS (kết nối type-level và value-level)
-- ==========================================

-- Singleton cho ReactorType
data SReactorType (rt :: ReactorTypeTL) where
    SFusion :: SReactorType 'FusionTL
    SAntimatter :: SReactorType 'AntimatterTL

-- Singleton cho ShieldType
data SShieldType (st :: ShieldTypeTL) where
    SMagnetic :: SShieldType 'MagneticTL
    SPhase :: SShieldType 'PhaseTL

-- Singleton cho EngineType
data SEngineType (et :: EngineTypeTL) where
    SIon :: SEngineType 'IonTL
    SPlasma :: SEngineType 'PlasmaTL

-- Singleton cho LifeSupportType
data SLifeSupportType (lst :: LifeSupportTypeTL) where
    SStandard :: SLifeSupportType 'StandardTL
    SAdvanced :: SLifeSupportType 'AdvancedTL

-- Singleton cho BridgeType
data SBridgeType (bt :: BridgeTypeTL) where
    SExplorer :: SBridgeType 'ExplorerTL
    SCommand :: SBridgeType 'CommandTL

-- Singleton cho SensorType
data SSensorType (st :: SensorTypeTL) where
    SBasic :: SSensorType 'BasicTL
    SEnhanced :: SSensorType 'EnhancedTL

-- Convert singletons to runtime values
fromSReactor :: SReactorType rt -> ReactorType
fromSReactor SFusion = Fusion
fromSReactor SAntimatter = Antimatter

fromSShield :: SShieldType st -> ShieldType
fromSShield SMagnetic = Magnetic
fromSShield SPhase = Phase

fromSEngine :: SEngineType et -> EngineType
fromSEngine SIon = Ion
fromSEngine SPlasma = Plasma

fromSLifeSupport :: SLifeSupportType lst -> LifeSupportType
fromSLifeSupport SStandard = Standard
fromSLifeSupport SAdvanced = Advanced

fromSBridge :: SBridgeType bt -> BridgeType
fromSBridge SExplorer = Explorer
fromSBridge SCommand = Command

fromSSensor :: SSensorType st -> SensorType
fromSSensor SBasic = Basic
fromSSensor SEnhanced = Enhanced

-- ==========================================
-- 3. TYPE FAMILIES (Logic Gates)
-- ==========================================

-- [B-440] Shield-Reactor Compatibility
type family CheckCompatibility (reactors :: (Bool, Bool)) (s :: ShieldTypeTL) :: Constraint where
    CheckCompatibility '( 'True, _ ) 'PhaseTL    = TypeError ('Text "ERROR: Cannot install Phase Shield with Fusion Reactor! [Rule B-440]")
    CheckCompatibility '( _, 'True ) 'MagneticTL = TypeError ('Text "ERROR: Cannot install Magnetic Shield with Antimatter Reactor! [Rule B-440]")
    CheckCompatibility _ _                       = ()

-- Update reactor flags
type family UpdateReactors (new :: ReactorTypeTL) (current :: (Bool, Bool)) :: (Bool, Bool) where
    UpdateReactors 'FusionTL     '( _, a ) = '( 'True, a )
    UpdateReactors 'AntimatterTL '( f, _ ) = '( f, 'True )

-- [B-307] Slot limit check
type family CheckSlots (current :: Nat) (cost :: Nat) :: Constraint where
    CheckSlots current cost =
        If ((current + cost) <=? 10)
           (() :: Constraint)
           (TypeError ('Text "ERROR: Slot limit exceeded (Max 10)! [Rule B-307]"))

-- [B-209] Core integrity check
type family CheckCoreIntegrity (cores :: (Bool, Bool, Bool, Bool)) :: Constraint where
    CheckCoreIntegrity '( 'True, 'True, 'True, 'True) = ()
    CheckCoreIntegrity _ = TypeError ('Text "ERROR: Missing Core Systems! Need Reactor, Engine, LifeSupport, Bridge [Rule B-209]")

-- ==========================================
-- 4. SPACESHIP GADT (với runtime data)
-- ==========================================

data Spaceship (p :: Phase) (rMap :: (Bool, Bool)) (cores :: (Bool, Bool, Bool, Bool)) (slots :: Nat) where
    -- Init phase: chỉ có tên
    ShipInit :: 
        { shipName :: String
        } -> Spaceship 'Init '( 'False, 'False) '( 'False, 'False, 'False, 'False) 0
    
    -- Core phase: đã có components
    ShipCore :: 
        { shipName :: String
        , shipComponents :: [Component]
        } -> Spaceship 'Core rMap cores slots
    
    -- Optional phase
    ShipOptional :: 
        { shipName :: String
        , shipComponents :: [Component]
        } -> Spaceship 'Optional rMap cores slots
    
    -- Finalized phase
    ShipFinalized :: 
        { shipName :: String
        , shipComponents :: [Component]
        } -> Spaceship 'Finalized rMap cores slots

-- Show instance cho debugging
instance Show (Spaceship p r c s) where
    show (ShipInit name) = "ShipInit '" ++ name ++ "'"
    show (ShipCore name comps) = "ShipCore '" ++ name ++ "' (" ++ show (length comps) ++ " components)"
    show (ShipOptional name comps) = "ShipOptional '" ++ name ++ "' (" ++ show (length comps) ++ " components)"
    show (ShipFinalized name comps) = "ShipFinalized '" ++ name ++ "' (" ++ show (length comps) ++ " components)"

-- ==========================================
-- 5. DSL OPERATIONS (Type-safe với singletons)
-- ==========================================

-- [A-103] start_blueprint
start_blueprint :: String -> Spaceship 'Init '( 'False, 'False) '( 'False, 'False, 'False, 'False) 0
start_blueprint = ShipInit

-- [A-201] set_frame
set_frame :: Spaceship 'Init r c s -> Spaceship 'Core r c s
set_frame (ShipInit name) = ShipCore name []

-- [A-305] Core modules - Reactor
add_reactor :: forall rt rMap hasR hasE hasL hasB s.
               (CheckSlots s 3)
            => SReactorType rt
            -> Spaceship 'Core rMap '(hasR, hasE, hasL, hasB) s
            -> Spaceship 'Core (UpdateReactors rt rMap) '( 'True, hasE, hasL, hasB) (s + 3)
add_reactor srt (ShipCore name comps) =
    let reactorValue = fromSReactor srt
        newComp = CReactor reactorValue
    in ShipCore name (newComp : comps)

-- [A-305] Core modules - Engine
add_engine :: forall et rMap hasR hasE hasL hasB s.
              (CheckSlots s 2)
           => SEngineType et
           -> Spaceship 'Core rMap '(hasR, hasE, hasL, hasB) s
           -> Spaceship 'Core rMap '(hasR, 'True, hasL, hasB) (s + 2)
add_engine set (ShipCore name comps) =
    let engineValue = fromSEngine set
        newComp = CEngine engineValue
    in ShipCore name (newComp : comps)

-- [A-305] Core modules - LifeSupport
add_life_support :: forall lst rMap hasR hasE hasL hasB s.
                    (CheckSlots s 2)
                 => SLifeSupportType lst
                 -> Spaceship 'Core rMap '(hasR, hasE, hasL, hasB) s
                 -> Spaceship 'Core rMap '(hasR, hasE, 'True, hasB) (s + 2)
add_life_support slst (ShipCore name comps) =
    let lsValue = fromSLifeSupport slst
        newComp = CLifeSupport lsValue
    in ShipCore name (newComp : comps)

-- [A-305] Core modules - Bridge
add_bridge :: forall bt rMap hasR hasE hasL hasB s.
              (CheckSlots s 1)
           => SBridgeType bt
           -> Spaceship 'Core rMap '(hasR, hasE, hasL, hasB) s
           -> Spaceship 'Core rMap '(hasR, hasE, hasL, 'True) (s + 1)
add_bridge sbt (ShipCore name comps) =
    let bridgeValue = fromSBridge sbt
        newComp = CBridge bridgeValue
    in ShipCore name (newComp : comps)

-- [B-209] lock_core_systems
lock_core_systems :: (CheckCoreIntegrity cores)
                  => Spaceship 'Core rMap cores s
                  -> Spaceship 'Optional rMap cores s
lock_core_systems (ShipCore name comps) = ShipOptional name comps

-- [A-305] Optional modules - Shield
add_shield :: forall st rMap cores s.
              (CheckSlots s 1, CheckCompatibility rMap st)
           => SShieldType st
           -> Spaceship 'Optional rMap cores s
           -> Spaceship 'Optional rMap cores (s + 1)
add_shield sst (ShipOptional name comps) =
    let shieldValue = fromSShield sst
        newComp = CShield shieldValue
    in ShipOptional name (newComp : comps)

-- [A-305] Optional modules - Sensor
add_sensor :: forall st rMap cores s.
              (CheckSlots s 1)
           => SSensorType st
           -> Spaceship 'Optional rMap cores s
           -> Spaceship 'Optional rMap cores (s + 1)
add_sensor sst (ShipOptional name comps) =
    let sensorValue = fromSSensor sst
        newComp = CSensor sensorValue
    in ShipOptional name (newComp : comps)

-- [A-212] finalize_blueprint
finalize_blueprint :: Spaceship 'Optional rMap cores s -> Spaceship 'Finalized rMap cores s
finalize_blueprint (ShipOptional name comps) = ShipFinalized name comps

-- ==========================================
-- 6. VISUALIZATION (Task 2)
-- ==========================================

-- Slot cost
slotCost :: Component -> Int
slotCost (CReactor _) = 3
slotCost (CEngine _) = 2
slotCost (CLifeSupport _) = 2
slotCost (CBridge _) = 1
slotCost (CShield _) = 1
slotCost (CSensor _) = 1

-- Mass
componentMass :: Component -> Int
componentMass (CReactor Fusion) = 300
componentMass (CReactor Antimatter) = 450
componentMass (CEngine Ion) = 100
componentMass (CEngine Plasma) = 120
componentMass (CLifeSupport Standard) = 80
componentMass (CLifeSupport Advanced) = 70
componentMass (CBridge Explorer) = 50
componentMass (CBridge Command) = 60
componentMass (CShield _) = 40
componentMass (CSensor Basic) = 30
componentMass (CSensor Enhanced) = 35

-- Power calculations
powerOutput :: Component -> Int
powerOutput (CReactor _) = 1000
powerOutput _ = 0

powerDraw :: Component -> Int
powerDraw (CEngine _) = 250
powerDraw (CLifeSupport _) = 50
powerDraw (CBridge _) = 75
powerDraw (CShield _) = 100
powerDraw (CSensor _) = 50
powerDraw _ = 0

-- Thrust
thrust :: Component -> Int
thrust (CEngine Ion) = 500
thrust (CEngine Plasma) = 750
thrust _ = 0

-- Helper: total slots used
totalSlotsUsed :: [Component] -> Int
totalSlotsUsed = sum . map slotCost

-- Helper: format component list
formatComponents :: [Component] -> String
formatComponents = intercalate "\n" . map (\c -> "  - " ++ show c)

-- [TASK 2] print_spec - ASCII visualization
print_spec :: Spaceship p r c s -> IO ()
print_spec ship = do
    let (name, comps) = case ship of
            ShipInit n -> (n, [])
            ShipCore n cs -> (n, cs)
            ShipOptional n cs -> (n, cs)
            ShipFinalized n cs -> (n, cs)
        
        -- Calculations
        frameMass = 1000
        totalMass = frameMass + sum (map componentMass comps)
        totalPowerGen = sum (map powerOutput comps)
        totalPowerDraw = sum (map powerDraw comps)
        powerBalance = totalPowerGen - totalPowerDraw
        totalThrust = sum (map thrust comps)
        twr = if totalMass > 0 then fromIntegral totalThrust / fromIntegral totalMass else 0.0
        usedSlots = totalSlotsUsed comps
        slotBar = replicate usedSlots '█' ++ replicate (10 - usedSlots) '░'
        
        -- Phase string
        phaseStr = case ship of
            ShipInit _ -> "INITIALIZING"
            ShipCore _ _ -> "CORE BUILDING"
            ShipOptional _ _ -> "OPTIONAL BUILDING"
            ShipFinalized _ _ -> "FINALIZED"
    
    putStrLn "\n╔════════════════════════════════════════════════════════╗"
    putStrLn "║                 SPACESHIP BLUEPRINT                   ║"
    putStrLn "╠════════════════════════════════════════════════════════╣"
    printf  "║ NAME:   %-45s ║\n" name
    printf  "║ STATUS: %-45s ║\n" phaseStr
    putStrLn "╠════════════════════════════════════════════════════════╣"
    putStrLn "║ [ENERGY SYSTEM]                                       ║"
    printf  "║   Power Generation: %5d MW                          ║\n" totalPowerGen
    printf  "║   Power Consumption: %5d MW                         ║\n" totalPowerDraw
    
    let powerStatus = if powerBalance >= 0 then "✓ BALANCED" else "✗ OVERLOADED"
    printf  "║   Power Balance:    %+5d MW (%s)               ║\n" powerBalance powerStatus
    
    putStrLn "╠════════════════════════════════════════════════════════╣"
    putStrLn "║ [PERFORMANCE]                                         ║"
    printf  "║   Total Mass:       %5d tons                       ║\n" totalMass
    printf  "║   Total Thrust:     %5d kN                         ║\n" totalThrust
    printf  "║   Thrust/Weight:    %7.2f                          ║\n" twr
    
    putStrLn "╠════════════════════════════════════════════════════════╣"
    putStrLn "║ [CAPACITY]                                            ║"
    putStrLn "║   Slots:  ["
    putStr "║            " 
    putStr slotBar
    printf "] %d/10                  ║\n" usedSlots
    
    putStrLn "╠════════════════════════════════════════════════════════╣"
    putStrLn "║ [INSTALLED MODULES]                                   ║"
    if null comps
        then putStrLn "║   (No modules installed)                            ║"
        else do
            putStr "║" 
            mapM_ (\c -> printf "   - %-43s║\n" (show c)) (reverse comps)
    putStrLn "╚════════════════════════════════════════════════════════╝"
    putStrLn ""

-- ==========================================
-- 7. TEST CASES & EXAMPLES
-- ==========================================

-- Example 1: Valid ship (compiles and runs)
validShip :: Spaceship 'Finalized '( 'True, 'False) '( 'True, 'True, 'True, 'True) 9
validShip =
    let s0 = start_blueprint "USS Enterprise"
        s1 = set_frame s0
        s2 = add_reactor SFusion s1
        s3 = add_engine SPlasma s2
        s4 = add_life_support SStandard s3
        s5 = add_bridge SExplorer s4
        s6 = lock_core_systems s5
        s7 = add_shield SMagnetic s6
        s8 = add_sensor SBasic s7
        s9 = finalize_blueprint s8
    in s9

-- Example 2: Another valid ship
validShip2 :: Spaceship 'Finalized '( 'False, 'True) '( 'True, 'True, 'True, 'True) 10
validShip2 =
    let s0 = start_blueprint "Millennium Falcon"
        s1 = set_frame s0
        s2 = add_reactor SAntimatter s1
        s3 = add_engine SIon s2
        s4 = add_life_support SAdvanced s3
        s5 = add_bridge SCommand s4
        s6 = lock_core_systems s5
        s7 = add_shield SPhase s6  -- Antimatter + Phase = OK
        s8 = add_sensor SEnhanced s7
        s9 = finalize_blueprint s8
    in s9

-- ==========================================
-- 8. COMPILE-TIME ERROR TESTS (comment/uncomment để test)
-- ==========================================

{-
-- ERROR 1: Missing core system (B-209)
errorMissingCore :: Spaceship p r c s
errorMissingCore =
    let s0 = start_blueprint "Bad Ship 1"
        s1 = set_frame s0
        s2 = add_reactor SFusion s1
        s3 = add_engine SPlasma s2
        -- Missing LifeSupport and Bridge!
        s4 = lock_core_systems s3  -- COMPILE ERROR: CheckCoreIntegrity fails
    in s4

-- ERROR 2: Slot overflow (B-307)
errorSlotOverflow :: Spaceship p r c s
errorSlotOverflow =
    let s0 = start_blueprint "Big Ship"
        s1 = set_frame s0
        s2 = add_reactor SFusion s1      -- 3 slots
        s3 = add_reactor SAntimatter s2  -- +3 = 6
        s4 = add_engine SPlasma s3       -- +2 = 8
        s5 = add_engine SIon s4          -- +2 = 10 (full!)
        s6 = add_life_support SStandard s5 -- +2 = 12 > 10! ERROR
    in s6

-- ERROR 3: Shield incompatibility (B-440)
errorShieldIncompat :: Spaceship p r c s
errorShieldIncompat =
    let s0 = start_blueprint "Incompat Ship"
        s1 = set_frame s0
        s2 = add_reactor SFusion s1
        s3 = add_engine SPlasma s2
        s4 = add_life_support SStandard s3
        s5 = add_bridge SExplorer s4
        s6 = lock_core_systems s5
        s7 = add_shield SPhase s6  -- ERROR: Fusion + Phase not compatible
    in s7

-- ERROR 4: Wrong build order (A-103, A-201)
errorWrongOrder :: Spaceship p r c s
errorWrongOrder =
    let s0 = start_blueprint "Wrong Order"
        s1 = add_reactor SFusion s0  -- ERROR: Can't add reactor before set_frame
    in s1
-}

-- ==========================================
-- 9. MAIN FUNCTION
-- ==========================================

main :: IO ()
main = do
    putStrLn "========================================="
    putStrLn "  CBC SPACESHIP DSL - Correct by Construction"
    putStrLn "========================================="
    
    putStrLn "\n=== Example 1: USS Enterprise ==="
    print_spec validShip
    
    putStrLn "\n=== Example 2: Millennium Falcon ==="
    print_spec validShip2
    
    putStrLn "\n=== Compile-Time Safety Features ==="
    putStrLn "The following errors are caught at COMPILE TIME:"
    putStrLn "1. [B-209] Missing core systems"
    putStrLn "2. [B-307] Slot limit exceeded"
    putStrLn "3. [B-440] Shield-reactor incompatibility"
    putStrLn "4. [A-103/A-201] Wrong build order"
    putStrLn "5. [A-305] Adding modules in wrong phase"
    
    putStrLn "\nTo see compile errors, uncomment error examples in code."
    putStrLn "========================================="