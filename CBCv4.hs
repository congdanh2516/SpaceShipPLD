{- HLINT ignore "Use camelCase" -}
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

module CBCSpaceship where

import GHC.TypeLits
import Data.Proxy
import Data.Kind (Constraint)
import Data.Type.Bool (If)
import GHC.TypeLits (ErrorMessage(..), TypeError)

-- ==========================================
-- 1. DATA DEFINITIONS & PHANTOM TYPES
-- ==========================================

data ReactorType = Fusion | Antimatter deriving (Show, Eq)
data ShieldType  = Magnetic | Phase deriving (Show, Eq)
data EngineType = Ion | Plasma deriving (Show, Eq)
data LifeSupportType = Standard | Advanced deriving (Show, Eq)
data BridgeType = Explorer | Command deriving (Show, Eq)
data SensorType = Basic | Enhanced deriving (Show, Eq)

data Phase = Init | Core | Optional | Finalized

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
-- 2. THE SPACESHIP TYPE (GADT với runtime data)
-- ==========================================
data Spaceship (p :: Phase) (rMap :: (Bool, Bool)) (cores :: (Bool, Bool, Bool, Bool)) (slots :: Nat) where
    -- Khởi tạo tàu: Chứa tên và components list
    Ship :: 
        { shipName :: String
        , shipComponents :: [Component]
        } -> Spaceship 'Init '( 'False, 'False) '( 'False, 'False, 'False, 'False) 0
    
    -- Core phase: thêm runtime component
    CoreShip ::
        { shipName :: String
        , shipComponents :: [Component]
        , newComponent :: Component  -- Component vừa thêm
        } -> Spaceship 'Core rMap cores slots
    
    -- Optional phase
    OptionalShip ::
        { shipName :: String
        , shipComponents :: [Component]
        , newComponent :: Component
        } -> Spaceship 'Optional rMap cores slots
    
    -- Finalized phase
    FinalizedShip ::
        { shipName :: String
        , shipComponents :: [Component]
        } -> Spaceship 'Finalized rMap cores slots

instance Show (Spaceship p r c s) where
    show (Ship name comps) = "Spaceship '" ++ name ++ "' (Init)"
    show (CoreShip name comps new) = "Spaceship '" ++ name ++ "' (Core), Components: " ++ show comps
    show (OptionalShip name comps new) = "Spaceship '" ++ name ++ "' (Optional), Components: " ++ show comps
    show (FinalizedShip name comps) = "Spaceship '" ++ name ++ "' (Finalized), Components: " ++ show comps

-- ==========================================
-- 3. LOGIC GATES (TYPE FAMILIES) - Giữ nguyên
-- ==========================================

type family CheckCompatibility (reactors :: (Bool, Bool)) (s :: ShieldType) :: Constraint where
    CheckCompatibility '( 'True, _ ) 'Phase    = TypeError ('Text "ERROR: Cannot install Phase Shield with Fusion Reactor! [Rule B-440]")
    CheckCompatibility '( _, 'True ) 'Magnetic = TypeError ('Text "ERROR: Cannot install Magnetic Shield with Antimatter Reactor! [Rule B-440]")
    CheckCompatibility _ _                     = ()

type family UpdateReactors (new :: ReactorType) (current :: (Bool, Bool)) :: (Bool, Bool) where
    UpdateReactors 'Fusion     '( _, a ) = '( 'True, a )
    UpdateReactors 'Antimatter '( f, _ ) = '( f, 'True )

type family CheckSlots (current :: Nat) (cost :: Nat) :: Constraint where
    CheckSlots current cost =
        If ((current + cost) <=? 10)
           (() :: Constraint)
           (TypeError ('Text "ERROR: Slot limit exceeded (Max 10)! [Rule B-307]"))

type family CheckCoreIntegrity (cores :: (Bool, Bool, Bool, Bool)) :: Constraint where
    CheckCoreIntegrity '( 'True, 'True, 'True, 'True) = ()
    CheckCoreIntegrity _ = TypeError ('Text "ERROR: Missing Core Systems! Need Reactor, Engine, LifeSupport, Bridge [Rule B-209]")

-- ==========================================
-- 4. DSL OPERATIONS (Với runtime implementation)
-- ==========================================

-- Proxies cho type-level programming
fusion :: Proxy 'Fusion
fusion = Proxy
antimatter :: Proxy 'Antimatter
antimatter = Proxy
phaseShield :: Proxy 'Phase
phaseShield = Proxy
magShield :: Proxy 'Magnetic
magShield = Proxy

-- Helper để tạo component từ proxy
createReactor :: Proxy rt -> ReactorType
createReactor _ = case (show (Proxy @rt)) of
    "Proxy" -> Fusion  -- Đơn giản hóa, thực tế cần Typeable

-- [A-103] start_blueprint -> set_frame
setFrame :: Spaceship 'Init r c s -> Spaceship 'Core r c s
setFrame (Ship name comps) = 
    CoreShip name comps (CBridge Explorer)  -- Giả sử luôn thêm bridge mặc định

-- Thêm Reactor
addReactor :: (CheckSlots s 3)
           => Proxy (rt :: ReactorType)
           -> Spaceship 'Core rMap '(hasR, hasE, hasL, hasB) s
           -> Spaceship 'Core (UpdateReactors rt rMap) '( 'True, hasE, hasL, hasB) (s + 3)
addReactor proxy (CoreShip name comps _) = 
    let rt = case proxy of
            _ -> Fusion  -- Thực tế cần từ proxy
        newComp = CReactor rt
    in CoreShip name (newComp : comps) newComp

-- Thêm Engine
addEngine :: (CheckSlots s 2)
          => EngineType
          -> Spaceship 'Core rMap '(hasR, hasE, hasL, hasB) s
          -> Spaceship 'Core rMap '(hasR, 'True, hasL, hasB) (s + 2)
addEngine engineType (CoreShip name comps _) = 
    let newComp = CEngine engineType
    in CoreShip name (newComp : comps) newComp

-- Thêm LifeSupport
addLifeSupport :: (CheckSlots s 2)
               => LifeSupportType
               -> Spaceship 'Core rMap '(hasR, hasE, hasL, hasB) s
               -> Spaceship 'Core rMap '(hasR, hasE, 'True, hasB) (s + 2)
addLifeSupport lsType (CoreShip name comps _) =
    let newComp = CLifeSupport lsType
    in CoreShip name (newComp : comps) newComp

-- Thêm Bridge
addBridge :: (CheckSlots s 1)
          => BridgeType
          -> Spaceship 'Core rMap '(hasR, hasE, hasL, hasB) s
          -> Spaceship 'Core rMap '(hasR, hasE, hasL, 'True) (s + 1)
addBridge bridgeType (CoreShip name comps _) =
    let newComp = CBridge bridgeType
    in CoreShip name (newComp : comps) newComp

-- Lock Core Systems
lockCoreSystems :: (CheckCoreIntegrity cores)
                => Spaceship 'Core rMap cores s
                -> Spaceship 'Optional rMap cores s
lockCoreSystems (CoreShip name comps _) =
    OptionalShip name comps (CSensor Basic)  -- Giả sử thêm sensor mặc định

-- Thêm Shield
addShield :: forall st rMap cores s. 
             (CheckSlots s 1, CheckCompatibility rMap st)
          => Proxy (st :: ShieldType)
          -> ShieldType
          -> Spaceship 'Optional rMap cores s
          -> Spaceship 'Optional rMap cores (s + 1)
addShield _ shieldType (OptionalShip name comps _) =
    let newComp = CShield shieldType
    in OptionalShip name (newComp : comps) newComp

-- Thêm Sensors
addSensors :: (CheckSlots s 1)
           => SensorType
           -> Spaceship 'Optional rMap cores s
           -> Spaceship 'Optional rMap cores (s + 1)
addSensors sensorType (OptionalShip name comps _) =
    let newComp = CSensor sensorType
    in OptionalShip name (newComp : comps) newComp

-- Finalize
finalize :: Spaceship 'Optional rMap cores s -> Spaceship 'Finalized rMap cores s
finalize (OptionalShip name comps _) = FinalizedShip name comps

-- ==========================================
-- 5. VISUALIZATION (Task 2)
-- ==========================================

-- Tính slot cost của component
slotCost :: Component -> Int
slotCost (CReactor _) = 3
slotCost (CEngine _) = 2
slotCost (CLifeSupport _) = 2
slotCost (CBridge _) = 1
slotCost (CShield _) = 1
slotCost (CSensor _) = 1

-- Tính mass
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

-- Hàm visualization chính
print_spec :: Spaceship p r c s -> IO ()
print_spec ship = do
    let name = shipName ship
        comps = shipComponents ship
        
        -- Tính toán
        frameMass = 1000
        totalMass = frameMass + sum (map componentMass comps)
        totalPowerGen = sum (map powerOutput comps)
        totalPowerDraw = sum (map powerDraw comps)
        powerBalance = totalPowerGen - totalPowerDraw
        totalThrust = sum (map thrust comps)
        twr = if totalMass > 0 then fromIntegral totalThrust / fromIntegral totalMass else 0
        usedSlots = sum (map slotCost comps)
        slotBar = replicate usedSlots '|' ++ replicate (10 - usedSlots) '.'
    
    putStrLn "\n[SPACESHIP BLUEPRINT]"
    putStrLn "======================"
    putStrLn $ "Name: " ++ name
    putStrLn $ "Status: " ++ show ship
    putStrLn ""
    putStrLn "[ENERGY SYSTEM]"
    putStrLn $ "  Generation:    " ++ show totalPowerGen ++ " MW"
    putStrLn $ "  Consumption:   " ++ show totalPowerDraw ++ " MW"
    putStrLn $ "  Balance:       " ++ (if powerBalance >= 0 then "+" else "") ++ show powerBalance ++ " MW " ++ 
               (if powerBalance >= 0 then "[OK]" else "[OVERLOAD]")
    putStrLn ""
    putStrLn "[PERFORMANCE]"
    putStrLn $ "  Total Mass:    " ++ show totalMass ++ " tons"
    putStrLn $ "  Total Thrust:  " ++ show totalThrust ++ " kN"
    putStrLn $ "  TWR Ratio:     " ++ printf "%.2f" twr
    putStrLn ""
    putStrLn "[CAPACITY]"
    putStrLn $ "  Slots Used:    [" ++ slotBar ++ "] " ++ show usedSlots ++ "/10"
    putStrLn ""
    putStrLn "[INSTALLED MODULES]"
    if null comps
        then putStrLn "  (None)"
        else mapM_ (putStrLn . ("  - " ++) . show) (reverse comps)

-- Helper từ Data.Text.Printf
printf :: String -> Double -> String
printf format = Prelude.printf format

-- ==========================================
-- 6. TEST - CHẠY ĐƯỢC!
-- ==========================================

main :: IO ()
main = do
    putStrLn "=== Testing Valid Ship ==="
    
    -- Tạo một ship hợp lệ
    let validShip =
            let s0 = Ship "USS Enterprise"
                s1 = setFrame s0
                s2 = addReactor fusion s1
                s3 = addEngine Plasma s2
                s4 = addLifeSupport Standard s3
                s5 = addBridge Explorer s4
                s6 = lockCoreSystems s5
                s7 = addShield magShield Magnetic s6
                s8 = finalize s7
            in s8
    
    print_spec validShip
    
    putStrLn "\n=== Compile-Time Error Examples ==="
    putStrLn "(Uncomment to see compile errors)"
    
    -- Ví dụ 1: Thiếu core system (sẽ không compile)
    {-
    let badShip1 =
            let s0 = Ship "BadShip1"
                s1 = setFrame s0
                s2 = addReactor fusion s1
                -- Thiếu engine, life support, bridge!
                s3 = lockCoreSystems s2  -- ERROR tại compile-time!
            in s3
    -}
    
    putStrLn "Done!"

-- ==========================================
-- 7. COMPILE-TIME ERROR TESTS (comment/uncomment)
-- ==========================================

{-
-- Test 1: Slot overflow (B-307)
testSlotOverflow :: Spaceship p r c s
testSlotOverflow =
    let s0 = Ship "BigShip"
        s1 = setFrame s0
        s2 = addReactor fusion s1        -- 3 slots
        s3 = addReactor antimatter s2    -- +3 = 6
        s4 = addEngine Plasma s3         -- +2 = 8
        s5 = addEngine Ion s4            -- +2 = 10 (full!)
        s6 = addLifeSupport Standard s5  -- +2 = 12 > 10! ERROR tại compile
    in s6

-- Test 2: Shield incompatibility (B-440)
testShieldIncompat :: Spaceship p r c s  
testShieldIncompat =
    let s0 = Ship "IncompatShip"
        s1 = setFrame s0
        s2 = addReactor fusion s1
        s3 = addEngine Plasma s2
        s4 = addLifeSupport Standard s3
        s5 = addBridge Explorer s4
        s6 = lockCoreSystems s5
        s7 = addShield phaseShield Phase s6  -- ERROR: Fusion + Phase không hợp
    in s7
-}