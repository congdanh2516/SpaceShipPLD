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

module Main where

import GHC.TypeLits
import Data.Proxy
import Data.Kind (Constraint)
import Data.Type.Bool (If)
import Text.Printf (printf)

-- ==========================================
-- 0. HELPER OPERATOR (THE FIX)
-- ==========================================

-- Toán tử Pipe (|>) giúp viết code theo chuỗi từ trên xuống dưới
-- x |> f tương đương với f(x)
infixl 0 |>
(|>) :: a -> (a -> b) -> b
x |> f = f x

-- ==========================================
-- 1. DATA DEFINITIONS (VALUES)
-- ==========================================

data ReactorType = Fusion | Antimatter deriving (Show, Eq)
data EngineType = Ion | Plasma deriving (Show, Eq)
data LifeSupportType = Standard | Advanced deriving (Show, Eq)
data BridgeType = Explorer | Command deriving (Show, Eq)
data ShieldType = Magnetic | Phase deriving (Show, Eq)
data SensorType = Basic | Enhanced deriving (Show, Eq)

data Component = CReactor ReactorType
               | CEngine EngineType
               | CLifeSupport LifeSupportType
               | CBridge BridgeType
               | CShield ShieldType
               | CSensor SensorType
               deriving (Show, Eq)

-- ==========================================
-- 2. TYPE LEVEL DEFINITIONS (PHANTOMS)
-- ==========================================

data Phase = Init | Core | Optional | Finalized

-- ==========================================
-- 3. LOGIC GATES (TYPE FAMILIES)
-- ==========================================

-- [Rule B-307] SLOT LIMIT CHECK
type family CheckSlots (current :: Nat) (cost :: Nat) :: Constraint where
    CheckSlots current cost =
        If ((current + cost) <=? 10)
           (() :: Constraint)
           (TypeError ('Text "[B-307] SLOT OVERFLOW: Not enough slots remaining in the Frame!"))

-- [Rule B-440] CHECK SHIELD COMPATIBILITY
type family CheckCompatibility (reactors :: (Bool, Bool)) (s :: ShieldType) :: Constraint where
    CheckCompatibility '( 'True, _ ) 'Phase    = TypeError ('Text "[B-440] ILLEGAL: Phase Shield conflicts with Fusion Reactor.")
    CheckCompatibility '( _, 'True ) 'Magnetic = TypeError ('Text "[B-440] ILLEGAL: Magnetic Shield conflicts with Antimatter Reactor.")
    CheckCompatibility _ _                     = ()

-- [Logic] UPDATE REACTOR STATE
type family UpdateReactors (new :: ReactorType) (current :: (Bool, Bool)) :: (Bool, Bool) where
    UpdateReactors 'Fusion     '( _, a ) = '( 'True, a )
    UpdateReactors 'Antimatter '( f, _ ) = '( f, 'True )

-- [Rule B-209] CORE INTEGRITY CHECK
type family CheckCoreIntegrity (cores :: (Bool, Bool, Bool, Bool)) :: Constraint where
    CheckCoreIntegrity '( 'True, 'True, 'True, 'True) = ()
    CheckCoreIntegrity _ = TypeError ('Text "[B-209] INCOMPLETE CORE: Reactor, Engine, LifeSupport, and Bridge are ALL required before locking.")

-- ==========================================
-- 4. THE SPACESHIP GADT
-- ==========================================

data Spaceship (p :: Phase) (rMap :: (Bool, Bool)) (cores :: (Bool, Bool, Bool, Bool)) (slots :: Nat) where
    UnsafeSpaceship :: {
        shipName :: String,
        installedComps :: [Component],
        runtimeSlots :: Int
    } -> Spaceship p rMap cores slots

instance Show (Spaceship p r c s) where
    show s = "Spaceship '" ++ shipName s ++ "' (Type Validated)"

-- ==========================================
-- 5. DSL OPERATIONS
-- ==========================================

-- Proxies
fusion = Proxy :: Proxy 'Fusion
antimatter = Proxy :: Proxy 'Antimatter
ion = Proxy :: Proxy 'Ion
plasma = Proxy :: Proxy 'Plasma
standard = Proxy :: Proxy 'Standard
advanced = Proxy :: Proxy 'Advanced
explorer = Proxy :: Proxy 'Explorer
command = Proxy :: Proxy 'Command
magnetic = Proxy :: Proxy 'Magnetic
phase = Proxy :: Proxy 'Phase
basic = Proxy :: Proxy 'Basic
enhanced = Proxy :: Proxy 'Enhanced

-- [A-103] start_blueprint
start_blueprint :: String -> Spaceship 'Init '( 'False, 'False) '( 'False, 'False, 'False, 'False) 0
start_blueprint name = UnsafeSpaceship name [] 0

-- [A-103] set_frame
set_frame :: Spaceship 'Init r c s -> Spaceship 'Core r c s
set_frame (UnsafeSpaceship name comps s) = UnsafeSpaceship name comps s

-- ADD REACTOR
addReactor :: forall (rt :: ReactorType) rMap hasR hasE hasL hasB s.
           (CheckSlots s 3, KnownSymbol (ShowType rt))
           => Proxy rt
           -> Spaceship 'Core rMap '(hasR, hasE, hasL, hasB) s
           -> Spaceship 'Core (UpdateReactors rt rMap) '( 'True, hasE, hasL, hasB) (s + 3)
addReactor _ (UnsafeSpaceship name comps s) =
    let rVal = case (symbolVal (Proxy :: Proxy (ShowType rt))) of
                 "Fusion" -> Fusion
                 "Antimatter" -> Antimatter
                 _ -> Fusion
    in UnsafeSpaceship name (CReactor rVal : comps) (s + 3)

type family ShowType (a :: k) :: Symbol where
    ShowType 'Fusion = "Fusion"
    ShowType 'Antimatter = "Antimatter"
    ShowType 'Ion = "Ion"
    ShowType 'Plasma = "Plasma"
    ShowType 'Standard = "Standard"
    ShowType 'Advanced = "Advanced"
    ShowType 'Explorer = "Explorer"
    ShowType 'Command = "Command"
    ShowType 'Magnetic = "Magnetic"
    ShowType 'Phase = "Phase"
    ShowType 'Basic = "Basic"
    ShowType 'Enhanced = "Enhanced"

-- ADD ENGINE
addEngine :: forall (et :: EngineType) rMap hasR hasE hasL hasB s.
          (CheckSlots s 2, KnownSymbol (ShowType et))
          => Proxy et
          -> Spaceship 'Core rMap '(hasR, hasE, hasL, hasB) s
          -> Spaceship 'Core rMap '(hasR, 'True, hasL, hasB) (s + 2)
addEngine _ (UnsafeSpaceship name comps s) =
    let eVal = case (symbolVal (Proxy :: Proxy (ShowType et))) of
                 "Ion" -> Ion
                 "Plasma" -> Plasma
                 _ -> Ion
    in UnsafeSpaceship name (CEngine eVal : comps) (s + 2)

-- ADD LIFE SUPPORT
addLifeSupport :: forall (lt :: LifeSupportType) rMap hasR hasE hasL hasB s.
               (CheckSlots s 2, KnownSymbol (ShowType lt))
               => Proxy lt
               -> Spaceship 'Core rMap '(hasR, hasE, hasL, hasB) s
               -> Spaceship 'Core rMap '(hasR, hasE, 'True, hasB) (s + 2)
addLifeSupport _ (UnsafeSpaceship name comps s) =
    let lVal = case (symbolVal (Proxy :: Proxy (ShowType lt))) of
                 "Standard" -> Standard
                 "Advanced" -> Advanced
                 _ -> Standard
    in UnsafeSpaceship name (CLifeSupport lVal : comps) (s + 2)

-- ADD BRIDGE
addBridge :: forall (bt :: BridgeType) rMap hasR hasE hasL hasB s.
          (CheckSlots s 1, KnownSymbol (ShowType bt))
          => Proxy bt
          -> Spaceship 'Core rMap '(hasR, hasE, hasL, hasB) s
          -> Spaceship 'Core rMap '(hasR, hasE, hasL, 'True) (s + 1)
addBridge _ (UnsafeSpaceship name comps s) =
    let bVal = case (symbolVal (Proxy :: Proxy (ShowType bt))) of
                 "Explorer" -> Explorer
                 "Command" -> Command
                 _ -> Explorer
    in UnsafeSpaceship name (CBridge bVal : comps) (s + 1)

-- [A-305] & [B-209] LOCK CORE SYSTEMS
lockCoreSystems :: (CheckCoreIntegrity cores)
                => Spaceship 'Core rMap cores s
                -> Spaceship 'Optional rMap cores s
lockCoreSystems (UnsafeSpaceship name comps s) = UnsafeSpaceship name comps s

-- ADD SHIELD
addShield :: forall (st :: ShieldType) rMap cores s.
          (CheckSlots s 1, CheckCompatibility rMap st, KnownSymbol (ShowType st))
          => Proxy st
          -> Spaceship 'Optional rMap cores s
          -> Spaceship 'Optional rMap cores (s + 1)
addShield _ (UnsafeSpaceship name comps s) =
    let sVal = case (symbolVal (Proxy :: Proxy (ShowType st))) of
                 "Magnetic" -> Magnetic
                 "Phase" -> Phase
                 _ -> Magnetic
    in UnsafeSpaceship name (CShield sVal : comps) (s + 1)

-- ADD SENSORS
addSensor :: forall (st :: SensorType) rMap cores s.
          (CheckSlots s 1, KnownSymbol (ShowType st))
          => Proxy st
          -> Spaceship 'Optional rMap cores s
          -> Spaceship 'Optional rMap cores (s + 1)
addSensor _ (UnsafeSpaceship name comps s) =
    let sVal = case (symbolVal (Proxy :: Proxy (ShowType st))) of
                 "Basic" -> Basic
                 "Enhanced" -> Enhanced
                 _ -> Basic
    in UnsafeSpaceship name (CSensor sVal : comps) (s + 1)

-- [A-212] FINALIZE
finalize :: Spaceship 'Optional rMap cores s -> Spaceship 'Finalized rMap cores s
finalize (UnsafeSpaceship name comps s) = UnsafeSpaceship name comps s

-- ==========================================
-- 6. VISUALIZATION
-- ==========================================

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

thrust :: Component -> Int
thrust (CEngine Ion) = 500
thrust (CEngine Plasma) = 750
thrust _ = 0

print_spec :: Spaceship 'Finalized r c s -> IO ()
print_spec (UnsafeSpaceship name comps used) = do
    let frameMass = 1000
    let totalMass = frameMass + sum (map componentMass comps)
    let totalPowerGen = sum (map powerOutput comps)
    let totalPowerDraw = sum (map powerDraw comps)
    let powerBalance = totalPowerGen - totalPowerDraw
    let totalThrust = sum (map thrust comps)
    let twr = fromIntegral totalThrust / fromIntegral totalMass :: Double
    let tSlots = 10
    let slotBar = replicate used '|' ++ replicate (tSlots - used) '.'

    putStrLn ""
    putStrLn "+--------------------------------------------------+"
    putStrLn "|               SPACESHIP BLUEPRINT                |"
    putStrLn "+--------------------------------------------------+"
    printf   "| NAME    : %-15s                        |\n" name
    printf   "| STATUS  : FINALIZED (Type Checked)               |\n"
    putStrLn "+--------------------------------------------------+"
    putStrLn "| [ENERGY SYSTEM]                                  |"
    printf   "| Generation    : %5d MW                         |\n" totalPowerGen
    printf   "| Consumption   : %5d MW                         |\n" totalPowerDraw
    let balanceSign = if powerBalance >= 0 then "+" else ""
    let statusText = if powerBalance >= 0 then "[OK]" else "[OVERLOAD]"
    printf   "| BALANCE       : %s%d MW %-12s             |\n" balanceSign powerBalance statusText
    putStrLn "|                                                  |"
    putStrLn "| [PERFORMANCE]                                    |"
    printf   "| Total Mass    : %5d tons                       |\n" totalMass
    printf   "| Total Thrust  : %5d kN                         |\n" totalThrust
    printf   "| TWR Ratio     : %5.2f                            |\n" twr
    putStrLn "|                                                  |"
    putStrLn "| [CAPACITY]                                       |"
    printf   "| Slots Used    : [%s] %2d/%d               |\n" slotBar used tSlots
    putStrLn "+--------------------------------------------------+"
    putStrLn "| INSTALLED MODULES:                               |"
    mapM_ (printf "| - %-47s|\n" . show) (reverse comps)
    putStrLn "+--------------------------------------------------+\n"

-- ==========================================
-- 7. SCENARIOS (Fixed Syntax using |>)
-- ==========================================

-- Chú ý: Dùng |> thay vì backtick
validShip = 
    start_blueprint "USS TypeSafe"
    |> set_frame
    |> addReactor fusion       -- 3 slots
    |> addEngine plasma        -- 2 slots
    |> addLifeSupport standard -- 2 slots
    |> addBridge explorer      -- 1 slot
    |> lockCoreSystems
    |> addShield magnetic      -- 1 slot (Magnetic OK với Fusion)
    |> addSensor basic         -- 1 slot
    |> finalize

{- 
-- DEMO LỖI (Bỏ comment để thấy compile error)

-- Lỗi quá tải slot (11/10)
badSlotShip = 
    start_blueprint "USS Overflow"
    |> set_frame
    |> addReactor fusion
    |> addReactor fusion
    |> addReactor fusion
    |> addEngine ion -- Error here

-- Lỗi xung đột khiên (Fusion kỵ Phase)
badShieldShip = 
    start_blueprint "USS Conflict"
    |> set_frame
    |> addReactor fusion
    |> addEngine ion
    |> addLifeSupport standard
    |> addBridge explorer
    |> lockCoreSystems
    |> addShield phase -- Error here
-}

main :: IO ()
main = do
    putStrLn "--- Hardcore CBC Visualization ---"
    print_spec validShip