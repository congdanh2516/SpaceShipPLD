-- Concepts and Vocabulary

-- module Main where
{- HLINT ignore "Use camelCase" -}

import Control.Monad.State
import Control.Monad.Except
import Data.List (intercalate, find)
import Control.Monad
import Text.Printf (printf)

-- type
-- core modules
data ReactorType = Fusion | Antimatter deriving (Show, Eq)
data EngineType = Ion | Plasma deriving (Show, Eq)
data LifeSupportType = Standard | Advanced deriving (Show, Eq)
data BridgeType = Explorer | Command deriving (Show, Eq)
--optional modules
data ShieldType = Magnetic | Phase deriving (Show, Eq)
data SensorType = Basic | Enhanced deriving (Show, Eq) -- "Advanced" will be same as LifeSupportType type

-- component ? ~ class
data Component = Reactor ReactorType
               | Engine EngineType
               | LifeSupport LifeSupportType
               | Bridge BridgeType
               | Shield ShieldType
               | Sensor SensorType
               deriving (Show, Eq)

-- phases: 
data BuildPhase = InitPhase -- not started yet
                | NotFramePhase -- started, haven't frame yet
                | CorePhase -- install core
                | OptionalPhase -- installed core, installing optional
                | FinalizedPhase -- finished
                deriving (Show, Eq, Enum)


-- spaceship
data Spaceship = Spaceship {
    shipName :: String,
    currentPhase :: BuildPhase,
    components :: [Component],
    usedSlots :: Int,

    reactorCount :: Int,
    engineCount :: Int,
    lifeSupportCount :: Int,
    bridgeCount :: Int
} deriving (Show)
-- why not Shield and Sensor count?

-- initial spaceship
initialSpaceship :: Spaceship
initialSpaceship = Spaceship "" InitPhase [] 0 0 0 0 0

-- BUSINESS LOGIC

-- the numbers of slots required to install the module, squared that a module occupy in the ship
slotCost :: Component -> Int
slotCost (Reactor _) = 3
slotCost (Engine _) = 2
slotCost (LifeSupport _) = 2
slotCost (Bridge _) = 1
slotCost (Shield _) = 1
slotCost (Sensor _) = 1

-- mass
componentMass :: Component -> Int
componentMass (Reactor Fusion) = 300
componentMass (Reactor Antimatter) = 450
componentMass (Engine Ion) = 100
componentMass (Engine Plasma) = 120
componentMass (LifeSupport Standard) = 80
componentMass (LifeSupport Advanced) = 70
componentMass (Bridge Explorer) = 50
componentMass (Bridge Command) = 60
componentMass (Shield _) = 40
componentMass (Sensor Basic) = 30
componentMass (Sensor Enhanced) = 35

-- power output: only Reactor
powerOutput :: Component -> Int
powerOutput (Reactor _) = 1000
powerOutput _ = 0

-- power draw: except Reactor
powerDraw :: Component -> Int
powerDraw (Engine _) = 250
powerDraw (LifeSupport _) = 50
powerDraw (Bridge _) = 75
powerDraw (Shield _) = 100
powerDraw (Sensor _) = 50
powerDraw _ = 0 -- no consume

-- force: only Engine
thrust :: Component -> Int
thrust (Engine Ion) = 500
thrust (Engine Plasma) = 750
thrust _ = 0

-- TASK 1 - Core implementation

type ShipBuilder a = ExceptT String (State Spaceship) a -- this is the reason why s--> get will get Spaceship info, then assigning to s

-- slot check and update component list
addComponent :: Component -> ShipBuilder ()
addComponent comp = do
    s <- get
    when (currentPhase s == FinalizedPhase) $
        throwError "Cannot add components to a finalized spaceship."

    let cost = slotCost comp
    if usedSlots s + cost > 10
        then throwError $ "[B-307] Not enough slots. Required: " ++ show cost ++ ", Available: " ++ show (10 - usedSlots s)
        else do
            put $ s { -- record update
                components = comp : components s, -- : is adding a new component to the list 
                usedSlots = usedSlots s + cost, -- accumulate used slots
                -- [B-209]
                reactorCount = reactorCount s + if isReactor comp then 1 else 0, 
                engineCount = engineCount s + if isEngine comp then 1 else 0,
                lifeSupportCount = lifeSupportCount s + if isLifeSupport comp then 1 else 0,
                bridgeCount = bridgeCount s + if isBridge comp then 1 else 0
            }


isReactor (Reactor _) = True; isReactor _ = False
isEngine (Engine _) = True; isEngine _ = False
isLifeSupport :: Component -> Bool
isLifeSupport (LifeSupport _) = True; isLifeSupport _ = False
isBridge (Bridge _) = True; isBridge _ = False


-- [A-103]
start_blueprint :: String -> ShipBuilder ()
start_blueprint name = do
    s <- get
    unless (currentPhase s == InitPhase) $
        throwError "[A-103] start_blueprint can only be as very first operation."
    put $ s { shipName = name, currentPhase = NotFramePhase } -- transfer Spaceship to next phase


set_frame :: ShipBuilder ()
set_frame = do
    s <- get
    unless (currentPhase s == NotFramePhase) $
        throwError "[A-201] set_frame must immediately follow start_blueprint."
    put $ s { currentPhase = CorePhase }

add_reactor :: ReactorType -> ShipBuilder ()
add_reactor r = do
    s <- get
    unless (currentPhase s == CorePhase) $
        throwError "[A-305] Core Modules (Reactor) can only be added before locking core systems."
    addComponent (Reactor r)

add_engine :: EngineType -> ShipBuilder ()
add_engine e = do
    s <- get
    unless (currentPhase s == CorePhase) $
        throwError "[A-305] Core Modules (Engine) can only be added before locking core systems."
    addComponent (Engine e)

add_life_support :: LifeSupportType -> ShipBuilder ()
add_life_support l = do
    s <- get
    unless (currentPhase s == CorePhase) $
        throwError "[A-305] Core Modules (Life Support) can only be added before locking core systems."
    addComponent (LifeSupport l)

add_bridge :: BridgeType -> ShipBuilder ()
add_bridge b = do
    s <- get
    unless (currentPhase s == CorePhase) $
        throwError "[A-305] Core Modules (Bridge) can only be added before locking core systems."
    addComponent (Bridge b)

lock_core_systems :: ShipBuilder ()
lock_core_systems = do
    s <- get
    unless (currentPhase s == CorePhase) $
        throwError "Cannot lock core systems: Not in core phase or already locked."

    let valid = reactorCount s > 0 &&
                engineCount s > 0 &&
                lifeSupportCount s > 0 &&
                bridgeCount s > 0
    unless valid $
        throwError "[B-209] Core System Integrity Failed: Must have at least 1 Reactor, Engine, LifeSupport, and Bridge."
    put $ s { currentPhase = OptionalPhase }

add_shield :: ShieldType -> ShipBuilder ()
add_shield sh = do
    s <- get
    unless (currentPhase s == OptionalPhase) $
        throwError "[A-305] Optional Modules (Shield) can only be added AFTER locking core systems."

    let rTypes = [r | Reactor r <- components s] 
    when (sh == Phase && Fusion `elem` rTypes) $
        throwError "[B-440] Phase Shields are incompatible with Fusion Reactors."
    when (sh == Magnetic && Antimatter `elem` rTypes) $
        throwError "[B-440] Dependency Error: Cannot install Magnetic Shield with Antimatter Reactor."
    addComponent (Shield sh)

add_sensor :: SensorType -> ShipBuilder ()
add_sensor se = do
    s <- get
    unless (currentPhase s == OptionalPhase) $
        throwError "[A-305] Optional Modules (Sensor) can only be added AFTER locking core systems."
    addComponent (Sensor se)

finalize_blueprint :: ShipBuilder Spaceship
finalize_blueprint = do
    s <- get
    put $ s { currentPhase = FinalizedPhase }
    return s



-- TASK 2 - Reporting

-- ==========================================
-- TASK 2: VISUALIZATION (Cập nhật mới)
-- ==========================================

print_spec :: Either String Spaceship -> IO ()
print_spec (Left err) = do
    putStrLn "\n/!\\ CRITICAL ERROR /!\\"
    putStrLn "+--------------------------------------------------+"
    putStrLn $ "| REASON: " ++ err
    putStrLn "+--------------------------------------------------+\n"

print_spec (Right ship) = do
    -- 1. Assemble data
    let comps = components ship

    -- Caculate Mass (Frame = 1000 + Components)
    let frameMass = 1000
    let totalMass = frameMass + sum (map componentMass comps)

    -- Caculate Power
    let totalPowerGen = sum (map powerOutput comps)
    let totalPowerDraw = sum (map powerDraw comps)
    let powerBalance = totalPowerGen - totalPowerDraw

    -- Caculate Thrust & TWR
    let totalThrust = sum (map thrust comps)
    -- Note: Convert Int to Double for division
    let twr = fromIntegral totalThrust / fromIntegral totalMass :: Double

    -- Caculate Slot visualization
    let uSlots = usedSlots ship
    let tSlots = 10 -- Frame luôn có 10 slots
    let slotBar = replicate uSlots '|' ++ replicate (tSlots - uSlots) '.'

    -- 2. Display
    putStrLn ""
    putStrLn "+--------------------------------------------------+"
    putStrLn "|               SPACESHIP BLUEPRINT                |"
    putStrLn "+--------------------------------------------------+"
    printf   "| NAME  : %-15s                          |\n" (shipName ship)
    printf   "| STATUS: %-15s                          |\n" (show (currentPhase ship))
    putStrLn "+--------------------------------------------------+"

    putStrLn "| [ENERGY SYSTEM]                                  |"
    printf   "| Generation  : %5d MW                           |\n" totalPowerGen
    printf   "| Consumption : %5d MW                           |\n" totalPowerDraw


    let balanceSign = if powerBalance >= 0 then "+" else ""
    let statusText = if powerBalance >= 0 then "[OK]" else "[OVERLOAD]"
    printf   "| BALANCE     : %s%d MW %-12s               |\n" balanceSign powerBalance statusText

    putStrLn "|                                                  |"
    putStrLn "| [PERFORMANCE]                                    |"
    printf   "| Total Mass  : %5d tons                         |\n" totalMass
    printf   "| Total Thrust: %5d kN                           |\n" totalThrust
    printf   "| TWR Ratio   : %5.2f                              |\n" twr

    putStrLn "|                                                  |"
    putStrLn "| [CAPACITY]                                       |"
    printf   "| Slots Used  : [%s] %2d/%d                 |\n" slotBar uSlots tSlots

    putStrLn "+--------------------------------------------------+"
    putStrLn "| INSTALLED MODULES:                               |"
    if null comps
        then putStrLn "| (None)                                           |"
        else mapM_ (printf "| - %-47s|\n" . show) (reverse comps)
    putStrLn "+--------------------------------------------------+\n"





-- ==========================================
-- 5. RUN EXAMPLES
-- ==========================================

-- Scenorio 1: Valid
designAlpha :: ShipBuilder Spaceship
designAlpha = do
    start_blueprint "USS Enterprise"
    set_frame
    
    -- Core Phase
    add_reactor Fusion        -- 3 slots
    add_engine Plasma         -- 2 slots
    add_life_support Standard -- 2 slots
    add_bridge Explorer       -- 1 slot
                              -- Total: 8/10
    lock_core_systems
    
    -- Optional Phase
    add_shield Magnetic       -- 1 slot (Fusion + Magnetic = OK)
                              -- Total: 9/10
    finalize_blueprint

-- Scenorio 2: Invalid
designBeta :: ShipBuilder Spaceship
designBeta = do
    start_blueprint "Fail Ship"
    set_frame
    add_reactor Fusion
    add_engine Ion
    add_life_support Standard
    add_bridge Command
    lock_core_systems
    
    add_shield Phase -- Error: Fusion is incompatible with Phase Shield
    finalize_blueprint

main :: IO ()
main = do
    putStrLn "Testing Design Alpha (Should Succeed):"
    -- runExceptT return (Either String a, Spaceship)
    -- evalState to run State monad with initial value
    let result1 = evalState (runExceptT designAlpha) initialSpaceship
    print_spec result1

    putStrLn "\nTesting Design Beta (Should Fail B-440):"
    let result2 = evalState (runExceptT designBeta) initialSpaceship
    print_spec result2
