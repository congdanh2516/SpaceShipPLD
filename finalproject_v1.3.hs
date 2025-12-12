{- HLINT ignore "Use camelCase" -}
{- HLINT ignore "Use isAsciiLower" -}
import Control.Monad.State
import Control.Monad.Except
import Data.List (intercalate, find)
import Control.Monad
import Text.Printf (printf)

-- Concepts and Vocabulary
-- Type:
-- core modules:
data ReactorType = Fusion | Antimatter deriving (Show, Eq)
data EngineType = Ion | Plasma deriving (Show, Eq)
data LifeSupportType = Standard | Advanced deriving (Show, Eq)
data BridgeType = Explorer | Command deriving (Show, Eq)
-- optional modules:
data ShieldType = Magnetic | Phase deriving (Show, Eq)
data SensorType = Basic | Enhanced deriving (Show, Eq) -- "Advanced" will be same as LifeSupportType type

-- Component:
data Component = Reactor ReactorType
               | Engine EngineType
               | LifeSupport LifeSupportType
               | Bridge BridgeType
               | Shield ShieldType
               | Sensor SensorType
               deriving (Show, Eq)

-- Phase: based on the rules A, B
data BuildPhase = InitPhase -- not started yet (only start_blueprint allowed)
                | NotFramePhase -- started, haven't frame yet (only set_frame allowed)
                | CorePhase -- install core (only core modules allowed)
                | OptionalPhase -- installed core, installing optional (only optional modules allowed)
                | FinalizedPhase -- finished (no more modules allowed)
                deriving (Show, Eq, Enum)

-- Spaceship type:
data Spaceship = Spaceship {
    shipName :: String,
    currentPhase :: BuildPhase,
    components :: [Component],
    usedSlots :: Int,

    reactorCount :: Int, -- check for lock_core_systems [B-209] ... can only be preformed after at least one of each Core Moulde has been installed
    engineCount :: Int, -- //
    lifeSupportCount :: Int, -- //
    bridgeCount :: Int -- //
} deriving (Show)

-- BUSINESS LOGIC
-- slot_cost is the numbers of slots required to install the module, area that a module occupy in the ship
slotCost :: Component -> Int
slotCost (Reactor _) = 3
slotCost (Engine _) = 2
slotCost (LifeSupport _) = 2
slotCost (Bridge _) = 1
slotCost (Shield _) = 1
slotCost (Sensor _) = 1

-- mass:  weight of each component
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


-- TASK 1 - eDSL and Detection of Safety Requirements

-- initial spaceship
initialSpaceship :: Spaceship
initialSpaceship = Spaceship "" InitPhase [] 0 0 0 0 0

-- ShipBuilder monad !!!
type ShipBuilder a = ExceptT String (State Spaceship) a -- this is the reason why s--> get will get Spaceship info, then assigning to s
-- ExceptT String: error handling with String messages
-- State Spaceship: state monad to manage Spaceship state throughout the building process. get: get current state, put: update state

capitalize :: String -> String
capitalize [] = []
capitalize (x:xs) = toUpper x : xs
  where
    toUpper c = if c >= 'a' && c <= 'z' then toEnum (fromEnum c - 32) else c

getComponentInfo :: Component -> (String, String, String)
getComponentInfo comp = case comp of
    Reactor r     -> ("core", "reactor", show r)
    Engine e      -> ("core", "engine", show e)
    LifeSupport l -> ("core", "lifeSupport", show l)
    Bridge b      -> ("core", "bridge", show b)
    Shield s      -> ("optional", "shield", show s)
    Sensor x      -> ("optional", "sensor", show x)

-- slot check and update component list
addComponent :: Component -> ShipBuilder ()
addComponent comp = do
    s <- get -- get current state of Spaceship

    let (moduleType, categoryName, subTypeName) = getComponentInfo comp

    when (currentPhase s == FinalizedPhase) $
        throwError $ "ORDERING ERROR at line 'add_" ++ categoryName ++ " " ++ subTypeName ++ "': Cannot add components to a finalized spaceship."

    when (currentPhase s == InitPhase) $
        throwError $ "[A-103] ORDERING ERROR at line 'add_" ++ categoryName ++ " " ++ subTypeName ++ "': You need to set start_blueprint first."

    when (currentPhase s == NotFramePhase) $
        throwError $ "[A-201] ORDERING ERROR at line 'add_" ++ categoryName ++ " " ++ subTypeName ++ "': You need to set_frame first."

    when (moduleType == "core" && currentPhase s /= CorePhase) $
        throwError $ "[A-305] ORDERING ERROR at line 'add_" ++ categoryName ++ " " ++ subTypeName ++ "': Core Modules (" ++ capitalize categoryName ++ ") can only be added BEFORE locking core systems."

    when (moduleType == "optional" && currentPhase s /= OptionalPhase) $
         throwError $ "[A-305] ORDERING ERROR at line 'add_" ++ categoryName ++ " " ++ subTypeName ++ "': Optional Modules (" ++ capitalize categoryName ++ ") can only be added AFTER locking core systems."
        
    when (categoryName == "shield") $ do
        let rTypes = [r | Reactor r <- components s] -- Get current Reactor list
        let Shield sh = comp -- Pattern match to get Shield type (safe because categoryName == "shield")

        when (sh == Phase && Fusion `elem` rTypes) $
            throwError "[B-440] DEPENDENCY ERROR at line 'add_shield Phase': Phase Shields are incompatible with Fusion Reactors."
        
        -- Check Magnetic Shield vs Antimatter Reactor
        when (sh == Magnetic && Antimatter `elem` rTypes) $
            throwError "[B-440] DEPENDENCY ERROR at line 'add_shield Magnetic': Cannot install Magnetic Shield with Antimatter Reactor."

    -- slot check
    let cost = slotCost comp
    if usedSlots s + cost > 10
        then throwError $ "[B-307] DEPENDENCY ERROR at line 'add_" ++ categoryName ++ " " ++ subTypeName ++ "': Not enough slots. Required: " ++ show cost ++ ", Available: " ++ show (10 - usedSlots s)
        else do
            put $ s { -- record update
                components = comp : components s, -- : is adding a new component to the list 
                usedSlots = usedSlots s + cost, -- accumulate used slots
                -- support for [B-209]
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
        throwError "[A-103] ORDERING ERROR: start_blueprint can only be as very first operation."
    put $ s { shipName = name, currentPhase = NotFramePhase } -- transfer Spaceship to next phase

set_frame :: ShipBuilder ()
set_frame = do
    s <- get
    unless (currentPhase s == NotFramePhase) $
        throwError "[A-201] ORDERING ERROR: set_frame must immediately follow start_blueprint."
    put $ s { currentPhase = CorePhase }

-- [A-305]
add_reactor :: ReactorType -> ShipBuilder ()
add_reactor r = do
    s <- get
    addComponent (Reactor r)

add_engine :: EngineType -> ShipBuilder ()
add_engine e = do
    addComponent (Engine e)

add_life_support :: LifeSupportType -> ShipBuilder ()
add_life_support l = do
    addComponent (LifeSupport l)

add_bridge :: BridgeType -> ShipBuilder ()
add_bridge b = do
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

add_sensor :: SensorType -> ShipBuilder ()
add_sensor se = do
    addComponent (Sensor se)

-- [A-305], [B-440]
add_shield :: ShieldType -> ShipBuilder ()
add_shield sh = do
   addComponent (Shield sh)

-- [A-212]
finalize_blueprint :: ShipBuilder Spaceship
finalize_blueprint = do
    s <- get
    -- only allow finalize_blueprint once locked core systems
    when (currentPhase s /= CorePhase && currentPhase s /= OptionalPhase) $  -- only finalize once 
        throwError "[A-212] ORDERING ERROR at line 'finalize_blueprint': finalize_blueprint can only be called once at the end of the building process."
    put $ s { currentPhase = FinalizedPhase }
    return s


-- TASK 2: Visualization

print_spec :: Either String Spaceship -> IO ()
print_spec (Left err) = do
    putStrLn "\n[CRITICAL ERROR]\n"
    putStrLn $ "Reason: " ++ err ++ "\n"

print_spec (Right ship) = do
    -- 1. Assemble data
    let comps = components ship

    -- Caculate mass (Frame = 1000 + Components)
    let frameMass = 1000
    let totalMass = frameMass + sum (map componentMass comps)

    -- Caculate power
    let totalPowerGen = sum (map powerOutput comps)
    let totalPowerDraw = sum (map powerDraw comps)
    let powerBalance = totalPowerGen - totalPowerDraw

    -- Caculate thrust & TWR
    let totalThrust = sum (map thrust comps)
    -- Note: Convert Int to Double for division
    let twr = fromIntegral totalThrust / fromIntegral totalMass :: Double

    -- Caculate slot visualization
    let uSlots = usedSlots ship
    let tSlots = 10 -- Frame luôn có 10 slots
    let slotBar = replicate uSlots '|' ++ replicate (tSlots - uSlots) '.'

    -- 2. Display
    putStrLn ""
    putStrLn "[BUILDING SUCCESSFULLY]"
    putStrLn ""
    putStrLn "+--------------------------------------------------+"
    putStrLn "|               SPACESHIP BLUEPRINT                |"
    putStrLn "+--------------------------------------------------+"
    printf   "| NAME    : %-15s                        |\n" (shipName ship)
    printf   "| STATUS  : %-15s                        |\n" (show (currentPhase ship))
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
    printf   "| Slots Used    : [%s] %2d/%d               |\n" slotBar uSlots tSlots

    putStrLn "+--------------------------------------------------+"
    putStrLn "| INSTALLED MODULES:                               |"
    if null comps
        then putStrLn "| (None)                                           |"
        else mapM_ (printf "| - %-47s|\n" . show) (reverse comps)
    putStrLn "+--------------------------------------------------+\n"


--  Test cases

-- Scenorio 1: Valid
designAlpha :: ShipBuilder Spaceship
designAlpha = do
    start_blueprint "USS Enterprise"
    -- set_frame
    
    -- Core Phase
    add_reactor Fusion        -- 3 slots
    add_engine Plasma         -- 2 slots
    add_life_support Standard -- 2 slots
    add_bridge Explorer       -- 1 slot   --> Total: 8/10
    lock_core_systems
    
    -- Optional Phase
    add_shield Magnetic       -- 1 slot (Fusion + Magnetic = OK)
                              -- Total: 9/10
    finalize_blueprint -- actually, we are not able to add more components after this, because this function has "return s"

-- Invalid
-- Scenorio 2: Invalid
designBeta :: ShipBuilder Spaceship
designBeta = do
    finalize_blueprint

-- Scenorio 3 [A-103]
design3 :: ShipBuilder Spaceship
design3 = do
    -- set_frame
    start_blueprint "USS Voyager"

    add_reactor Antimatter

    finalize_blueprint

-- Scenorio 4 [A-305]
design4 :: ShipBuilder Spaceship
design4 = do
    start_blueprint "USS Voyager"
    set_frame
    add_reactor Fusion        -- 3 slots
    add_engine Plasma         -- 2 slots
    add_life_support Standard
    add_bridge Explorer
    add_shield Magnetic

    lock_core_systems

    finalize_blueprint


main :: IO ()
main = do
    putStrLn "Testing Scenorio 1 (Should Succeed):"
    -- runExceptT return (Either String a, Spaceship)
    -- evalState to run State monad with initial value
    let result1 = evalState (runExceptT designAlpha) initialSpaceship
    print_spec result1

    putStrLn "\nTesting Scenorio 2 (Should Fail [B-440]):"
    let result2 = evalState (runExceptT designBeta) initialSpaceship
    print_spec result2

    putStrLn "\nTesting Scenorio 3:"
    let result2 = evalState (runExceptT design3) initialSpaceship
    print_spec result2

    putStrLn "\nTesting Scenorio 4:"
    let result2 = evalState (runExceptT design4) initialSpaceship
    print_spec result2

--TASK 3: Documentation (refer to .pdf file)
