{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{- HLINT ignore "Use camelCase" -}

module Main where

import System.IO (isEOF, hSetBuffering, stdout, BufferMode(NoBuffering))
import Data.List (isPrefixOf, isInfixOf, sortOn, intercalate, find)

import Control.Monad.State (evalState)
import Control.Monad.Except (runExceptT)

-- import from BasicRequirements.hs
import qualified BasicRequirements as DSL 

-- ==========================
-- 1. DATA TYPES (RUNTIME)
-- ==========================

data Priority = P1 | P2 | P3 | P4 | P5 | P6
  deriving (Show, Eq, Ord)

data ComponentType
  = Reactor              
  | Engine               
  | Shield               
  | Sensors              
  | LifeSupport          
  | Bridge               
  deriving (Show, Eq)

data ComponentStatus 
  = Online 
  | Offline 
  | OfflinePowerDenied 
  | Damaged
  | HighThrustActive 
  | ShieldEmergencyActive
  deriving (Show, Eq)

data Component = Component
  { cId       :: String
  , cType     :: ComponentType
  , cPriority :: Priority
  , cStatus   :: ComponentStatus
  } deriving (Show, Eq)

data SpaceshipState = SpaceshipState
  { tickCount    :: Int
  , components   :: [Component]
  , totalHeat    :: Double
  , alive        :: Bool
  , coolerActive :: Bool
  } deriving (Show, Eq)

data ExternalEvent = ShieldHit | EngineFullThrust
  deriving (Show, Eq)

data PowerRequest = PowerRequest
  { prId :: String
  , prPriority :: Priority
  , prAmount :: Int
  , prKind :: String 
  } deriving (Show, Eq)


-- ==========================
-- 2. INTEGRATION: CONVERTER (FIXED)
-- ==========================

convertShipToState :: DSL.Spaceship -> SpaceshipState
convertShipToState dslShip = 
    let 
        dslComps = DSL.components dslShip 
        
        -- FIX LỖI 1: Tự động xác định Type thay vì gọi DSL.componentType
        convertComp :: DSL.Component -> Int -> Component
        convertComp c idx = 
            let (cType, prefix) = case c of
                    DSL.Reactor _     -> (Reactor, "Reactor")
                    DSL.LifeSupport _ -> (LifeSupport, "LifeSup")
                    DSL.Shield _      -> (Shield, "Shield")
                    DSL.Bridge _      -> (Bridge, "Bridge")
                    DSL.Engine _      -> (Engine, "Engine")
                    DSL.Sensor _      -> (Sensors, "Sensors")
                
                prio = case cType of
                    Reactor     -> P1
                    LifeSupport -> P1
                    Shield      -> P2
                    Bridge      -> P4
                    Engine      -> P5
                    Sensors     -> P6

                status = if cType == Reactor then Online else Offline
            in Component 
                { cId = prefix ++ show idx -- VD: Engine1
                , cType = cType
                , cPriority = prio
                , cStatus = status
                }

        runtimeComps = zipWith convertComp dslComps [1..]
        
    in SpaceshipState
       { tickCount = 0
       , components = runtimeComps
       , totalHeat = 0
       , alive = True
       , coolerActive = False
       }

-- ==========================
-- 3. TICK & LOGIC (GIỮ NGUYÊN)
-- ==========================

tick' :: SpaceshipState -> String -> (SpaceshipState, String, String)
tick' state jsonEvents =
  case alive state of
    False -> (state, "[]", "[]")
    True ->
      let events = parseEvents jsonEvents
          (newState, logs, alerts) = runSimulationLogic state events
      in ( newState, encodeStringList logs, encodeStringList alerts )

-- Logic tính toán và chuyển trạng thái
runSimulationLogic :: SpaceshipState -> [ExternalEvent] -> (SpaceshipState, [String], [String])
runSimulationLogic st@SpaceshipState{..} evts =
  let
    -- Phase 1
    (phase1Reqs, phase1Logs) = generatePhase1Requests components evts

    -- Phase 2
    numReactors = length [() | c@Component{cType=Reactor} <- components]
    predictedHeatDelta = computePredictedHeatDelta numReactors phase1Reqs
    predictedHeat = totalHeat + predictedHeatDelta

    coolingReqs = generateCoolingRequests components predictedHeat
    phase2Logs = if null coolingReqs then [] 
                 else ["PredictedHeat " ++ show (round predictedHeat) ++ " >50 -> LifeSupport requests Cooling(150)"]

    -- Phase 3
    allReqs = phase1Reqs ++ coolingReqs
    totalPowerOut = numReactors * 1000

    (granted, denied, arbLogs, arbAlerts, updatedComps, coolerGranted) = 
      arbitratePower components allReqs totalPowerOut

    coolerTransitionLogs = 
      case (coolerActive, coolerGranted) of
        (False, True)  -> ["CoolingEngaged"]
        (True, False)  -> ["CoolingDisengaged"]
        _              -> []

    -- Phase 4
    actualHeatDelta = computeActualHeatDelta numReactors granted
    newHeat = max 0 (totalHeat + actualHeatDelta)
    isOverheat = newHeat >= 90.0

    lifeSupportFailed = any (\c -> isLifeSupport (cType c) && (cStatus c == OfflinePowerDenied || cStatus c == Offline)) updatedComps
    isAlive = alive && not isOverheat && not lifeSupportFailed

    finalLogs = phase1Logs ++ phase2Logs ++ coolerTransitionLogs ++ arbLogs
    finalAlerts = arbAlerts ++ (if isOverheat then ["Overheat"] else []) ++ (if not isAlive then ["SHIP_DESTROYED"] else [])

    finalState = st 
      { tickCount = tickCount + 1
      , components = if isAlive then updatedComps else components
      , totalHeat = newHeat
      , alive = isAlive
      , coolerActive = coolerGranted && isAlive
      }
  in (finalState, finalLogs, finalAlerts)

-- ==========================
-- HELPERS (GIỮ NGUYÊN NHƯNG QUAN TRỌNG)
-- ==========================

generatePhase1Requests :: [Component] -> [ExternalEvent] -> ([PowerRequest], [String])
generatePhase1Requests comps evts =
  let hasShieldHit = ShieldHit `elem` evts
      hasEngineThrust = EngineFullThrust `elem` evts
      mkReq c@Component{..} = case cType of
          Reactor -> Nothing
          LifeSupport -> Just (PowerRequest cId P1 50 "Standard")
          Engine -> if hasEngineThrust then Just (PowerRequest cId P5 500 "HighThrust") else Just (PowerRequest cId P5 250 "Standard")
          Shield -> if hasShieldHit then Just (PowerRequest cId P2 300 "Emergency") else Just (PowerRequest cId P2 100 "Standard")
          Bridge -> Just (PowerRequest cId P4 75 "Standard")
          Sensors -> Just (PowerRequest cId P6 50 "Standard")
      logs = (if hasShieldHit then ["ShieldHit"] else []) ++ (if hasEngineThrust then ["EngineFullThrust"] else [])
      mapMaybeReq f = foldr (\x acc -> case f x of Just y -> y:acc; Nothing -> acc) []
  in (mapMaybeReq mkReq comps, logs)

generateCoolingRequests comps predictedHeat = if predictedHeat > 50 then [ PowerRequest (cId c) P3 150 "Cooling" | c <- comps, isLifeSupport (cType c) ] else []

computePredictedHeatDelta numReactors reqs = (4.0 * fromIntegral numReactors) + sum [ contributionFromReq r | r <- reqs ] - (2.0 * fromIntegral numReactors)
computeActualHeatDelta numReactors granted = (4.0 * fromIntegral numReactors) + sum [ contributionFromReq r | r <- granted ] - (2.0 * fromIntegral numReactors) - (if any (\r -> prKind r == "Cooling") granted then 10.0 else 0.0)

contributionFromReq PowerRequest{..} = case prKind of "HighThrust" -> 8.0; "Emergency" -> 10.0; _ -> 0.0

arbitratePower comps reqs totalPower =
  let prioWeight P1 = 1; prioWeight P2 = 2; prioWeight P3 = 3; prioWeight P4 = 4; prioWeight P5 = 5; prioWeight P6 = 6
      sorted = sortOn (prioWeight . prPriority) reqs
      process avail (r:rs) granted denied logs alerts compsAcc coolerG =
        if prAmount r <= avail
           then process (avail - prAmount r) rs (granted ++ [r]) denied (logs ++ ["PowerGranted(" ++ prId r ++ ", " ++ show (prAmount r) ++ ")"]) alerts (setComponentOnGrant compsAcc r) (coolerG || (prKind r == "Cooling"))
           else 
             let (alerts', logs') = if prKind r == "Standard" && any (\c -> cId c == prId r && isLifeSupport (cType c)) compsAcc then (alerts ++ ["PowerDenied(LifeSupport)"], logs) 
                                    else if prKind r == "Emergency" && any (\c -> cId c == prId r && isShield (cType c)) compsAcc then (alerts ++ ["PowerDenied(Shields)"], logs)      
                                    else if prKind r == "HighThrust" && any (\c -> cId c == prId r && isEngine (cType c)) compsAcc then (alerts ++ ["EngineThrustFailure"], logs)
                                    else (alerts, logs ++ ["PowerDenied(" ++ prId r ++ ")"])
             in process avail rs granted (denied ++ [r]) logs' alerts' (setComponentOnDenied compsAcc r) coolerG
      process _ [] granted denied logs alerts compsAcc coolerG = (granted, denied, logs, alerts, compsAcc, coolerG)
  in process totalPower sorted [] [] [] [] comps False

setComponentOnGrant cs req = map (\c -> if cId c == prId req then (if prKind req == "Cooling" then c { cStatus = Online } else if prKind req == "HighThrust" then c { cStatus = HighThrustActive } else if prKind req == "Emergency" then c { cStatus = ShieldEmergencyActive } else c { cStatus = Online }) else c) cs
setComponentOnDenied cs req = map (\c -> if cId c == prId req then (if prKind req == "Cooling" then c else c { cStatus = OfflinePowerDenied }) else c) cs

isLifeSupport LifeSupport = True; isLifeSupport _ = False
isEngine Engine = True; isEngine _ = False
isShield Shield = True; isShield _ = False
isBridge Bridge = True; isBridge _ = False
isSensors Sensors = True; isSensors _ = False

parseEvents :: String -> [ExternalEvent]
parseEvents s =
  let shield = "\"ShieldHit\"" `isInfixOf` s || "ShieldHit" `isInfixOf` s
      thrust = "\"EngineFullThrust\"" `isInfixOf` s || "EngineFullThrust" `isInfixOf` s
  in case (shield, thrust) of (True, True) -> [ShieldHit, EngineFullThrust]; (True, False) -> [ShieldHit]; (False, True) -> [EngineFullThrust]; _ -> []

encodeStringList list = "[" ++ intercalate ", " (map show list) ++ "]"

state_to_string :: SpaceshipState -> String
state_to_string SpaceshipState{..} =
  let numReactors = length [() | c@Component{cType=Reactor} <- components]
      totalOut = numReactors * 1000
      totalDraw = sum [ drawForSnapshot c coolerActive | c <- components ]
      findComp p = filter p components
      lifeStatus = case findComp (isLifeSupport . cType) of (c:_) -> stJson (cStatus c); [] -> "\"UNAVAILABLE\""
      coolerStatus = if any (isLifeSupport . cType) components then (if coolerActive then "\"ACTIVE\"" else "\"INACTIVE\"") else "\"UNAVAILABLE\""
      engineStatus = case findComp (isEngine . cType) of (c:_) -> engJson c; [] -> "\"UNAVAILABLE\""
      bridgeStatus = case findComp (isBridge . cType) of (c:_) -> stJson (cStatus c); [] -> "\"UNAVAILABLE\""
      shieldStatus = case findComp (isShield . cType) of (c:_) -> shiJson c; [] -> "\"UNAVAILABLE\""
      sensorsStatus = case findComp (isSensors . cType) of (c:_) -> stJson (cStatus c); [] -> "\"UNAVAILABLE\""
  in "{\n  \"total_power_draw\": " ++ show totalDraw ++ ",\n  \"total_power_output\": " ++ show totalOut ++ ",\n  \"heat\": " ++ show (round totalHeat) ++ ",\n  \"life_support_status\": " ++ lifeStatus ++ ",\n  \"cooler_status\": " ++ coolerStatus ++ ",\n  \"engine_status\": " ++ engineStatus ++ ",\n  \"bridge_status\": " ++ bridgeStatus ++ ",\n  \"shield_status\": " ++ shieldStatus ++ ",\n  \"sensors_status\": " ++ sensorsStatus ++ "\n}"

stJson Offline = "\"OFFLINE\""; stJson OfflinePowerDenied = "\"OFFLINE_POWER_DENIED\""; stJson _ = "\"ONLINE\""
engJson Component{..} = case cStatus of Offline -> "\"OFFLINE\""; OfflinePowerDenied -> "\"OFFLINE_POWER_DENIED\""; HighThrustActive -> "\"HIGH_THRUST\""; _ -> "\"ONLINE\""
shiJson Component{..} = case cStatus of Offline -> "\"OFFLINE\""; OfflinePowerDenied -> "\"OFFLINE_POWER_DENIED\""; ShieldEmergencyActive -> "\"EMERGENCY\""; _ -> "\"ONLINE\""
drawForSnapshot Component{..} coolerOn = case cType of Reactor -> 0; LifeSupport -> (if cStatus `elem` [Offline, OfflinePowerDenied] then 0 else if coolerOn then 200 else 50); Engine -> (if cStatus == HighThrustActive then 500 else if cStatus `elem` [Offline, OfflinePowerDenied] then 0 else 250); Shield -> (if cStatus == ShieldEmergencyActive then 300 else if cStatus `elem` [Offline, OfflinePowerDenied] then 0 else 100); Bridge -> (if cStatus `elem` [Offline, OfflinePowerDenied] then 0 else 75); Sensors -> (if cStatus `elem` [Offline, OfflinePowerDenied] then 0 else 50)


-- ==========================
-- 4. MAIN DRIVER (FIXED LỖI 2&3)
-- ==========================

main :: IO ()
main = do
    hSetBuffering stdout NoBuffering
    
    -- FIX: Gọi runExceptT và evalState trực tiếp (không qua DSL.)
    let dslResult = evalState (runExceptT DSL.designAlpha) DSL.initialSpaceship
    
    case dslResult of
        Left err -> putStrLn $ "Error: " ++ err
        Right dslShip -> do
            let st0 = convertShipToState dslShip
            loop st0

loop :: SpaceshipState -> IO ()
loop state = do
    done <- isEOF
    if done then return ()
    else do
        cmd <- getLine
        processCommand state cmd

processCommand :: SpaceshipState -> String -> IO ()
processCommand state cmd
    | cmd == "print_state" = do
        putStrLn (state_to_string state)
        loop state
    | "tick " `isPrefixOf` cmd = do
        let jsonEvents = drop 5 cmd
        let (newState, logs, alerts) = tick' state jsonEvents
        putStrLn logs
        putStrLn alerts
        putStrLn (state_to_string newState)
        loop newState
    | otherwise = loop state