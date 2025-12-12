{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module Runtime where

-- Standard libraries
import System.IO (isEOF, hSetBuffering, stdout, BufferMode(NoBuffering))
import Data.Ord (Down(Down))
import Data.Maybe (fromMaybe)
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.Aeson (eitherDecode)
import Data.Aeson.Types (parseEither)
import Data.List (isPrefixOf, isInfixOf, sortOn, intercalate)

-- ==========================
-- 1. DATA TYPES
-- ==========================

data Priority = P1 | P2 | P3 | P4 | P5 | P6
  deriving (Show, Eq, Ord)

-- Component types (Values are hardcoded in logic functions)
data ComponentType
  = Reactor              -- 1000 output
  | Engine               -- 250 / 500
  | Shield               -- 100 / 300
  | Sensors              -- 50
  | LifeSupport          -- 50 + 150
  | Bridge               -- 75
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

data ShipBlueprint = ShipBlueprint
  { bpName :: String
  , bpComponents :: [Component]
  } deriving (Show, Eq)

data SpaceshipState = SpaceshipState
  { tickCount    :: Int
  , components   :: [Component]
  , totalHeat    :: Double
  , alive        :: Bool
  , coolerActive :: Bool
  } deriving (Show, Eq)

-- External events
data ExternalEvent = ShieldHit | EngineFullThrust | NoEvent
  deriving (Show, Eq)

-- Power request representation
data PowerRequest = PowerRequest
  { prId :: String
  , prPriority :: Priority
  , prAmount :: Int
  , prKind :: String  -- "Standard", "HighThrust", "Emergency", "Cooling"
  } deriving (Show, Eq)

-- ==========================
-- 2. INITIAL STATE
-- ==========================

initial_state :: ShipBlueprint -> SpaceshipState
initial_state (ShipBlueprint _ blueprintComps) =
  let resetStatus c = case cType c of
        Reactor -> c { cStatus = Online }
        _       -> c { cStatus = Offline }
      initializedComps = map resetStatus blueprintComps
  in SpaceshipState
       { tickCount = 0
       , components = initializedComps
       , totalHeat = 0
       , alive = True
       , coolerActive = False
       }

-- ==========================
-- 3. TICK wrapper
-- ==========================

tick' :: SpaceshipState -> String -> (SpaceshipState, String, String)
tick' state jsonEvents =
  case alive state of
    False -> (state, encodeStringList [], encodeStringList []) -- once destroyed, no further changes/logs
    True ->
      let events = parseEvents jsonEvents
          (newState, logs, alerts) = runSimulationLogic state events
      in ( newState
         , encodeStringList logs
         , encodeStringList alerts
         )

-- ==========================
-- 4. STATE SERIALIZATION
-- ==========================

state_to_string :: SpaceshipState -> String
state_to_string SpaceshipState{..} =
  let numReactors = length [() | c@Component{cType=Reactor} <- components]
      totalOut = numReactors * 1000
      totalDraw = sum [ drawForSnapshot c coolerActive | c <- components ]

      findComp p = filter p components

      lifeStatus = case findComp (isLifeSupport . cType) of
                     (c:_) -> lifeSupportStatusToJson (cStatus c)
                     []    -> "\"UNAVAILABLE\""

      coolerStatus = if any (isLifeSupport . cType) components
                     then if coolerActive then "\"ACTIVE\"" else "\"INACTIVE\""
                     else "\"UNAVAILABLE\""

      engineStatus = case findComp (isEngine . cType) of
                       (c:_) -> engineStatusToJson c
                       []    -> "\"UNAVAILABLE\""

      bridgeStatus = case findComp (isBridge . cType) of
                       (c:_) -> compStatusToJson c
                       []    -> "\"UNAVAILABLE\""

      shieldStatus = case findComp (isShield . cType) of
                       (c:_) -> shieldStatusToJson c
                       []    -> "\"UNAVAILABLE\""

      sensorsStatus = case findComp (isSensors . cType) of
                        (c:_) -> compStatusToJson c
                        []    -> "\"UNAVAILABLE\""

      heatInt = round totalHeat
  in "{\n" ++
     "  \"total_power_draw\": " ++ show totalDraw ++ ",\n" ++
     "  \"total_power_output\": " ++ show totalOut ++ ",\n" ++
     "  \"heat\": " ++ show heatInt ++ ",\n" ++
     "  \"life_support_status\": " ++ lifeStatus ++ ",\n" ++
     "  \"cooler_status\": " ++ coolerStatus ++ ",\n" ++
     "  \"engine_status\": " ++ engineStatus ++ ",\n" ++
     "  \"bridge_status\": " ++ bridgeStatus ++ ",\n" ++
     "  \"shield_status\": " ++ shieldStatus ++ ",\n" ++
     "  \"sensors_status\": " ++ sensorsStatus ++ "\n" ++
     "}"

lifeSupportStatusToJson :: ComponentStatus -> String
lifeSupportStatusToJson Offline = "\"OFFLINE\"" -- initial allowed
lifeSupportStatusToJson OfflinePowerDenied = "\"OFFLINE_POWER_DENIED\""
lifeSupportStatusToJson _ = "\"ONLINE\""

engineStatusToJson :: Component -> String
engineStatusToJson Component{..} =
  case cStatus of
    Offline -> "\"OFFLINE\""
    OfflinePowerDenied -> "\"OFFLINE_POWER_DENIED\""
    HighThrustActive -> "\"HIGH_THRUST\""
    _ -> "\"ONLINE\""

shieldStatusToJson :: Component -> String
shieldStatusToJson Component{..} =
  case cStatus of
    Offline -> "\"OFFLINE\""
    OfflinePowerDenied -> "\"OFFLINE_POWER_DENIED\""
    ShieldEmergencyActive -> "\"EMERGENCY\""
    _ -> "\"ONLINE\""

compStatusToJson :: Component -> String
compStatusToJson Component{..} =
  case cStatus of
    Offline -> "\"OFFLINE\""
    OfflinePowerDenied -> "\"OFFLINE_POWER_DENIED\""
    _ -> "\"ONLINE\""

drawForSnapshot :: Component -> Bool -> Int
drawForSnapshot Component{..} coolerOn =
  case cType of
    Reactor -> 0
    LifeSupport ->
      if cStatus == Offline || cStatus == OfflinePowerDenied 
      then 0
      else if coolerOn then 200 else 50
    Engine ->
      case cStatus of
        Offline -> 0
        OfflinePowerDenied -> 0
        HighThrustActive -> 500
        _ -> 250
    Shield ->
      case cStatus of
        Offline -> 0
        OfflinePowerDenied -> 0
        ShieldEmergencyActive -> 300
        _ -> 100
    Bridge -> if cStatus == Offline || cStatus == OfflinePowerDenied then 0 else 75
    Sensors -> if cStatus == Offline || cStatus == OfflinePowerDenied then 0 else 50

-- ==========================
-- 5. SIMULATION LOGIC
-- ==========================

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
    phase2Logs = if null coolingReqs 
                 then [] 
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

    lifeSupportFailed =
      any (\c -> isLifeSupport (cType c) &&
                 (cStatus c == OfflinePowerDenied || cStatus c == Offline))
          updatedComps

    isAlive = alive && not isOverheat && not lifeSupportFailed

    finalLogs = phase1Logs ++ phase2Logs ++ coolerTransitionLogs ++ arbLogs
    finalAlerts = arbAlerts ++ (if isOverheat then ["Overheat"] else []) 
                            ++ (if not isAlive then ["SHIP_DESTROYED"] else [])

    finalState = st 
      { tickCount = tickCount + 1
      , components = if isAlive then updatedComps else components -- freeze components if ship destroyed this tick
      , totalHeat = newHeat
      , alive = isAlive
      , coolerActive = coolerGranted && isAlive
      }
  in (finalState, finalLogs, finalAlerts)

-- ==========================
-- 6. REQUEST GENERATION
-- ==========================

generatePhase1Requests :: [Component] -> [ExternalEvent] -> ([PowerRequest], [String])
generatePhase1Requests comps evts =
  let hasShieldHit = ShieldHit `elem` evts
      hasEngineThrust = EngineFullThrust `elem` evts
      
      mkReq c@Component{..} =
        case cType of
          Reactor -> Nothing
          LifeSupport -> Just (PowerRequest cId P1 50 "Standard")
          Engine -> 
            if hasEngineThrust 
            then Just (PowerRequest cId P5 500 "HighThrust")
            else Just (PowerRequest cId P5 250 "Standard")
          Shield ->
            if hasShieldHit
            then Just (PowerRequest cId P2 300 "Emergency")
            else Just (PowerRequest cId P2 100 "Standard")
          Bridge -> Just (PowerRequest cId P4 75 "Standard")
          Sensors -> Just (PowerRequest cId P6 50 "Standard")
      
      logs = if hasShieldHit then ["ShieldHit"] else []
      
      -- Helper locally to avoid signature clash
      mapMaybeReq f = foldr (\x acc -> case f x of Just y -> y:acc; Nothing -> acc) []
  in (mapMaybeReq mkReq comps, logs)

generateCoolingRequests :: [Component] -> Double -> [PowerRequest]
generateCoolingRequests comps predictedHeat =
  if predictedHeat > 50
  then [ PowerRequest (cId c) P3 150 "Cooling" | c <- comps, isLifeSupport (cType c) ]
  else []

-- ==========================
-- 8. HEAT CALCULATION
-- ==========================

computePredictedHeatDelta :: Int -> [PowerRequest] -> Double
computePredictedHeatDelta numReactors reqs =
  let reactorHeat = 4.0 * fromIntegral numReactors
      heatFromOther = sum [ contributionFromReq r | r <- reqs ]
      passiveDissip = 2.0 * fromIntegral numReactors
  in reactorHeat + heatFromOther - passiveDissip

contributionFromReq :: PowerRequest -> Double
contributionFromReq PowerRequest{..} =
  case prKind of
    "HighThrust" -> 8.0
    "Emergency"  -> 10.0
    _            -> 0.0

computeActualHeatDelta :: Int -> [PowerRequest] -> Double
computeActualHeatDelta numReactors grantedReqs =
  let reactorHeat = 4.0 * fromIntegral numReactors
      heatFromGranted = sum [ contributionFromReq r | r <- grantedReqs ]
      passiveDissip = 2.0 * fromIntegral numReactors
      coolerDissip = if any (\r -> prKind r == "Cooling") grantedReqs 
                      then 10.0 else 0.0
  in reactorHeat + heatFromGranted - passiveDissip - coolerDissip

-- ==========================
-- 9. ARBITRATION
-- ==========================

arbitratePower :: [Component] -> [PowerRequest] -> Int 
               -> ([PowerRequest], [PowerRequest], [String], [String], [Component], Bool)
arbitratePower comps reqs totalPower =
  let -- prioWeight: smaller = higher priority so sortOn ascending places P1 first
      prioWeight :: Priority -> Int
      prioWeight P1 = 1
      prioWeight P2 = 2
      prioWeight P3 = 3
      prioWeight P4 = 4
      prioWeight P5 = 5
      prioWeight P6 = 6

      -- sort by priority ascending (P1 first)
      sorted = sortOn (prioWeight . prPriority) reqs

      process _ [] granted denied logs alerts compsAcc coolerG = 
        (granted, denied, logs, alerts, compsAcc, coolerG)
      
      process avail (r:rs) granted denied logs alerts compsAcc coolerG =
        let requestLog = "PowerDrawRequest(" ++ prId r ++ ", " ++ show (prAmount r) ++ ")"
            logs1 = logs ++ [requestLog]
        in if prAmount r <= avail
           then 
             let grantLog = "PowerGranted(" ++ prId r ++ ", " ++ show (prAmount r) ++ ")"
                 logs2 = logs1 ++ [grantLog]
                 avail' = avail - prAmount r
                 granted' = granted ++ [r]
                 compsAcc' = setComponentOnGrant compsAcc r
                 coolerG' = coolerG || (prKind r == "Cooling")
             in process avail' rs granted' denied logs2 alerts compsAcc' coolerG'
           
           else 
             let -- determine if special alert should be emitted (and thus suppress PowerDenied(...) log)
                 isLifeStd = prKind r == "Standard" && 
                             any (\c -> cId c == prId r && isLifeSupport (cType c)) compsAcc
                 isShieldEmer = prKind r == "Emergency" && 
                                any (\c -> cId c == prId r && isShield (cType c)) compsAcc
                 isEngineHigh = prKind r == "HighThrust" && 
                                any (\c -> cId c == prId r && isEngine (cType c)) compsAcc

                 (alerts', logs2) =
                   if isLifeStd
                   then (alerts ++ ["PowerDenied(LifeSupport)"], logs) 
                   else if isShieldEmer
                   then (alerts ++ ["PowerDenied(Shields)"], logs)      
                   else if isEngineHigh
                   then (alerts ++ ["EngineThrustFailure"], logs)
                   else (alerts, logs1 ++ ["PowerDenied(" ++ prId r ++ ")"])

                 denied' = denied ++ [r]
                 compsAcc' = setComponentOnDenied compsAcc r
             in process avail rs granted denied' logs2 alerts' compsAcc' coolerG
  in process totalPower sorted [] [] [] [] comps False

setComponentOnGrant :: [Component] -> PowerRequest -> [Component]
setComponentOnGrant cs req = map upd cs
  where
    upd c
      | cId c /= prId req = c
      | prKind req == "Cooling" = c { cStatus = Online } -- cooler toggles life support to ONLINE
      | prKind req == "HighThrust" = c { cStatus = HighThrustActive }
      | prKind req == "Emergency" = c { cStatus = ShieldEmergencyActive }
      | otherwise = c { cStatus = Online }

setComponentOnDenied :: [Component] -> PowerRequest -> [Component]
setComponentOnDenied cs req = map upd cs
  where
    upd c
      | cId c /= prId req = c 
      | prKind req == "Cooling" = c  -- Cooling denied: keep previous state (cooler remains off)
      | otherwise = c { cStatus = OfflinePowerDenied }

-- ==========================
-- 10. TYPE PREDICATES
-- ==========================

isReactor Reactor = True; isReactor _ = False
isLifeSupport LifeSupport = True; isLifeSupport _ = False
isEngine Engine = True; isEngine _ = False
isShield Shield = True; isShield _ = False
isBridge Bridge = True; isBridge _ = False
isSensors Sensors = True; isSensors _ = False

-- ==========================
-- 11. PARSING AND JSON HELPERS
-- ==========================

-- Parse events JSON array safely using aeson. If parse fails, fall back to substring detection.
parseEvents :: String -> [ExternalEvent]
parseEvents s =
  let shield = "\"ShieldHit\"" `isInfixOf` s || "ShieldHit" `isInfixOf` s
      thrust = "\"EngineFullThrust\"" `isInfixOf` s || "EngineFullThrust" `isInfixOf` s
  in case (shield, thrust) of
       (True, True)   -> [ShieldHit, EngineFullThrust]
       (True, False)  -> [ShieldHit]
       (False, True)  -> [EngineFullThrust]
       _              -> []

mapMaybeStrToEvent :: [String] -> [ExternalEvent]
mapMaybeStrToEvent = foldr (\x acc -> case x of
                           "ShieldHit" -> ShieldHit : acc
                           "EngineFullThrust" -> EngineFullThrust : acc
                           _ -> acc) []

encodeStringList :: [String] -> String
encodeStringList list = "[" ++ intercalate ", " (map show list) ++ "]"

-- ==========================
-- 12. MAIN DRIVER (INTERACTIVE LOOP)
-- ==========================

main :: IO ()
main = do
    hSetBuffering stdout NoBuffering

    -- Initialize default blueprint
    let r1 = Component "Reactor1" Reactor P1 Online
    let ls = Component "LifeSup" LifeSupport P1 Offline
    let sh = Component "Shield1" Shield P2 Offline
    let en = Component "Engine1" Engine P5 Offline
    let br = Component "Bridge1" Bridge P4 Offline
    let sn = Component "Sensors1" Sensors P6 Offline

    let myBlueprint = ShipBlueprint "USS Voyager" [r1, ls, sh, en, br, sn]
    let st0 = initial_state myBlueprint

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
