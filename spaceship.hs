{- HLINT ignore "Use camelCase" -}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PolyKinds #-}

import GHC.TypeLits                 
-- import GHC.TypeLits.Compare         
import Data.Kind (Type, Constraint)

--  Phase
data Phase = Init | Core | Optional | Finalized

-- Reactore type
data ReactorType = Fusion | Antimatter

-- Shield type
data ShieldType  = Magnetic | Phase

-- Status
data Status = Missing | Present

-- Spaceship structure
data Spaceship
    (phase :: Phase)
    (slots :: Nat)
    (reactor :: Status)
    (engine :: Status)
    (lifesupport :: Status)
    (bridge :: Status )
    (installedList :: [ReactorType])
    = Spaceship
    {
        runtimeName :: String,
        runtimeMass :: Int,
        runtimeCopms :: [String] -- components name
    } deriving (Show)

-- [B-440]
type family CheckShield (s :: ShieldType) (rs :: [ReactorType]) :: Constraint where
    CheckShield 'Phase ('Fusion ': xs) = TypeError ('Text "VIOLATION [B-440]: Cannot install Phase Shield with Fusion Reactor")

    CheckShield 'Magnetic ('Antimatter ': xs) = TypeError ('Text "VIOLATION [B-440]: Cannot install Magnetic Shield with Antimatter Reactor")

    CheckShield s (_ ': xs) = CheckShield s xs

    CheckShield _ '[] = ()

-- [A-103] start_blueprint
start_blueprint :: String -> Spaceship 'Init 0 'Missing 'Missing 'Missing 'Missing '[]
start_blueprint name = Spaceship name 0 []

-- [A-103] set_frame
set_frame :: Spaceship 'Init slots reactor engine lifesupport bridge installedList -> Spaceship 'Core slots reactor engine lifesupport bridge installedList
set_frame (Spaceship name mass comps) = Spaceship name (mass+1000) comps

-- [B-305]
add_reactor :: 

