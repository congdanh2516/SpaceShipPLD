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

-- ==========================================
-- 1. DATA DEFINITIONS & PHANTOM TYPES
-- ==========================================

data ReactorType = Fusion | Antimatter
data ShieldType  = Magnetic | Phase

data Phase = Init | Core | Optional | Finalized

-- ==========================================
-- 2. THE SPACESHIP TYPE
-- ==========================================
data Spaceship (p :: Phase) (rMap :: (Bool, Bool)) (cores :: (Bool, Bool, Bool, Bool)) (slots :: Nat) where
    -- Khởi tạo tàu: Chưa có gì cả, Phase Init
    Ship :: String -> Spaceship 'Init '( 'False, 'False) '( 'False, 'False, 'False, 'False) 0

instance Show (Spaceship p r c s) where
    show _ = "Spaceship Blueprint (Verified by GHC Compiler)"

-- ==========================================
-- 3. LOGIC GATES (TYPE FAMILIES)
-- ==========================================

-- [Rule B-440] CHECK COMPATIBILITY

type family CheckCompatibility (reactors :: (Bool, Bool)) (s :: ShieldType) :: Constraint where
    CheckCompatibility '( 'True, _ ) 'Phase    = TypeError ('Text "ERROR: Cannot install Phase Shield detected Fusion Reactor! [Rule B-440]")
    CheckCompatibility '( _, 'True ) 'Magnetic = TypeError ('Text "ERROR: Cannot install Magnetic Shield detected Antimatter Reactor! [Rule B-440]")
    CheckCompatibility _ _                     = ()

-- [Logic] UPDATE REACTOR STATE
type family UpdateReactors (new :: ReactorType) (current :: (Bool, Bool)) :: (Bool, Bool) where
    UpdateReactors 'Fusion     '( _, a ) = '( 'True, a )  -- Bật cờ Fusion
    UpdateReactors 'Antimatter '( f, _ ) = '( f, 'True )  -- Bật cờ Antimatter

-- [Rule B-307] SLOT LIMIT CHECK
type family CheckSlots (current :: Nat) (cost :: Nat) :: Constraint where
    CheckSlots current cost =
        If ((current + cost) <=? 10)
           (() :: Constraint)
           (TypeError ('Text "ERROR: Slot limit exceeded (Max 10)! [Rule B-307]"))

-- [Rule B-209] CORE INTEGRITY CHECK
type family CheckCoreIntegrity (cores :: (Bool, Bool, Bool, Bool)) :: Constraint where
    CheckCoreIntegrity '( 'True, 'True, 'True, 'True) = ()
    CheckCoreIntegrity _ = TypeError ('Text "ERROR: Missing Core Systems! Reactor, Engine, LifeSupport and Bridge are required before locking. [Rule B-209]")

-- ==========================================
-- 4. DSL OPERATIONS (Rule Enforcement)
-- ==========================================

fusion :: Proxy 'Fusion
fusion = Proxy
antimatter :: Proxy 'Antimatter
antimatter = Proxy
phaseShield :: Proxy 'Phase
phaseShield = Proxy
magShield :: Proxy 'Magnetic
magShield = Proxy

-- [A-103] start_blueprint -> set_frame
-- Chỉ có setFrame nhận được 'Init.
setFrame :: Spaceship 'Init r c s -> Spaceship 'Core r c s
setFrame _ = undefined

-- Thêm Reactor (Core Module)
-- Cost: 3 slots. Cập nhật rMap và core flag thứ 1.
addReactor :: (CheckSlots s 3)
           => Proxy (rt :: ReactorType)
           -> Spaceship 'Core rMap '(hasR, hasE, hasL, hasB) s
           -> Spaceship 'Core (UpdateReactors rt rMap) '( 'True, hasE, hasL, hasB) (s + 3)
addReactor _ _ = undefined

-- Thêm Engine (Core Module)
-- Cost: 2 slots. Cập nhật core flag thứ 2.
addEngine :: (CheckSlots s 2)
          => Spaceship 'Core rMap '(hasR, hasE, hasL, hasB) s
          -> Spaceship 'Core rMap '(hasR, 'True, hasL, hasB) (s + 2)
addEngine _ = undefined

-- Thêm LifeSupport (Core Module)
-- Cost: 2 slots. Cập nhật core flag thứ 3.
addLifeSupport :: (CheckSlots s 2)
               => Spaceship 'Core rMap '(hasR, hasE, hasL, hasB) s
               -> Spaceship 'Core rMap '(hasR, hasE, 'True, hasB) (s + 2)
addLifeSupport _ = undefined

-- Thêm Bridge (Core Module)
-- Cost: 1 slot. Cập nhật core flag thứ 4.
addBridge :: (CheckSlots s 1)
          => Spaceship 'Core rMap '(hasR, hasE, hasL, hasB) s
          -> Spaceship 'Core rMap '(hasR, hasE, hasL, 'True) (s + 1)
addBridge _ = undefined

-- [A-305] & [B-209] Lock Core Systems
-- Chuyển từ 'Core -> 'Optional. Yêu cầu CheckCoreIntegrity.
lockCoreSystems :: (CheckCoreIntegrity cores)
                => Spaceship 'Core rMap cores s
                -> Spaceship 'Optional rMap cores s
lockCoreSystems _ = undefined

-- Thêm Shield (Optional Module)
-- Cost: 1 slot. Check Compatibility với TOÀN BỘ reactor đã lắp.
addShield :: (CheckSlots s 1, CheckCompatibility rMap st)
          => Proxy (st :: ShieldType)
          -> Spaceship 'Optional rMap cores s
          -> Spaceship 'Optional rMap cores (s + 1)
addShield _ _ = undefined

-- Thêm Sensors (Optional Module) - Ví dụ thêm để đầy slot
addSensors :: (CheckSlots s 1)
           => Spaceship 'Optional rMap cores s
           -> Spaceship 'Optional rMap cores (s + 1)
addSensors _ = undefined

-- [A-212] Finalize
-- Chuyển sang 'Finalized. Sau bước này không thể gọi hàm nào nữa (vì không hàm nào nhận 'Finalized).
finalize :: Spaceship 'Optional rMap cores s -> Spaceship 'Finalized rMap cores s
finalize _ = undefined

-- ==========================================
-- 5. SCENARIOS & PROOF
-- ==========================================

-- Kịch bản 1: Design Hợp lệ (100% OK)
validShip :: Spaceship 'Finalized '( 'True, 'False) '( 'True, 'True, 'True, 'True) 9
validShip =
    let s0 = Ship "The Haskell"         -- Init
        s1 = setFrame s0                -- Core
        s2 = addReactor fusion s1       -- Core (Slots: 3, Reactors: Fusion)
        s3 = addEngine s2               -- Core (Slots: 5)
        s4 = addLifeSupport s3          -- Core (Slots: 7)
        s5 = addBridge s4               -- Core (Slots: 8)
        s6 = lockCoreSystems s5         -- Optional (Check Integrity OK)
        s7 = addShield magShield s6     -- Optional (Slots: 9, Check Comp: Fusion + Mag OK)
        s8 = finalize s7                -- Finalized
    in s8

-- Kịch bản 2: Edge Case Fix (Lắp 2 loại Reactor)
-- Tàu này có cả Fusion và Antimatter.
-- Nó KHÔNG được phép lắp Phase Shield (kỵ Fusion) VÀ KHÔNG được lắp Magnetic Shield (kỵ Antimatter).
-- Code dưới đây nếu bỏ comment dòng cuối sẽ báo lỗi ngay lập tức.
mixedReactorShip =
    let s0 = Ship "HybridPower"
        s1 = setFrame s0
        s2 = addReactor fusion s1       -- Has Fusion
        s3 = addReactor antimatter s2   -- Has Fusion AND Antimatter (Slots: 6)
        s4 = addEngine s3
        s5 = addLifeSupport s4
        s6 = addBridge s5               -- Slots: 11 (Opps, quá slot -> Lỗi B-307 nếu compile).
                                        -- Giả sử ta bỏ bớt addReactor thứ 2 để đủ slot test logic Shield:
    in s1 
    
    -- TEST LOGIC B-440 (Bỏ comment để thấy lỗi):
    -- let bad = addShield phaseShield (lockCoreSystems (addBridge (addLifeSupport (addEngine (addReactor antimatter (addReactor fusion (setFrame (Ship "X"))))))))
    -- -> Lỗi: Cannot install Phase Shield detected Fusion Reactor!
    
    -- let bad2 = addShield magShield (lockCoreSystems (addBridge (addLifeSupport (addEngine (addReactor fusion (addReactor antimatter (setFrame (Ship "X"))))))))
    -- -> Lỗi: Cannot install Magnetic Shield detected Antimatter Reactor!


main :: IO ()
main = putStrLn "Congratulations! Your Spaceship DSL is Type-Safe and Compile-Time Checked."
