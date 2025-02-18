module Test.Plutus.Conversion.Address (suite) where

import Prelude

import Data.Maybe (Maybe(Just, Nothing), fromJust)
import Data.Array ((..), length, zip)
import Data.UInt (UInt, fromInt)
import Data.Newtype (class Newtype, wrap)
import Data.Traversable (for_)
import Data.Tuple (Tuple(Tuple))
import Data.Tuple.Nested ((/\))
import Mote (group, test)
import Partial.Unsafe (unsafePartial)
import Plutus.Conversion (fromPlutusAddress, toPlutusAddress)
import Plutus.Types.Address (Address) as Plutus
import Plutus.Types.Credential
  ( Credential(PubKeyCredential, ScriptCredential)
  , StakingCredential(StakingHash, StakingPtr)
  )
import Serialization.Address
  ( NetworkId(MainnetId, TestnetId)
  , addressFromBech32
  )
import Serialization.Hash (ed25519KeyHashFromBech32, scriptHashFromBech32)
import Test.Spec.Assertions (shouldEqual)
import Test.Utils (errMaybe, toFromAesonTest)
import TestM (TestPlanM)
import Types.Aliases (Bech32String)

suite :: TestPlanM Unit
suite = do
  group "Conversion: Plutus Address <-> CSL Address" do
    group "Shelley mainnet addresses" do
      addressConversionTests MainnetId
    group "Shelley testnet addresses" do
      addressConversionTests TestnetId
  group "Plutus.Types.Address" $ do
    group "FromPlutusType & ToPlutusType" $ do
      let indices = 0 .. (length addresses - 1)
      group "Shelley mainnet addresses" $ do
        let testData = zip (zip addressesBech32Mainnet addresses) indices
        for_ testData $ \(Tuple (Tuple addrBech32 addr) addrType) ->
          toFromPlutusAddressTest MainnetId addrType addrBech32 addr
      group "Shelley testnet addresses" $ do
        let testData = zip (zip addressesBech32Testnet addresses) indices
        for_ testData $ \(Tuple (Tuple addrBech32 addr) addrType) ->
          toFromPlutusAddressTest TestnetId addrType addrBech32 addr
    group "Aeson tests" $ do
      group "Roundtrip tests"
        $ for_ addresses
        $ toFromAesonTest "Address"

addressConversionTests :: NetworkId -> TestPlanM Unit
addressConversionTests networkId =
  let
    addressesBech32 =
      case networkId of
        MainnetId -> addressesBech32Mainnet
        TestnetId -> addressesBech32Testnet
    indices = 0 .. (length addresses - 1)
    testData = zip (zip addressesBech32 addresses) indices
  in
    for_ testData $ \((addrBech32 /\ addr) /\ addrType) ->
      toFromPlutusAddressTest networkId addrType addrBech32 addr

toFromPlutusAddressTest
  :: NetworkId -> Int -> Bech32String -> Plutus.Address -> TestPlanM Unit
toFromPlutusAddressTest networkId addrType addrBech32 addrPlutus = do
  let testLabel = "Performs conversion between addresses of type "
  test (testLabel <> show addrType) $ do
    addrForeign <-
      errMaybe "addressFromBech32 failed on valid bech32" $
        addressFromBech32 addrBech32
    resAddrPlutus <-
      errMaybe "toPlutusAddress failed on valid foreign address" $
        toPlutusAddress addrForeign
    resAddrPlutus `shouldEqual` addrPlutus
    resAddrForeign <-
      errMaybe "fromPlutusAddress failed on valid native address" $
        fromPlutusAddress networkId resAddrPlutus
    resAddrForeign `shouldEqual` addrForeign

-- Mainnet addresses.
-- Test vectors are taken from the CIP-0019 specification.
-- https://github.com/cardano-foundation/CIPs/tree/master/CIP-0019#test-vectors
addressesBech32Mainnet :: Array Bech32String
addressesBech32Mainnet =
  [ "addr1qx2fxv2umyhttkxyxp8x0dlpdt3k6cwng5pxj3jhsydzer3n0d3vllmyqwsx5wktcd8"
      <> "cc3sq835lu7drv2xwl2wywfgse35a3x"

  , "addr1z8phkx6acpnf78fuvxn0mkew3l0fd058hzquvz7w36x4gten0d3vllmyqwsx5wktcd8"
      <> "cc3sq835lu7drv2xwl2wywfgs9yc0hh"

  , "addr1yx2fxv2umyhttkxyxp8x0dlpdt3k6cwng5pxj3jhsydzerkr0vd4msrxnuwnccdxlhd"
      <> "jar77j6lg0wypcc9uar5d2shs2z78ve"

  , "addr1x8phkx6acpnf78fuvxn0mkew3l0fd058hzquvz7w36x4gt7r0vd4msrxnuwnccdxlhd"
      <> "jar77j6lg0wypcc9uar5d2shskhj42g"

  , "addr1gx2fxv2umyhttkxyxp8x0dlpdt3k6cwng5pxj3jhsydzer5pnz75xxcrzqf96k"

  , "addr128phkx6acpnf78fuvxn0mkew3l0fd058hzquvz7w36x4gtupnz75xxcrtw79hu"

  , "addr1vx2fxv2umyhttkxyxp8x0dlpdt3k6cwng5pxj3jhsydzers66hrl8"

  , "addr1w8phkx6acpnf78fuvxn0mkew3l0fd058hzquvz7w36x4gtcyjy7wx"
  ]

-- Testnet addresses.
-- Test vectors are taken from the CIP-0019 specification.
-- https://github.com/cardano-foundation/CIPs/tree/master/CIP-0019#test-vectors
addressesBech32Testnet :: Array Bech32String
addressesBech32Testnet =
  [ "addr_test1qz2fxv2umyhttkxyxp8x0dlpdt3k6cwng5pxj3jhsydzer3n0d3vllmyqwsx5w"
      <> "ktcd8cc3sq835lu7drv2xwl2wywfgs68faae"

  , "addr_test1zrphkx6acpnf78fuvxn0mkew3l0fd058hzquvz7w36x4gten0d3vllmyqwsx5w"
      <> "ktcd8cc3sq835lu7drv2xwl2wywfgsxj90mg"

  , "addr_test1yz2fxv2umyhttkxyxp8x0dlpdt3k6cwng5pxj3jhsydzerkr0vd4msrxnuwncc"
      <> "dxlhdjar77j6lg0wypcc9uar5d2shsf5r8qx"

  , "addr_test1xrphkx6acpnf78fuvxn0mkew3l0fd058hzquvz7w36x4gt7r0vd4msrxnuwncc"
      <> "dxlhdjar77j6lg0wypcc9uar5d2shs4p04xh"

  , "addr_test1gz2fxv2umyhttkxyxp8x0dlpdt3k6cwng5pxj3jhsydzer5pnz75xxcrdw5vky"

  , "addr_test12rphkx6acpnf78fuvxn0mkew3l0fd058hzquvz7w36x4gtupnz75xxcryqrvmw"

  , "addr_test1vz2fxv2umyhttkxyxp8x0dlpdt3k6cwng5pxj3jhsydzerspjrlsz"

  , "addr_test1wrphkx6acpnf78fuvxn0mkew3l0fd058hzquvz7w36x4gtcl6szpr"
  ]

addresses :: Array Plutus.Address
addresses = wrap <$>
  [ { addressCredential: pubKeyCredential
    , addressStakingCredential: Just stakingHash
    }
  , { addressCredential: scriptCredential
    , addressStakingCredential: Just stakingHash
    }
  , { addressCredential: pubKeyCredential
    , addressStakingCredential: Just $ StakingHash scriptCredential
    }
  , { addressCredential: scriptCredential
    , addressStakingCredential: Just $ StakingHash scriptCredential
    }
  , { addressCredential: pubKeyCredential
    , addressStakingCredential: Just stakingPtr
    }
  , { addressCredential: scriptCredential
    , addressStakingCredential: Just stakingPtr
    }
  , { addressCredential: pubKeyCredential
    , addressStakingCredential: Nothing
    }
  , { addressCredential: scriptCredential
    , addressStakingCredential: Nothing
    }
  ]

paymentKeyBech32 :: Bech32String
paymentKeyBech32 =
  "addr_vkh1jjfnzhxe966a33psfenm0ct2udkkr569qf55v4uprgkgu8zsvmg"

pubKeyCredential :: Credential
pubKeyCredential =
  PubKeyCredential <<< wrap <<< unsafePartial fromJust $
    ed25519KeyHashFromBech32 paymentKeyBech32

scriptBech32 :: Bech32String
scriptBech32 =
  "script1cda3khwqv60360rp5m7akt50m6ttapacs8rqhn5w342z7r35m37"

scriptCredential :: Credential
scriptCredential =
  ScriptCredential <<< wrap <<< unsafePartial fromJust $
    scriptHashFromBech32 scriptBech32

stakeKeyBech32 :: Bech32String
stakeKeyBech32 =
  "stake_vkh1xdak9nllvsp6q636e0p5lrzxqq7xnlne5d3gemafc3e9z3v4vud"

stakingHash :: StakingCredential
stakingHash =
  StakingHash <<< PubKeyCredential <<< wrap <<< unsafePartial fromJust $
    (ed25519KeyHashFromBech32 stakeKeyBech32)

stakingPtr :: StakingCredential
stakingPtr =
  let
    wrapUInt :: forall (t :: Type). Newtype t UInt => Int -> t
    wrapUInt = wrap <<< fromInt
  in
    StakingPtr $
      { slot: wrapUInt 2498243, txIx: wrapUInt 27, certIx: wrapUInt 3 }
