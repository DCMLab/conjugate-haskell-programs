{-# LANGUAGE TemplateHaskell, DataKinds, DeriveGeneric, TypeApplications, ScopedTypeVariables,
             UndecidableInstances, FlexibleContexts, PartialTypeSignatures, StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}

import           Inference.Conjugate

import           GHC.Generics                   ( Generic )

main :: IO ()
main = putStrLn "Test suite not yet implemented"

newtype PermutationParams f = PermParams
  { _ppP :: f Beta
  }
  deriving (Generic)

deriving instance (Show (f Beta)) => Show (PermutationParams f)

makeLenses ''PermutationParams

examplePerm :: _ => RandomInterpreter m PermutationParams => m Int
examplePerm = do
  coins <- permutationPlate 10 $ sampleValue Bernoulli ppP
  pure $ sum $ (\c -> if c then 1 else 0) <$> coins
