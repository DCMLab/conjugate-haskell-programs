{-# LANGUAGE TemplateHaskell, DataKinds, DeriveGeneric, TypeApplications, ScopedTypeVariables,
             UndecidableInstances, FlexibleContexts, PartialTypeSignatures, StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}
module Main where

import           Inference.Conjugate

import           Control.Monad                  ( foldM
                                                , replicateM
                                                )
import           Data.Dynamic                   ( Dynamic
                                                , Typeable
                                                , fromDynamic
                                                , toDyn
                                                )
import           Data.Foldable                  ( forM_ )
import           Data.Maybe                     ( mapMaybe )
import qualified Data.Sequence                 as S
import qualified Data.Vector                   as V
import           GHC.Float                      ( int2Double )
import           GHC.Generics
import           Lens.Micro.TH                  ( makeLenses )
import           System.Random.MWC.Probability

-- | An example of a record that describes prior variables.
-- It can be instantiated with different type constructors for 'f' (kind @* -> *@),
-- which allows it to represent both the hyperparameters and parameters of the model.
data ExampleParams f = ExampleParams
  { _epP    :: f Beta
  , _epCat1 :: f (Dirichlet 3)
  , _epCat2 :: f (Dirichlet 3)
  }
  deriving Generic

-- We need lenses to the fields of ExampleParams, which can be generated automatically.
makeLenses ''ExampleParams

-- The Show instance for ExampleParams needs to be standalone
-- because of the Show (f p l) constraints that GHC can't figure out on its own.
-- Here we specify it manually for nicer output.
instance ( Show (f Beta)
         , Show (f (Dirichlet 3)))
         => Show (ExampleParams f) where
  show (ExampleParams p cat1 cat2) =
    "ExampleParams"
      <> "\n  epP    = "
      <> show p
      <> "\n  epCat1 = "
      <> show cat1
      <> "\n  epCat2 = "
      <> show cat2

-- The prior record can be instantiated for...

-- ... parameters:
exampleProbs :: ExampleParams ProbsRep
exampleProbs = ExampleParams { _epP    = ProbsRep 0.7
                             , _epCat1 = ProbsRep $ V.fromList [0.3, 0.1, 0.6]
                             , _epCat2 = ProbsRep $ V.fromList [0.1, 0.8, 0.1]
                             }

-- ... hyperparameters:
examplePriors :: ExampleParams HyperRep
examplePriors = ExampleParams { _epP    = HyperRep (0.5, 0.5)
                              , _epCat1 = HyperRep $ V.fromList [0.5, 0.5, 0.5]
                              , _epCat2 = HyperRep $ V.fromList [0.5, 0.5, 0.5]
                              }

-- | The likelihood of the example model, described as a probabilistic program.
-- It uses lenses into the parameter record to refer to the parameters it draws samples from.
-- The distribution of each variable is thus determined by the type of the lens / record field.
-- The likelihood can be interpreted by different interpreters 'm' for inference.
exampleLk :: _ => RandomInterpreter m ExampleParams => m Int
exampleLk = do
  coin <- sampleValue "coin" Bernoulli epP
  sampleValue "cat" (Categorical @3) $ if coin then epCat1 else epCat2

-- | An example of how to work with a model.
main :: IO ()
main = do
  -- Define a prior distribution.
  -- Here, ExampleParams' Generic instance is used to get Jeffrey's prior for all its fields.
  let prior :: ExampleParams HyperRep
      prior = jeffreysPrior @ExampleParams
  putStrLn "prior:"
  print prior
  -- Initialize a mutable random state
  gen                             <- createSystemRandom
  -- Sample a set of probabilities from the prior.
  -- This is again done using the Generic instance of ExampleParams.
  probs :: ExampleParams ProbsRep <- sample (sampleProbs @ExampleParams prior)
                                            gen
  putStrLn "sampled probabilities:"
  print probs
  -- Sample a trace from the likelihood
  (result, trace) <- sampleTrace probs exampleLk gen
  -- Print the trace (which contains only the sampled values).
  -- The source of each value is obtained by running the trace through the model again.
  putStrLn "trace:"
  printTrace trace exampleLk
  putStrLn $ "result: " <> show result
  -- Evaluate the log probability of the trace.
  let logp = snd <$> evalTraceLogP probs trace exampleLk
  putStrLn $ "log p(trace) = " <> show logp
  -- Update the priors according to the sampled trace.
  -- Normally, this trace is obtained from observations in a dataset.
  let posteriorMb = getPosterior prior trace exampleLk
  putStrLn "posterior (using sampled trace):"
  case posteriorMb of
    Just posterior -> print posterior
    Nothing        -> putStrLn "failed to compute posterior"
  -- Construct traces manually (e.g. from observations)
  -- by storing the sequence of sampled values as they appear in the dataset.
  let tracesObs =
        [ Trace $ S.fromList [toDyn True, toDyn (0 :: Int)]
        , Trace $ S.fromList [toDyn False, toDyn (1 :: Int)]
        , Trace $ S.fromList [toDyn True, toDyn (0 :: Int)]
        , Trace $ S.fromList [toDyn False, toDyn (2 :: Int)]
        , Trace $ S.fromList [toDyn True, toDyn (0 :: Int)]
        , Trace $ S.fromList [toDyn False, toDyn (1 :: Int)]
        , Trace $ S.fromList [toDyn False, toDyn (2 :: Int)]
        , Trace $ S.fromList [toDyn False, toDyn (1 :: Int)]
        ]
      posteriorObsMb =
        foldM (\hyper obs -> getPosterior hyper obs exampleLk) prior tracesObs
  putStrLn "posterior (using observations):"
  case posteriorObsMb of
    Just posteriorObs -> print posteriorObs
    Nothing           -> putStrLn "failed to compute posterior"
  -- See how much the we have learned:
  -- Draw uniform samples as a baseline and compare them with the observations
  -- both under the prior and under the posterior distribution.
  let nFakes = 100
  fakes <- replicateM nFakes $ do
    probs <- sample (sampleProbs @ExampleParams prior) gen
    sampleTrace probs exampleLk gen
  -- putStrLn "random traces for comparison:"
  -- forM_ fakes $ \(_, t) -> printTrace t exampleLk
  let
    Just posteriorObs = posteriorObsMb
    fakeLogPsPrior    = mapMaybe
      (\(_, trace) -> snd <$> evalTracePredLogP prior trace exampleLk)
      fakes
    fakeLogPsPosterior = mapMaybe
      (\(_, trace) -> snd <$> evalTracePredLogP posteriorObs trace exampleLk)
      fakes
    obsLogPsPrior = mapMaybe
      (\trace -> snd <$> evalTracePredLogP prior trace exampleLk)
      tracesObs
    obsLogPsPosterior = mapMaybe
      (\trace -> snd <$> evalTracePredLogP posteriorObs trace exampleLk)
      tracesObs
  putStrLn "before learning:"
  putStrLn $ "  mean prior logp (fakes):     " <> show
    (sum fakeLogPsPrior / int2Double nFakes)
  putStrLn $ "  mean prior logp (obs):       " <> show (sum obsLogPsPrior / 8)
  putStrLn "after learning:"
  putStrLn $ "  mean posterior logp (fakes): " <> show
    (sum fakeLogPsPosterior / int2Double nFakes)
  putStrLn $ "  mean posterior logp (obs):   " <> show
    (sum obsLogPsPosterior / 8)
