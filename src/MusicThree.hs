module MusicThree where

import Euterpea
import Euterpea.Music

import System.Random
import System.Random.Distributions

--------------------- Generative Self-Similar Music  -----------------------------
-- | a rose tree to represent a self-similar generative melody of simple notes
data Cluster = Cluster SNote [Cluster] deriving Show

-- | a simple note
type SNote = (Dur, AbsPitch)

-- | given a pattern of simple notes, generate an infinite self similar melody
selfSim :: [SNote] -> Cluster
selfSim pattern = Cluster (0,0) (fmap mkCluster pattern)
  where mkCluster note =
          Cluster note (fmap (mkCluster . addMult note) pattern)

addMult :: SNote -> SNote -> SNote
addMult (d0, p0) (d1, p1) = (d0 * d1, p0 + p1)

-- | remove notes from the nth level
fringe :: Int -> Cluster -> [SNote]
fringe 0 (Cluster note cls) = [note]
fringe n (Cluster note cls) = concatMap (fringe $ n - 1) cls

-- | now we convert this to a melody euterpea understands
simToMusic :: [SNote] -> Music Pitch
simToMusic = line . fmap mkNote

mkNote :: (Dur, AbsPitch) -> Music Pitch
mkNote (d, ap) = note d (pitch ap)

generate :: [SNote] -> Int -> AbsPitch -> Dur -> Music Pitch
generate pattern level nPitches tempoScale =
  transpose nPitches $ tempo tempoScale $ simToMusic $
  fringe level $ selfSim pattern

m0 :: [SNote]
m0 = [(1,2), (1,0), (1,5), (1,7)]

tm0 = instrument Vibraphone (generate m0 10 50 10)
ttm0 = tm0 :=: (transpose 12 $ retro tm0)

m1 :: [SNote]
m1 = [(1,0), (0.5,0),(0.5,0)]

tm1 = instrument Percussion (generate m1 4 43 (0.5))

m2 :: [SNote]
m2 = [(dqn,0), (qn,4)]

tm2 = generate m2 6 50 (1/50)

m3 :: [SNote]
m3 = [(hn, 3), (qn, 4), (qn, 0), (hn, 6)]

tm3 = generate m3 4 50 (1/4)
ttm3 = l1 :=: l2
  where l1 = instrument ElectricGuitarClean tm3
        l2 = instrument AcousticBass $ transpose (-9) (retro tm3)

m4 :: [SNote]
m4 = [ (hn, 3), (hn, 8), (hn, 22), (qn, 4), (qn, 7), (qn, 21)
     , (qn,0), (qn,5), (qn,15), (wn,6), (wn,9), (wn,19)]
tm4 = generate m4 3 10 (1/2)

-- | instead of using a music seed value and then playing each note at each
-- level we can play a level all at one time
fringe' :: Int -> Cluster -> [[SNote]]
fringe' 0 (Cluster note cls) = [[note]]
fringe' n (Cluster note cls) = fmap (fringe (n-1)) cls

simToMusic' :: [[SNote]] -> Music Pitch
simToMusic' = chord . fmap (line . fmap mkNote)

generate' :: [SNote] -> Int -> AbsPitch -> Dur -> Music Pitch
generate' pat n tr te =
  transpose tr $ tempo te $ simToMusic' $ fringe' n $ selfSim pat

ss1 = generate' m2 4 50 (1/8)
ss2 = generate' m3 4 50 (1/2)
ss3 = generate' m4 3 50 2

m5 = [(en, 4), (sn, 7), (en, 0)]
ss5 = generate m5 8 45 (1/50000000)
ss6 = generate' m5 4 45 (1/10000)

----------------------- Stochastic Compositions -------------------------------
-- | we need to be able to convert Floats to pitches
toAbs :: Float -> AbsPitch
toAbs x = round $ 40 * x + 30

-- | now we can make notes. I've set the duration to a tenth note only
mkTNote :: AbsPitch -> Music Pitch
mkTNote = note en . pitch

-- | making a random melody is just a matter of taking a list of absPitches and
-- fmaping mkNotes onto them and then collecting them all with line. The only
-- question now is how to generate the list of Floats that turns into AbsPitches
-- that turns in to notes
mkRandMelody :: [AbsPitch] -> Music Pitch
mkRandMelody = line . take 32 . fmap mkTNote

-- a uniform series of notes
melUni :: Music Pitch
melUni = mkRandMelody ((randomRs (30, 70) (mkStdGen 42)))
  -- randomRs generates a list of random numbers between 30 and 70 using the
  -- generator returned by mkStdGen

-- a linear distribution
melLin :: Music Pitch
melLin = mkRandMelody . fmap toAbs . rands linear $ (mkStdGen 42)

-- an exponential distribution
melExp :: Float -> Music Pitch
melExp = mkRandMelody . fmap toAbs . flip rands (mkStdGen 42) . exponential

-- a guassian distribution, the distribution is dictated by sigma and mu, the
-- spread and the center
melGauss :: Float -> Float -> Music Pitch
melGauss sig mu
  = mkRandMelody . fmap toAbs . (flip rands (mkStdGen 42)) $ gaussian sig mu

  -- some examples melGauss 0.01 0.5 places the center of the dist in the middle
  -- of the scale range, with a spread of 0.01, try sigmas of 0.1 and 0.05

-- we can do random walks by treating a sequence of random numbers as intervals
-- instead of pitches

toAbs2 :: Float -> AbsPitch
toAbs2 = round . (5*)

-- | here we take a start pitch to begin our walk, then we take a list of random
-- numbers that dictate the interval, the rest is just calling scanl (+) which
-- will generate a list of lists of intermediary values as the rand list is
-- summed up with start as the accumulator. Then we convert these numerics in to
-- Music Pitches
mkLine2 :: AbsPitch -> [AbsPitch] -> Music Pitch
mkLine2 start rands = line . take 64 . fmap mkTNote $ scanl (+) start rands

-- | we set the guassian mean to zero and start our rand walk at 50
randomWalkGauss :: Float -> Music Pitch
randomWalkGauss sig = mkLine2 50 . fmap toAbs2 . flip rands (mkStdGen 42) $ gaussian sig 0

randomWalkExp :: Float -> Music Pitch
randomWalkExp lam = mkLine2 50 . fmap (toAbs2 . subtract (1/lam)) . flip rands (mkStdGen 42) $ exponential lam
