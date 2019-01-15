module MusicOne where

import Data.Foldable (foldr1)

import Euterpea (play)
import Euterpea.Music

-- this is how you play a single note
  -- function app is a space
  -- yes c is a func
-- play (c 4 qn)

-- n = c 4 qn
-- m = b 4 wn

weslyChord = [c 4 wn, e 4 wn, g 4 wn]

weslyChords = zipWith (\mp ptc -> transpose ptc mp) threetimes [1,5..]
  where
    weslyChord' = foldr1 (:=:) weslyChord
    threetimes = take 3 $ repeat weslyChord'

playSeq :: Music a -> Music a -> Music a
playSeq = (:+:)

ian = [c, d]

-- data Music a
--   = Prim (Primitive a)
--   | (Music a) :+: (Music a)
--   | (Music a) :=: (Music a)
--   | Modify Control (Music a)

-- data Primitive a = Note Dur a | Rest Dur


-- data Control
--   = Tempo Rational
--   | Transpose AbsPitch
--   | Instrument InstrumentName
--   | Phrase [PhraseAttribute]
--   | KeySig PitchClass Mode
--   | Custom String
        -- Defined in ‘Euterpea.Music’

mkPitch :: PitchClass -> Octave -> Rational -> Music Pitch
mkPitch pc oct rat = Prim (Note rat (pc, oct))

chngPitch :: PitchClass -> Music Pitch -> Music Pitch
chngPitch pc (Prim (Note rat (_, oct))) = Prim (Note rat (pc, oct))



peice1 = [e 3 qn, d 2 qn, c 1 qn, d 3 qn] ++ (rep [e 3 qn] 2) ++ [e 3 hn] ++  (rep [d 3 qn] 2) ++ [d 3 hn] ++ (rep [e 3 qn] 2) ++ [e 3 hn] ++ [e 3 qn, d 2 qn, c 1 qn, d 3 qn] ++ (rep [e 3 qn] 3) ++ [e 3 qn, d 2 qn, d 2 qn, e 3 qn, d 2 qn, c 1 sn]

merge :: [a] -> [a] -> [a]
merge xs ys = xs ++ ys

rep :: [x] -> Int -> [x]
rep xs 0 = []
rep xs i = merge xs (rep xs (i-1))

combineSequentially :: [Music a] -> Music a
combineSequentially = foldr1 (:+:)

playSameTime :: [Music a] -> Music a
playSameTime = foldr1 (:=:)

toInstrument :: InstrumentName -> Music a -> Music a
toInstrument i m = Modify (Instrument i) m

speedUp :: Real d => d -> Music a -> Music a
speedUp = Modify . Tempo . toRational
