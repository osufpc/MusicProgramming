module MusicTwo where

import Euterpea
import Euterpea.Music

---------------------------------- Recap ---------------------------------------
-- | play a note
-- play (c 4 qn)

-- | you can view the AST of the music just be evaluating it in ghci
-- > c 4 wn
-- Prim (Note (1 % 1) (C,4))

-- | notes are functions
-- c :: Octave -> Dur -> Music Pitch

-- | our main type is (Music a), typically a is reified to Pitch, use (:i Music)
-- in ghci to view information about the Music type
-- data Music a
--   = Prim (Primitive a)
--   | (Music a) :+: (Music a)
--   | (Music a) :=: (Music a)
--   | Modify Control (Music a)

-- | We can play notes sequentially with (:+:)
seqPlay :: Music Pitch
seqPlay = (c 4 wn) :+: (g 4 wn)

-- play seqPlay

-- | We can play notes and in parallel with (:=:)
parPlay :: Music Pitch
parPlay = (c 4 wn) :=: (g 4 wn)

-- | We can hold notes in lists because this is FP
weslysChord = [c 4 wn, e 4 wn, g 4 wn]

-- | and then we can map and fold over lists like usual
-- foldr1 just uses the last element in the list as the accumulator
-- we could also call foldr with rest like so
-- combineSequentially = foldr (:+:) (rest wn)
combineSequentially :: [Music a] -> Music a
combineSequentially = foldr1 (:+:)

-- this is also exported by Euterpea as the function:
-- line :: [Music a] -> Music a

playSameTime :: [Music a] -> Music a
playSameTime = foldr1 (:=:)

-- | We can change notes using the Modify data constructor
-- here are the convienient functions

-- | make a note
-- note :: Dur -> a -> Music a

-- | define a rest
-- rest :: Dur -> Music a

-- | set the tempo of a Music Pitch
-- tempo :: Dur -> Music a -> Music a

-- | transpose a Music pitch by an Absolute Pitch
-- transpose :: AbsPitch -> Music a -> Music a

-- an Absolute Pitch is just an integer, Euterpea converts a pitch to an
-- see chapter 2 page 48 in the pdf for more info
-- abspitch by the absPitch function
-- absPitch :: Pitch -> AbsPitch

----------------------- Some Simple Compositions -------------------------------
-- | given a list of anything, generate the prefixes for all elements in the
-- list. i.e. prefixes [1..3] = [[1],[1,2],[1,2,3]]
prefixes :: [a] -> [[a]]
prefixes [] = []
prefixes (x:xs) = [x] : map f (prefixes xs)
  where f prefx = x : prefx

-- now we can apply this to a line of notes
composePrefixes :: [Music Pitch] -> Music Pitch
-- playPrefixes ms = line (concat $ prefixes ms)
  -- equivalent in point free style
composePrefixes = line . concat . prefixes

-- | something a little more complicated
prefix :: [Music Pitch] -> Music Pitch
prefix ms = m :+: transpose 5 m :+: m
  where
    -- we compose with a prefix of the original melody
    m1 = composePrefixes ms

    -- we reverse the melody and then transpose it up a perfect fourth
    -- book says that is five semitones...idk...music people help?
    m2 = transpose 12 $ composePrefixes (reverse ms)

    -- now we take m1, the prefix of the original, play it on a flute
    -- and we take the reverse, transposed version of it and play it on a
    -- VoiceOohs
    m  = instrument Flute m1 :=: instrument VoiceOohs m2

-- | some cool stuff with this composition
coolOne = [c 5 en, e 5 sn, g 5 en, b 5 sn, a 5 en, f 5 sn, d 5 en, b 4 sn, c 5 en]
coolTwo = [c 5 sn, e 5 sn, g 5 sn, b 5 sn, a 5 sn, f 5 sn, d 5 sn, b 4 sn, c 5 sn]

-- Make sure you give this a listen:
-- play coolOne
-- play (prefix coolOne)
-- play coolTwo
-- play (prefix coolTwo)


----------------------- More Music Functions ------------------------------------

-- ******************* Delays and Repeats ************************************ --
-- offset by adding a rest in front of your notes
-- offset :: Dur -> Music a -> Music a
-- offset d m = rest d :+: m

-- we can also repeat a note several times using the times function like so
-- times :: Int -> Music a -> Music a

-- or we can define it ourselves using list operations
myTimes :: Int -> Music a -> Music a
myTimes i m = line (take 1 (repeat m))

-- ******************* Inversions and Retrograde ***************************** --
-- retrograde means play in reverse
-- an inversion means to negate the pitches after the first note
-- these functions are predefined
-- > :t retro
-- > retro :: Music a -> Music a

-- > :t invert
-- > invert :: Music Pitch -> Music Pitch

-- They both use the lineToList function which decomposes a line of music into a
-- list of notes
-- > :t lineToList
-- > lineToList :: Music a -> [Music a]


-- ******************* Computing Duration ***************************** --
-- sometimes you want to know the exact duration of a (Music a), use the dur
-- function to do this
-- > :t dur
-- > dur :: Music a -> Dur

-- for example run the following to show that prefix increases the duration of
-- our simple composition

-- dur $ line coolTwo
-- dur $ prefix coolTwo

-- duration is a Rational, the duration of (m :+: n) = (dur m) + (dur n)
-- duration of (m :=: n) = max m n

-- for taking the min of a parallel composition use the (/=:) operator
-- > :t (/=:)
-- > (/=:) :: Music a -> Music a -> Music a

-- what will happen when we do this:
-- > dur (forever (c 4 qn))


-- ******************* Cut and Remove ***************************** --

-- we can remove notes from the beginning of a composition with the (cut)
-- function

-- > :t cut
-- > cut :: Dur -> Music a -> Music a

-- Similary we can remove notes from the end with (remove)
-- > :t remove
-- > remove :: Dur -> Music a -> Music a

-- if you're interested in these definitions see pg 88 in the book

-- ******************* Removing Zeros ***************************** --
-- Some of these functions introduce empty notes e.g. rest 0
-- we can erase these from teh AST to simplify things with the
-- removeZeros function
-- > :t removeZeros
-- > removeZeros :: Music a -> Music a

-- observe
test = c 4 en :+: forever (d 4 en)

removeTest = cut hn (remove hn test)

-- > removeTest
-- > Prim (Note (0 % 1) (C,4)) :+: (Prim (Note (0 % 1) (D,4)) :+: (Prim (Note (0
-- > % 1) (D,4)) :+: (Prim (Note (0 % 1) (D,4)) :+: (Prim (Note (1 % 8) (D,4))
-- > :+: (Prim (Note (1 % 8) (D,4)) :+: (Prim (Note (1 % 8) (D,4)) :+: (Prim
-- > (Note (1 % 8) (D,4)) :+: Prim (Rest (0 % 1)))))))))

-- an now
-- > removeZeros removeTest
-- > Prim (Note (1 % 8) (D,4)) :+: (Prim (Note (1 % 8) (D,4)) :+: (Prim (Note (1
-- > % 8) (D,4)) :+: Prim (Note (1 % 8) (D,4))))


-- ***************************** Trills *********************************** --

-- a trill rapidly alternates between two pitches, we can define this like so
trill :: Int -> Dur -> Music Pitch -> Music Pitch
trill i dur (Prim (Note tDur p))
  | dur >= tDur = note tDur p
  | otherwise   = note dur p :+:
                  trill (negate i) dur
                  (note (tDur - dur) (trans i p))
trill i d (Modify (Tempo r) m) = tempo r (trill i (d * r) m)
trill i d (Modify c m)         = Modify c (trill i d m)
trill _ _ _                    = error "trill: must have a note"

-- we can use this to define other trills, like one that specifies the number of
-- subdivided notes to be included
trilln :: Int -> Int -> Music Pitch -> Music Pitch
trilln i nTimes m = trill i (dur m/fromIntegral nTimes) m

-- we can also define a roll
roll :: Dur -> Music Pitch -> Music Pitch
roll dur m = trill 0 dur m

-- and rolln
rolln :: Int -> Music Pitch -> Music Pitch
rolln nTimes m = trilln 0 nTimes m

-- *********************** Map and Fold for Music ********************************** --

-- Euterpea provides a map and fold for music. These are sophisticated
-- alterations of the AST not like the folds we've been doing on lists
-- They are:

-- pMap maps over primitive values
-- > :t pMap
-- > pMap :: (a -> b) -> Primitive a -> Primitive b

-- mMap maps over music values
-- > :t mMap
-- > mMap :: (a -> b) -> Music a -> Music b

-- try this out, note this is eta reduced
-- Volume ranges from 0 to 127 in Midi
getLoud :: Volume -> Music Pitch -> Music (Pitch, Volume)
getLoud v = mMap (\p -> (p, v))
  -- non-eta reduced
  -- getLoud v m = mMap (\p -> (p, v)) m

-- the fold for music is complicated because Music has 4 constructors
-- > :t mFold
-- > mFold
  -- > :: (Primitive a -> b)
  -- > -> (b -> b -> b)
  -- > -> (b -> b -> b)
  -- > -> (Control -> b -> b)
  -- > -> Music a
   -- > -> b

-- we can also count notes like this:
-- mFold (const 1) (+) (+) (const $ const 0) (line weslysChord)

-- here is how we can define the dur function using the fold
-- dur = mFold getDur (+) max modDur
-- where getDur (Note d _) = d
--       getDur (Rest d) = d
--
--       modDur (Tempo r) d = d / r
--       modDur  _        d = d

-- ******************** Complexity and Recursion ********************************** --
-- check this out
x1 = g 4 qn :=: (c 4 en :+: d 4 en :+: e 4 en)
x2 = g 4 qn :=: tempo (3/2) (c 4 en :+: d 4 en :+: e 4 en)

-- the 3/2 cased the eighth notes to sound like triplets!!
-- we can use this to make phase compositions like so:
phaseIt factor m = m :=: tempo factor m

-- we can also define a function to apply transformations to elements in a
-- sequence and to accumulate phrases a number of times
rep :: (Music a -> Music a) -> (Music a -> Music a) -> Int -> Music a -> Music a
rep f g 0 m = rest 0
rep f g n m = m :=: g (rep f g (n - 1) (f m))

-- now some other cool functions
run :: Music a -> Music a
run = rep (transpose 5) (offset tn) 8

cascade :: Music a -> Music a
cascade = rep (transpose 4) (offset en) 8 . run

cascades :: Music a -> Music a
cascades = rep id (offset sn) 2 . cascade

funkGroove :: Music Pitch
funkGroove = tempo 3 $ cut 8 $ forever composition
  where p1 = perc LowTom qn
        p2 = perc AcousticSnare en
        composition = ((p1 :+: qnr :+: p2 :+: qnr :+: p2
                        :+: p1 :+: p1 :+: qnr :+: p2 :+: enr)
                      :=: roll en (perc ClosedHiHat 2))
