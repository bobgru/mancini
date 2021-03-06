module BabyElephantWalk where
import Euterpea
-- import HSoM -- TODO how to import this?


graceNote :: Int -> Music Pitch -> Music Pitch
graceNote n (Prim (Note d p)) =
          note (d/8) (trans n p) :+: note (7*d/8) p
graceNote _ _ =
          error "Can only add a grace note to a note."

graceNote' :: Music Pitch -> Music Pitch -> Music Pitch
graceNote' m1 m2 = cut (d/8) m1 :+: remove (d/8) m2
  where d = dur m2 

stac :: Music Pitch -> Music Pitch
stac = Modify (Phrase [Art (Staccato (1/2))])

lh1 = line $ [ f  2 en :=: c 3 en
             , f  2 en :=: c 3 en
             , gs 2 en
             , a  2 en ]

lh3 = line $ [ f  2 en :=: c 3 en
             , f  2 en :=: c 3 en
             , cs 3 en
             , d  3 en ]

lh7 = line $ [ f  2 en :=: c 3 en
             , f  2 en :=: c 3 en
             , ds 3 en
             , e  3 en ]

lh10 = line $  [ f  2 en :=: c 3 en
               , f  2 en :=: c 3 en
               , bf 2 en :=: f 3 en
               , bf 2 en :=: f 3 en ]

lh12 = line $  [ f  2 en :=: c 3 en
               , f  2 en :=: c 3 en
               , gs 2 en
               , c  2 en ]

lh4 = fmap (trans 5) lh1

lh8 = fmap (trans 7) lh1

lh6 = line $ [ bf 2 en :=: f 3 en
             , bf 2 en :=: f 3 en
             , gs 2 en
             , a  2 en ]

lh17 = line $  [ bf 2 en :=: f  3 en
               , bf 2 en :=: f  3 en
               , cs 3 en
               , d  3 en ]

lh9 = line $ [ c  3 en :=: g 3 en
             , c  3 en :=: g 3 en
             , cs 3 en
             , d  3 en ]

lh11 = line $  [ a  2 en :=: e 3 en
               , a  2 en :=: e 3 en
               , g  2 en :=: d 3 en
               , g  2 en :=: d 3 en ]

lh13 = line $  [ f  2 qn
               , af 2 en
               , f  2 en
               , bf 2 en
               , af 2 dqn ]

lh14 = line $ [ f  2 qn
              , af 2 en
              , f  2 en
              , bf 2 en
              , bf 2 en
              , af 2 en
              , f  2 en ]

lh15 = (line $  [ af 2 en
                , bf 2 en
                , bf 2 hn
                , f  2 en
                , af 2 en ])
       :=:
       (line $  [ enr
                , f  3 en
                , e  3 en
                , ef 3 en
                , d  3 en
                , df 3 en
                , c  3 en
                , b  2 en ])

lh16 = line $ [ af 2 en :=: bf 2 en
              , bf 2 en :=: f  3 en
              , cs 3 en
              , d  3 en ]

lh18 = line $  [ f  2 en :=: c  3 en
               , enr
               , c  3 qn
               , d  3 qn
               , c  3 qn ]

lh19 = line $  [ f  2 wn :=: c  3 wn ]

leftPage1 = stac $ line $ concat $
  [ take 15 (repeat lh1), [lh3]
  , take  3 (repeat lh4), [lh6]
  , take  3 (repeat lh1), [lh7]
  , [lh8, lh9, lh4, lh6]

  , take 4 (repeat lh1)

  , take 15 (repeat lh1), [lh3]
  , take  3 (repeat lh4), [lh6]
  , take  3 (repeat lh1), [lh7]
  , [lh8, lh9, lh4, lh6]

  , take 2 (repeat lh1)
  , [lh10, lh11]
  , take 2 (repeat lh1) ]

leftPage3 =
  line $ concat $
    [ [lh1, lh12]
    , [lh13]
    , [wnr]
    , [lh14]
    , [lh15] ]

leftPage4 = 

  (stac $ line $ concat $
    [ [lh16, lh17, lh17, lh6]
    , take 3 (repeat lh1), [lh3]
    , [lh8, lh9, lh4, lh6]
    , take 7 (repeat lh1), [lh3]
    , take 3 (repeat lh4), [lh6]
    , take 3 (repeat lh1), [lh7]
    , [lh8, lh9, lh4, lh6] ])
  :+:
  (line $ concat $
    [ [lh18]
    , [lh19] ])

leftLine = line $
  [ leftPage1
  , leftPage3
  , leftPage4
  ]

rh1 = line $ [ qnr
             , f 6 qn :=: a 6 qn
             , qnr 
             , ef 6 en :=: g 6 en
             , ef 6 en :=: g 6 en ]

theme1 st gr =
  line $ [ gr $ f 4 den
         , st (a 4 sn)
         , st (c 5 en)
         , st (f 5 en)
         , a 5 en
         , g 5 en
         , st (f 5 en)
         , st (d 5 en) ]

theme1_v2 = theme1 id   id
theme1_v1 = theme1 stac id
theme1_v3 = fmap (trans 12) $ theme1 id (graceNote (-1))

rh3' = line $ [ b 4 en, c 5 en ]
rh3  = fmap (trans 12) rh3'

rh4' = line $ [ c  5 dhn, d  5 en, af 4 en ]
rh4  = fmap (trans 12) rh4'

rh5x = line $ [ af 4 dhn, af 4 en, d  5 en ]
rh5  = fmap (trans 12) rh5x

rh5' =  rh5x
        :=:
        (line $  [ qnr
                 , d  4 qn :=: bf 3 qn
                 , qnr
                 , c  4 en :=: af 3 en
                 , enr ])

theme6 gr = line $ [ gr $ af 4 en, f  4 en ]

theme6_v1  = theme6 (graceNote (-1))
theme6_v2   = fmap (trans 12) $ theme6 (graceNote (-1))
theme6_v3 = theme6 id
theme6_v4  = fmap (trans 12) $ theme6 id

theme7 gr12 gr3 = 
  line $ [ gr12 $ g  5 qn
         , gr12 $ g  5 qn
         , gr3  $ e  5 en
         , c  5 en
         , enr
         , c  5 en ]

theme8 gr =
  line $ [ gr $ f  5 qn
         , gr $ f  5 qn
         , ef 5 sn
         , f  5 sn
         , ef 5 sn
         , c  5 sn
         , bf 4 en
         , gs 4 en ]

theme9 gr =
  line $ [ gr $ b  4 qn
         , gr $ b  4 qn
         , bf 4 sn
         , cf 5 sn
         , bf 4 sn
         , af 4 sn
         , f  4 en
         , c  4 en ]

theme10 = 
  line $ [ ef 4 en
         , f  4 en
         , qnr
         , hnr ]

theme7_v1 = fmap (trans 12) $ theme7 id (graceNote (-1))
theme7_v2 = theme7 (graceNote (-1)) (graceNote (-1))
theme7_v3 = theme7 id id
theme7_v4 = fmap (trans 12) $ theme7 id id
theme7_v5 = fmap (trans 12) $ theme7 id (graceNote (-1))

theme8_v1 = fmap (trans 12) $ theme8 id
theme8_v2 = theme8 (graceNote (-1))
theme8_v3 = theme8 id
theme8_v4 = fmap (trans 12) $ theme8 id

theme9_v1 = fmap (trans 12) $ theme9 id
theme9_v2 = theme9 (graceNote (-1))

theme10_v1 = fmap (trans 12 ) $ theme10

rh11 = line $  [ qnr
               , c  3 qn :=: f  3 qn :=: a  3 qn
               , qnr
               , stac (d  3 en :=: ef  3 en :=: g  3 en)
               , stac (d  3 en :=: ef  3 en :=: g  3 en) ]

rh12 = theme10
       :=:
       (stac $ line $ [ qnr
               , d  4 en
               , d  4 en
               , c  4 en
               , c  4 en
               , bf 3 en
               , bf 3 en ])
                
rh13 = line $  [ a  3 en
               , enr
               , f  5 qn :=: a  5 qn
               , qnr
               , ef 5 en :=: g  5 en 
               , ef 5 en :=: g  5 en ]

rh14 = line $  [ qnr
               , f  5 qn :=: a  5 qn
               , qnr
               , enr
               , c  3 en ]

rh15 = line $  [ f  3 qn
               , af 3 en
               , f  3 en
               , bf 3 en
               , af 3 dqn ]

rh16 = line $ [ graceNote' (cs 6 qn) (bf 5 qn :=: d 6 qn :=: f 6 qn)
              , a  5 qn :=: c 6 qn :=: f 6 qn
              , graceNote' (cs 6 qn) (bf 5 en :=: d 6 en :=: f 6 en)
              , (bf 5 en :=: d 6 en :=: f 6 en)
              , a  5 en :=: c 6 en :=: f 6 en
              , enr ]

rh17 = line $ [ f  3 qn
              , af 3 en
              , f  3 en
              , bf 3 en
              , bf 3 en
              , af 3 en
              , f  3 en ]

rh18 = (line $  [ af 3 en
                , bf 3 en
                , bf 3 hn
                , f  3 en
                , af 3 en ])
       :=:
       (line $  [ enr
                , f  4 en
                , e  4 en
                , ef 4 en
                , d  4 en
                , df 4 en
                , c  4 en
                , b  3 en ])

rh19 = (line $  [ bf 3 en
                , enr
                , qnr
                , hnr ])
       :=:
       (line $  [ af 3 dhn
                , af 3 en
                , d  4 en ])

rh20 = line $ [ af 3 en
              , f  3 en
              , qnr
              , hnr ]

rh22 = line $  [ b 5 en
               , c 6 en 
               , qnr
               , qnr
               , enr
               , c 6 en ]

rh26 = line $  [ f  4 en :=: f  5 en
               , enr
               , c  4 qn
               , d  4 qn
               , c  4 qn ]

rh27 = (line $  [ a  3 wn :=: ef 4 wn ])
       :=:
       (hnr :+: chord [d 5 hn, ef 5 hn, g 5 hn, b 5 hn])

rightPage1 = line $ concat $
  [ [rh1]
  , [wnr]
  , [rh1]
  , [wnr]
  , [fmap (trans 12) theme1_v1]
  , [rh3, qnr, hnr]
  , [fmap (trans 12) theme1_v1]
  , [rh4]
  , [rh5]
  , [theme6_v2, qnr, hnr]
  , [fmap (trans 12) theme1_v1]
  , [rh3, qnr, qnr, enr, c 6 en]
  , [theme7_v1] ]

rightPage2 = line $ concat $
  [ [theme8_v1]
  , [theme9_v1]
  , [theme10_v1]
  , [rh11]
  , [wnr]
  , [rh11]
  , [wnr]
  , [theme1_v1]
  , [rh3', qnr, hnr]
  , [theme1_v1]
  , [rh4']
  , [rh5']
  , [theme6_v1, qnr, hnr]
  , [theme1_v1]
  , [rh3', qnr, hnr] ]

rightPage3 = line $ concat $
  [ [theme7_v2]
  , [theme8_v2]
  , [theme9_v2]
  , [rh12]
  , [rh13]
  , [rh14]
  , [rh15]
  , [rh16]
  , [rh17]
  , [rh18]
  , [rh19]
  , [rh20]
  , [theme1_v3]
  , [rh22]
  , [theme7_v5] ]

rightPage4 = line $ concat $
  [ [theme8_v4]
  , [theme1_v2 :=: fmap (trans 12) theme1_v2]
  , [rh3 :=: rh3', qnr, hnr]
  , [theme1_v2 :=: fmap (trans 12) theme1_v2]
  , [rh4 :=: rh4']
  , [rh5 :=: rh5x]
  , [theme6_v4 :=: theme6_v3, qnr, hnr]
  , [theme1_v2 :=: fmap (trans 12) theme1_v2]
  , [rh3 :=: rh3', qnr, qnr, enr, c 6 en :=: c 5 en]
  , [theme7_v4 :=: theme7_v3]
  , [theme8_v4 :=: theme8_v3]
  , [rh26]
  , [rh27] ]

rightLine = line $
  [ rightPage1
  , rightPage2
  , rightPage3
  , rightPage4 ]

main = play $ leftLine :=: rightLine
