module MoonRiver where
import Euterpea

prologue = line . take 2 . repeat $ chord [rh1, slurNotes lh1]

slur x = Modify (Phrase [Art (Slurred x)])

slurNotes m =
  let
    ns = lineToList m
    d  = dur m
    ds = map dur ns
    setDur ((rd, nd), (Prim (Note _ p))) = [rest rd, Prim (Note nd p)]
    setDur _ = error $ "slurNotes applied to line with non-Prim"
    restDurs = scanl (+) 0 (init ds)
    noteDurs = map (d -) restDurs
    
  in chord $ map line
           $ map setDur
           $ zip (zip restDurs noteDurs) ns


rh1 = dhnr
lh1 = line [c 2 en, g 2 en, e 3 qn, g 2 qn]
lh1' = chord [c 2 dhn
            ,line [enr, g 2 en, e 3 qn, g 2 qn]]

lh1'' = chord [c 2 dhn
            ,line [enr, g 2 (5/8)]
            ,line [enr, enr, e 3 hn]
            ,line [enr, enr, qnr, g 2 qn]
            ]
lh1''' = slur (7/3) lh1




rh3 = g 4 dhn :+: rh4
lh3 =   slurNotes (line [c 2 en, g 2 en, e 3 qn])
    :+: slurNotes (line $ concat [[c 2 en, b 2 en], lineToList lh4])

rh4 = slurNotes $ line [d 5 qn, c 5 hn]
lh4 = line [a 2 en, e 2 en, c 3 en, e 3 en, a 3 qn]

rh5 = line [b 4 dqn, a 4 en, g 4 en, f 4 en]
lh5 = line [f 2 en, c 3 en, a 3 hn]

rh6 = line [g 4 hn, c 4 qn]
lh6 = line [e 2 en, c 3 en, g 3 hn]

rh9 = d 4 dhn
lh9 = line [b 2 en, b 3 en, d 3 en, f 3 en, a 3 qn]

rh10 = line [d 4 hn, e 4 qn]
lh10 = line [e 2 en, b 3 en, d 3 en, gs 3 en, b 3 qn]

rh11 = c 4 dhn
lh11 = line [a 2 en, e 3 en, a 3 qn, a 2 qn]

rh12 = line [g 4 qn, e 4 dqn, d 4 en]
lh12 = line [g 2 en, e 3 en, bf 3 qn, g 2 qn]

rh13 = c 4 dhn
lh13 = line [f 2 en, c 3 en, a 4 qn, f 2 qn]

rh14 = line [g 4 qn, e 4 dqn, d 4 en]
lh14 = line [bf 1 en, f 2 en, d 3 en, bf 3 en, c 4 qn]

rh15 = line [chord [c 4 qn, a 3 qn, e 3 qn]
            ,chord [a 3 qn, c 4 qn, e 4 qn]
            ,chord [c 4 qn, e 4 qn, g 4 qn]]
lh15 = line [a 2 hn, g 2 qn]

rh16 = chord [line [c 5 en, b 4 en, b 4 dqn, a 3 en]
             ,line [e 3 qn, ds 3 hn]]
lh16 = chord [line [a 3 hn, qnr]
             ,line [fs 2 qn, b 2 qn, b 1 qn]]

rh17 = chord [line [b 4 en, c 5 en, c 5 dqn, g 4 en]
             ,line [d 4 qn, cs 4 hn]]
lh17 = chord [line [g 3 hn, qnr]
             ,line [e 2 qn, a 2 qn, a 1 qn]]

rh18 = chord [line [a 4 dhn]
             ,line [c 4 qn, b 3 hn]]
lh18 = chord [line [f 3 hn, qnr]
             ,line [d 2 qn, g 2 qn, g 1 qn]]

rh19 = g 4 dhn
lh19 = line [c 2 en, g 2 en, e 3 qn, c 2 en, b 1 en]

rh20 = line [d 5 qn, c 5 hn]
lh20 = line [a 1 en, e 2 en, c 3 en, e 3 en, a 3 qn]

rh21 = line [b 4 dqn, a 4 en, g 4 en, f 4 en]
lh21 = line [f 2 en, c 3 en, a 3 hn]

rh22 = line [g 4 hn, c 4 qn]
lh22 = line [e 2 en, c 3 en, g 3 hn]

rh23 = line [b 4 dqn, a 4 en, g 4 en, f 4 en]
lh23 = line [f 2 en, c 3 en, a 3 hn]

rh24 = line [g 4 hn, c 4 qn]
lh24 = line [e 2 en, c 3 en, g 3 hn]

rh25 = d 3 dhn
lh25 = line [b 1 en, b 2 en, d 3 en, f 3 en, a 3 qn]

rh26 = line [d 4 hn, e 4 qn]
lh26 = line [e 2 en, b 2 en, d 3 en, gs 3 en, b 3 qn]

rh27 = chord [c 4 dhn, line [a 3 qn, g 3 hn]]
lh27 = line [chord [a 2 qn, e 3 qn], chord [a 2 hn, e 3 hn]]

rh28 = chord [line [e 4 hn, g 4 qn]
             ,line [chord [a 3 qn, c 4 qn], chord [a 3 hn, c 4 hn]]]
lh28 = line [chord [g 2 qn, e 3 qn], chord [g 2 hn, e 3 hn]]

rh29 = chord [c 5 dhn
             ,line [chord [e 4 qn, a 4 qn]
                   ,chord [e 4 hn, a 4 hn]]]
lh29 = line [chord [fs 2 qn, e 3 qn]
            ,chord [fs 2 hn, e 3 hn]]

rh30 = chord [line [d 5 hn, c 5 qn]
             ,chord [ef 4 dhn, a 4 dhn]]
lh30 = line [f 2 dhn, df 3 dhn]

rh31 = g 4 dhn
lh31 = line [e 2 en, c 3 en, g 4 qn, e 2 qn]

rh32 = line [g 4 qn, b 4 en, a 4 en, g 4 en, f 4 en]
lh32 = line [f 2 en, c 3 en, a 3 hn]

rh33 = g 4 dhn
lh33 = line [e 2 en, c 2 en, g 3 qn, e 2 qn]

rh34 = line [g 4 en, c 4 en, b 4 en, a 4 en, g 4 en, f 4 en]
lh34 = line [f 2 en, c 3 en, a 3 hn]

rh35 = g 4 dhn
lh35 = line [e 2 en, c 3 en, g 3 qn, e 2 qn]

rh36 = c 4 dhn
lh36 = line [a 1 en, e 2 en, c 3 qn, a 1 qn]

rh37 = line [f 4 qn, d 4 hn]
lh37 = line [d 2 en, a 2 en, f 3 qn, d 2 en]

rh38 = line [d 4 hn, e 4 qn]
lh38 = line [g 2 en, f 3 en, b 3 qn, g 2 qn]

rh39 = c 4 dhn
lh39 = line [c 2 en, g 2 en, e 3 qn, g 2 qn]

rh40 = c 4 dhn
lh40 = line [c 2 en, g 2 en, e 3 qn, g 2 qn]

epilogue = line [chord [rh41, lh41], chord [rh42, lh42]]

rh41 = chord [c 4 dhn, line [a 3 qn, g 3 qn, f 3 qn]]
lh41 = line [chord [f 2 qn, c 3 qn]
            ,chord [e 2 qn, b 2 qn]
            ,chord [d 2 qn, a 2 qn]]

rh42 = chord [c 4 dhn, e 3 dhn]
lh42 = chord [c 3 dhn, g 2 dhn]

main = play $ Modify (Phrase [Dyn (StdLoudness P)]) $
  prologue
  :+:
  (line . take 2 $ repeat $
    (line $ concat [ [rh3]
                   -- , rh4]
                   , [rh5, rh6, rh5, rh6]
                   , [rh9, rh10, rh11, rh12]
                   , [rh13, rh14, rh15, rh16]
                   , [rh17, rh18, rh19]
                   , [rh20, rh21, rh22]
                   , [rh23, rh24, rh25]
                   , [rh26, rh27, rh28]
                   , [rh29, rh30, rh31]
                   , [rh32, rh33, rh34]
                   , [rh35, rh36, rh37, rh38]
                   , [rh39, rh40]
                   ])
    :=:
    (line $ concat [ [lh3]
                   -- , lh4]
                   , [lh5, lh6, lh5, lh6]
                   , [lh9, lh10, lh11, lh12]
                   , [lh13, lh14, lh15, lh16]
                   , [lh17, lh18, lh19]
                   , [lh20, lh21, lh22]
                   , [lh23, lh24, lh25]
                   , [lh26, lh27, lh28]
                   , [lh29, lh30, lh31]
                   , [lh32, lh33, lh34]
                   , [lh35, lh36, lh37, lh38]
                   , [lh39, lh40]
                   ]))
  :+:
  epilogue
