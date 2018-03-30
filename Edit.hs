-- A miniature editor.  A homage to Unix ed.

main :: IO ()
main  =  interact edit

edit :: String -> String
edit user   =  display texts
  where
  texts     =  [[]] : perform commands texts
  commands  =  decode user

data Command  =  Ins Int String | Del Int | Undo

type Text = [String]

decode :: String -> [Command]
decode  =  map (dec . buffer) . lines
  where
  dec ('d':' ':n )  =  Del (read n)
  dec ('i':' ':ns)  =  Ins (read n) s
    where
    (n,s)  =  breakAt ' ' ns
  dec "u" = Undo

buffer :: [a] -> [a]
buffer xs  =  foldl const xs xs

breakAt :: Eq a => a -> [a] -> ([a],[a])
breakAt e (x:xs)  |  e==x       =  ([],xs)
                  |  otherwise  =  (x:ys,zs)
  where
  (ys,zs)  =  breakAt e xs

perform :: [Command] -> [[Text]] -> [[Text]]
perform =  zipWith perf
  where
  perf (Del n) h@(t:_)    =  (take (n-1) t ++ drop n t) : h
  perf (Ins n s) h@(t:_)  =  (take n t ++ [s] ++ drop n t) : h
  perf Undo [t] = [t]
  perf Undo (t:h) = h

display :: [[Text]] -> String
display  =  concat . map disp
  where
  disp (t:_)    =  unlines (zipWith line [0..] t) ++
               "ed> "
  line n s  =  show n ++" "++ s

