import Data.Char

gcContent :: ([Char]) -> Float
gcContent str =
  (sum [if c == 'C' || c == 'G' then 1 else 0 | c <- str]
  ) /
  (fromIntegral $ length str)

-- main = do
--   print $ gcContent "CCACCCTCGTGGTATGGCTAGGCATTCAGGAACCGGAGAACGCTTCAGACCAGCCCGGAC\
--                     \TGGGAACCTGCGGGCAGTAGGTGGAAT"

startsWith :: (Eq a) => a -> [a] -> Bool
startsWith c (x:_) = (x == c)


insertNewEntry :: [(String, String)] -> String -> [(String, String)]
insertNewEntry entries (_:name) = (name, ""):entries


updateLastEntry :: [(String, String)] -> String -> [(String, String)]
updateLastEntry (e:entries) line =
  ((fst e), (snd e) ++ line) : entries


parseLine :: [(String, String)] -> String -> [(String, String)]
parseLine entries line
  | (startsWith '>' line) = insertNewEntry line entries
  | otherwise             = updateLastEntry line entries


parseLines input =
  foldl (parseLine) [] (lines input)


main = do
  contents <- getContents
  print $ parseLines contents
