import Data.Char


--
-- actual data analysis
--

gcContent :: ([Char]) -> Float
gcContent str =
  (sum [if c == 'C' || c == 'G' then 1 else 0 | c <- str]
  ) /
  (fromIntegral $ length str) * 100.0


entryGcContent :: (String, String) -> (String, Float)
entryGcContent (name, str) = (name, gcContent str)


entryWithMaxValue :: [(String, Float)] -> (String, Float)
entryWithMaxValue [a] = a
entryWithMaxValue [a, b]
  | (snd a) > (snd b) = a
  | otherwise         = b
entryWithMaxValue (e:tail) = (entryWithMaxValue [e, (entryWithMaxValue tail)])


formatResult :: (String, Float) -> String
formatResult (name, value) = name ++ "\n" ++ (show value)


main = do
  contents <- getContents
  putStrLn $ formatResult (entryWithMaxValue (map entryGcContent (parseLines contents)))

--main = do
--  contents <- getContents
--  print $ parseLines contents



--
-- stdin parsing
--

-- Simple "does string start with this char" function.
startsWith :: (Eq a) => a -> [a] -> Bool
startsWith c (x:_) = (x == c)


-- Pops the leading '>' off the name, and returns a new list including an entry
-- with that name. Data starts off as "". New element is at the head of the list
insertNewEntry :: [(String, String)] -> String -> [(String, String)]
insertNewEntry entries (_:name) = (name, ""):entries


-- Updates the head entry by appending `line` to its data.
updateLastEntry :: [(String, String)] -> String -> [(String, String)]
updateLastEntry ((name, dat):others) line =
  (name, dat ++ line) : others


-- Parses `line` and appends to / modifies `entries` accordingly.
-- Lines that start with '>' are new entries, otherwise the entire line is
-- blindly appended to the latest entry's data.
parseLine :: [(String, String)] -> String -> [(String, String)]
parseLine entries line
  | (startsWith '>' line) = insertNewEntry entries line
  | otherwise             = updateLastEntry entries line


-- Takes an entire string of input, chunks it into lines, and gives you a list
-- of entries in (String, String) format.
parseLines input =
  foldl parseLine [] (lines input)


--main = do
--  contents <- getContents
--  print $ parseLines contents
