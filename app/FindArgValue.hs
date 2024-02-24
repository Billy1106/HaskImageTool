module FindArgValue (findArgValue) where

-- Checks if a string starts with a prefix
startsWith :: String -> String -> Bool
-- E.g startsWith "--input=" "--input=inputs/cat.jpg" => True because the second string starts with the first string
startsWith "" _ = True
startsWith _ "" = False
startsWith (p:ps) (x:xs) = (p == x) && startsWith ps xs

-- Custom function to remove the prefix from a string
-- E.g removePrefix "--input=" "--input=inputs/cat.jpg" => "inputs/cat.jpg"
removePrefix :: String -> String -> String
removePrefix prefix str
  | startsWith prefix str = drop (length prefix) str
  | otherwise = str -- If the prefix is not found, return the original string (but this should not happen)

findArgValue :: String -> [String] -> String
-- E.g findArgValue "--input=" ["--input=inputs/cat.jpg", "--output=outputs/resized_cat.png"] => "inputs/cat.jpg"
-- E.g findArgValue "--output=" ["--input=inputs/cat.jpg", "--output=outputs/resized_cat.png"] => "outputs/resized_cat.png"
-- If the prefix is found in the list of args, return the first arg that has the prefix removed
findArgValue prefix args = head [removePrefix prefix arg | arg <- args, startsWith prefix arg]

-- startsWith prefix arg to check if the prefix is present in the arg
    -- If it is, remove the prefix from the arg using removePrefix prefix arg
    -- Return the result of removePrefix prefix arg
-- If the prefix is not found, return the original string (but this should not happen)