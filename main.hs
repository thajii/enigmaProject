import Data.Char ( ord )

validateInput :: [(Char, Char)] -> Bool
validateInput [] = True
validateInput xs    | validateLength xs  = True -- && validateNoDoubles xs
                    | otherwise = False

validateLength :: [(Char, Char)] -> Bool
validateLength xs   | length xs <= 24 = True
                    | otherwise = False

--validateNoDoubles :: [(Char, Char)] -> Bool
--validateNoDoubles (a:b:xs) |

preEncodeString :: [(Char, Char)] -> [(Char, Int)]
preEncodeString [] = []
preEncodeString (x:xs)  | ord(fst(x)) > ord(snd(x)) = [(fst(x), (ord(fst(x)) - ord(snd(x))))] ++ preEncodeString xs
                        | otherwise = [(fst(x), (ord(snd(x)) - ord(fst(x))))] ++ preEncodeString xs

main :: IO()
main = do
    print("Erfolgreich kompiliert")
    print(preEncodeString[('A','E'),('B','Z')])
