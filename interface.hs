import Components (Walze, walze1, walze2, walze3, walze4, walze5, Walzenkombi, Umkehrwalze, umkehrwalze1, umkehrwalze2, umkehrwalze3, verschluessleString)
--hier einen Weg finden einfach alles zu importieren, die Länge triggert

--Funktionen um Eingabestrings in Werte zu übersetzen String -> Component
convertWalze :: Char -> Walze
convertWalze '1' = walze1  
convertWalze '2' = walze2 
convertWalze '3' = walze3 
convertWalze '4' = walze4 
convertWalze '5' = walze5 

convertWalzenKombi :: String -> Walzenkombi 
convertWalzenKombi (x:xs) = (convertWalze x, convertWalze (head xs), convertWalze (head xs))

convertUmkehrwalze :: String -> Umkehrwalze 
convertUmkehrwalze string | string == "1" = umkehrwalze1 
                          | string == "2" = umkehrwalze2 
                          | string == "3" = umkehrwalze3 

main :: IO ()
main = do
       putStrLn "Bitte die Walzenstellung (3 Walzen) angeben | Bsp: >>123"
       walzenString <- getLine--input
       putStrLn "Bitte die Umehrwalze angeben | Bsp: >>1"
       umkehrwalzenString <- getLine--input
       --putStrLn "Bitte die Steckverbindungen angeben | Bsp: ??"
       --input
       putStrLn "Bitte den zu ver-/entschlüsselnden Text in Grossbuchstaben ohne Leerzeichen eingeben | >>HELLO"
       text <- getLine --input
       putStrLn (verschluessleString text (convertWalzenKombi walzenString)  (convertUmkehrwalze umkehrwalzenString))
       
