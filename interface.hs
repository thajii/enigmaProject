import Components (Walze, walze1, walze2, walze3, walze4, walze5, Walzenkombi, Umkehrwalze, umkehrwalze1, umkehrwalze2, umkehrwalze3, verschluessleString)
--hier einen Weg finden einfach alles zu importieren, die Länge triggert
import Data.Char (toUpper)

--Funktionen um Eingabestrings in Werte zu übersetzen String -> Component
convertWalze :: Char -> Walze
convertWalze '1' = walze1  
convertWalze '2' = walze2 
convertWalze '3' = walze3 
convertWalze '4' = walze4 
convertWalze '5' = walze5 
convertWalze  _  = error "Bitte eine andere Walze (1-5) wählen"

convertWalzenKombi :: String -> Walzenkombi 
convertWalzenKombi (x:xs) = (convertWalze x, convertWalze (head xs), convertWalze (head xs))

convertUmkehrwalze :: String -> Umkehrwalze 
convertUmkehrwalze (x:xs) | x == '1' = umkehrwalze1 
                          | x == '2' = umkehrwalze2 
                          | x == '3' = umkehrwalze3 
                          | otherwise = error "Bitte eine andere Umkehrwalze (1-3) wählen"

main :: IO ()
main = 
    do 
       putStrLn "Bitte die Walzenstellung (Walzen 1-5 möglich) angeben | Bsp: >>123"
       walzenString <- getLine--input
       putStrLn "Bitte die Umehrwalze angeben (1-3) | Bsp: >>1"
       umkehrwalzenString <- getLine--input
       --putStrLn "Bitte die Steckverbindungen angeben | Bsp: >>??"
       --input
       putStrLn "Bitte den zu ver-/entschlüsselnden Text ohne Leerzeichen eingeben | Bsp: >>HELLO"
       text <- getLine --input
       putStrLn (verschluessleString (map toUpper text) (convertWalzenKombi walzenString)  (convertUmkehrwalze umkehrwalzenString))
       main
       
