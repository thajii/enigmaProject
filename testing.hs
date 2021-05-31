import Data.List (sort)

{-
main :: IO ()
main = do   
    line <- getLine  
    if null line  
        then print("Fehler") return ()
               
        else do  
            putStrLn $ reverseWords line  
            main  
  
reverseWords :: String -> String  
reverseWords = unwords . map reverse . words  
--}
{-
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
       putStrLn (verschluessleString (map toUpper text) (convertWalzenKombi walzenString)  (convertUmkehrwalze umkehrwalzenString) plugboard)
       main
-}
--{-
import Components (Walze, walze1, walze2, walze3, walze4, walze5, Walzenkombi, Umkehrwalze, umkehrwalze1, umkehrwalze2, umkehrwalze3, verschluessleString, plugboard)
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

e1 :: [[Char]]
e1 = ["123","234"]
e2 :: [[Char]]
e2 = ["1","2","3"]




moeglicheWalzenKombis :: [String]
moeglicheWalzenKombis = [a:b:c:[] |a <-['1'..'5'], b <-['1'..'5'], c <-['1'..'5'] ]

--Überprüfen der Eingabe des Plugboards
plugboardCheck :: String -> Bool -- Keine Duplikate und Länge mod 2 == 0
plugboardCheck [] = True -- es gab einen Fehler bei ausbleibender Angabe ------> Nochmal drübergucken mit der change Plugboard methdoe
plugboardCheck string = even (length string) && keineDuplikate (sort string) 
--Hilfsfunktion für plugboardCheck
keineDuplikate :: String -> Bool --False wenn es Duplikate gibt, 
keineDuplikate [] = True
keineDuplikate (x:y:xs) | x == y = False 
                        | otherwise = True && keineDuplikate (y:xs) -- VSC Anmerkung hier ignorieren


filterText :: String -> String
filterText string = [x | x <- string, x `notElem` ",.?!-:;1234567890ß</>_ +*#ÜÄÖüäö@$%&()="]


string :: [Char]
string = "schätzchen ich hoff wir sehen uns bald, wie ist draußen das wetter?"


main :: IO ()
main = 
    do putStrLn "-------------------------"
       --print (filterText string)
       --print moeglicheWalzenKombis
       --print (plugboardOK "QR")
       --print (plugboardOK "ACDCT")
       --print (plugboardOK "QWERTZUIOPLKJHGFDSAYXCVBNM")
       print (even (length []))
       print (sort "A5CD1C")
     
       print (plugboardCheck "ACDC")
       print (plugboardCheck "ACCA")
       print('1' == 'C')
       