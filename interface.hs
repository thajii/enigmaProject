import Components (Walze, walze1, walze2, walze3, walze4, walze5, Walzenkombi, Umkehrwalze, umkehrwalze1, umkehrwalze2, umkehrwalze3, verschluessleString, plugboard, changePlugboard)
--hier einen Weg finden einfach alles zu importieren, die Länge triggert
import Data.Char (toUpper)
import Data.List (sort)

--Funktionen um Eingabestrings in Components zu übersetzen
convertWalze :: Char -> Walze
convertWalze '1' = walze1  
convertWalze '2' = walze2 
convertWalze '3' = walze3 
convertWalze '4' = walze4 
convertWalze '5' = walze5 
convertWalze  _  = error "Bitte eine andere Walze (1-5) wählen"

convertWalzenKombi :: String -> Walzenkombi 
convertWalzenKombi (x:y:xs) = (convertWalze x, convertWalze y, convertWalze (head xs))

convertUmkehrwalze :: String -> Umkehrwalze 
convertUmkehrwalze (x:xs) | x == '1' = umkehrwalze1 
                          | x == '2' = umkehrwalze2 
                          | x == '3' = umkehrwalze3 
                          | otherwise = error "Bitte eine andere Umkehrwalze (1-3) wählen"

--Alle gültigen Eingabekombinationen der Walzen
moeglicheWalzenKombis :: [String]
moeglicheWalzenKombis = [a:b:c:[] |a <-['1'..'5'], b <-['1'..'5'], c <-['1'..'5']]

--Alle gültigen Eingabemöglichkeiten für Umkehrwalzen
moeglicheUmkehrWalzen:: [String]
moeglicheUmkehrWalzen = ["1","2","3"]
 
--Überprüfen der Eingabe des Plugboards
plugboardCheck :: String -> Bool 
plugboardCheck [] = True 
plugboardCheck string = even (length string) && nurBuchstaben string && keineDuplikate (sort string) -- lazy evaluation

--Hilfsfunktion für plugboardCheck
keineDuplikate :: String -> Bool -- False wenn es Duplikate gibt
keineDuplikate [] = True
keineDuplikate (x:y:xs) | x == y = False 
                        | otherwise = True && keineDuplikate (y:xs) 

--Hilfsfunktion für plugboardCheck

nurBuchstaben:: String -> Bool 
nurBuchstaben [] = True
nurBuchstaben (x:xs) | x `notElem` ['A'..'Z'] ++ ['a'..'z']= False
                     | otherwise = True && nurBuchstaben xs 

--Textfilter welcher um ungewollte Zeichen zu entfernen
filterText :: String -> String
filterText string = [x | x <- string, x `notElem` ",.?!-:;1234567890ß</>_ +*#ÜÄÖüäö@$%&()=\""]     

-- Main-"Loop"
main :: IO ()
main = 
    do putStrLn "Bitte die Walzenstellung (1-5 möglich) angeben | Bsp: >>123"
       walzenString <- getLine
       if  walzenString `notElem` moeglicheWalzenKombis
           then 
               do putStrLn "Fehler: Walzenauswahl ungültig!"
                  main
            else 
                do putStrLn "Walzenauswahl O.K."
                   putStrLn "Bitte die Umkehrwalze angeben(1-3 möglich) | Bsp: >>1"
                   umkehrwalzenString <- getLine
                   if umkehrwalzenString `notElem` moeglicheUmkehrWalzen
                       then 
                           do putStrLn "Fehler: Umkehrwalze ungültig!"
                              main
                        else
                            do putStrLn "Umkehrwalzenauswahl O.K."
                               putStrLn "Bitte die Steckverbindungen (ohne Dopplungen) angeben | Bsp: >>THLS"
                               plugboardString <- getLine 
                               if not(plugboardCheck plugboardString)
                                   then 
                                       do putStrLn "Fehler: Steckverbindungen ungültig!"
                                          main
                                    else
                                        do putStrLn "Steckverbindungen O.K."
                                           putStrLn "Bitte den zu ver-/entschlüsselnden Text eingeben"
                                           putStrLn "Achtung: Zahlen bitte ausschreiben, Satzzeichen werden entfernt!"
                                           text <- getLine 
                                           putStrLn " "
                                           putStrLn "Ergebnis:" 
                                           let plugboardVerwendet =  changePlugboard (map toUpper plugboardString) plugboard
                                           putStrLn (verschluessleString (map toUpper (filterText text)) (convertWalzenKombi walzenString)  (convertUmkehrwalze umkehrwalzenString) plugboardVerwendet)
                                           putStrLn "________________________________________________________ "
                                           putStrLn " "
                                           main
                            
                 