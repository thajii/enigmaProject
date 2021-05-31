import Components (Walze, walze1, walze2, walze3, walze4, walze5, Walzenkombi, Umkehrwalze, umkehrwalze1, umkehrwalze2, umkehrwalze3, verschluessleString, plugboard, changePlugboard)
--hier einen Weg finden einfach alles zu importieren, die Länge triggert
import Data.Char (toUpper)
import Data.List (sort)
import Data.Char (ord)

--Funktionen um Eingabestrings in Werte zu übersetzen String -> Component
convertWalze :: Char -> Walze
convertWalze '1' = walze1  
convertWalze '2' = walze2 
convertWalze '3' = walze3 
convertWalze '4' = walze4 
convertWalze '5' = walze5 
convertWalze  _  = error "Bitte eine andere Walze (1-5) wählen"

convertWalzenKombi :: String -> Walzenkombi --hier die ersten drei elemente für robustheit
convertWalzenKombi (x:y:xs) = (convertWalze x, convertWalze y, convertWalze (head xs))-- dritte Walze?

convertUmkehrwalze :: String -> Umkehrwalze --hier immer den Head nehmen für Robustheit
convertUmkehrwalze (x:xs) | x == '1' = umkehrwalze1 
                          | x == '2' = umkehrwalze2 
                          | x == '3' = umkehrwalze3 
                          | otherwise = error "Bitte eine andere Umkehrwalze (1-3) wählen"
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

--Alle gültigen Eingabekombinationen der Walzen
moeglicheWalzenKombis :: [String]
moeglicheWalzenKombis = [a:b:c:[] |a <-['1'..'5'], b <-['1'..'5'], c <-['1'..'5']]

--Alle gültigen Eingabemöglichkeiten für Umkehrwalzen
moeglicheUmkehrWalzen:: [String]
moeglicheUmkehrWalzen = ["1","2","3"]
 
--Überprüfen der Eingabe des Plugboards
plugboardCheck :: String -> Bool -- Keine Duplikate und Länge mod 2 == 0
plugboardCheck [] = True -- es gab einen Fehler bei ausbleibender Angabe ------> Nochmal drübergucken mit der change Plugboard methdoe
plugboardCheck string = even (length string) && keineDuplikate (sort string) && nurBuchstaben string
--Hilfsfunktion für plugboardCheck
keineDuplikate :: String -> Bool --False wenn es Duplikate gibt, 
keineDuplikate (x:y:xs) | x == y = False 
                        | otherwise = True || keineDuplikate (y:xs) -- VSC Anmerkung hier ignorieren
--Hilfsfunktion für plugboardCheck
nurBuchstaben:: String -> Bool --siehe name der Funktion
nurBuchstaben (x:xs) | x `notElem` ['A'..'Z'] ++ ['a'..'z']= False--kleine buchstaben auch io
                     | otherwise = True || nurBuchstaben xs -- VSC Anmerkung hier ignorieren
--Noch eine Hilfsunktion für plugboardCheck
createFirstLists :: String -> String
createFirstLists [] = []
createFirstLists (x:y:xs) = [x] ++ createFirstLists xs 

createSecondLists :: String -> String
createSecondLists [] = [] []
createSecondLists (x:y:xs) = [y] ++ createSecondLists xs

--Quicksort 
qst[] = []
qst (x:xs) = qstsmaller ++ [x] ++ qstlarger
    where
        smaller = [a | a <- xs, a <= x]
        larger = [b | b <- xs, b > x]

--Letters in ASCII


--ASCII in Letters

--Textfilter welcher um ungewollte Zeichen zu entfernen
filterText :: String -> String
filterText string = [x | x <- string, x `notElem` ",.?!-:;1234567890ß</>_ +*#ÜÄÖüäö@$%&()=\""]      

main :: IO ()--neuer Main-"loop"
main = 
    do putStrLn "Bitte die Walzenstellung (1-5 möglich) angeben | Bsp: >>123"
       walzenString <- getLine--input
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
                                           putStrLn " "
                                           putStrLn "Ergebnis:" 
                                           let plugboardVerwendet =  changePlugboard (map toUpper plugboardString) plugboard
                                           putStrLn (verschluessleString (map toUpper (filterText text)) (convertWalzenKombi walzenString)  (convertUmkehrwalze umkehrwalzenString) plugboardVerwendet)
                                           putStrLn "________________________________________________________ "
                                           putStrLn " "
                                           main
                            
                 