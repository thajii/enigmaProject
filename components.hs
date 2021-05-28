module Components where-- Module for the mechanical parts of the enigma

--Test
alphabet :: [Char]
alphabet = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"

-- Data & Types
data Walze = Walze {rand_alphabet:: String, klar_alphabet :: String, umspringbuchstabe ::Char} -- umspringbuchstabe ggf als Übergasbeparameter
    deriving(Show)--testen ob die Walze saich drehen lässt mit mittels show/ terminalausgabe

type Walzenkombi = (Walze, Walze, Walze)--hier eine Kombination für das Drehen aller Walzen

type Umkehrwalze = [(Char, Char)]

type Plugboard = [(Char,Char)]

-- Walzen
walze1 :: Walze
walze1 = Walze "EKMFLGDQVZNTOWYHXUSPAIBRCJ" alphabet 'Q' 

walze2 :: Walze
walze2 = Walze "AJDKSIRUXBLHWTMCQGZNPYFVOE" alphabet 'E' 

walze3 :: Walze
walze3 = Walze "BDFHJLCPRTXVZNYEIWGAKMUSQO" alphabet 'V' 

walze4 :: Walze
walze4 = Walze "VZBRGITYUPSDNHLXAWMJQOFECK" alphabet 'K' 

walze5 :: Walze
walze5 = Walze "ESOVPZJAYQUIRHXLNFTGKDCMWB" alphabet 'Q' 

-- Walzenfunktionen
dreheWalze :: Walze -> Walze -- auf der linken Funktionsseite muss ein Konstruktora ufruf stehen, da führt kein weg dran vorbei
dreheWalze (Walze rand_alphabet klar_alphabet umspringbuchstabe) = Walze rand_alphabet_neu klar_alphabet umspringbuchstabe 
        where rand_alphabet_neu = tail rand_alphabet ++ [head rand_alphabet]
              --klar_alphabet_neu = tail klar_alphabet ++ [head klar_alphabet]-- beide Alphabete müssen verschoben werden: bspw. W1 A=E -> B=K

checkObDrehen :: Walze -> Bool--sicher?
checkObDrehen walze = umspringbuchstabe walze == head (rand_alphabet walze)

dreheAlleWalzen :: (Walze, Walze, Walze) -> (Walze, Walze, Walze)--hier drehe 1 und checken ob die anderen gedreht werden müssen falls ja drehen
dreheAlleWalzen (w1, w2, w3) = (dreheWalze w1,
                                 if checkObDrehen w1 == True 
                                    then dreheWalze w2 
                                        else w2, 
                                 if checkObDrehen w2 == True 
                                    then dreheWalze w3 
                                        else w3)

sucheInWalze :: [(Char,Char)] -> Char -> Char
sucheInWalze [] _ = '!' -- für Fehler
sucheInWalze tupelliste c | snd (head tupelliste) == c = fst (head tupelliste)
                          | otherwise = sucheInWalze (tail tupelliste) c

uebersetzen :: Char -> Walze -> Char
uebersetzen c walze =  sucheInWalze (zip random klar) c-- walze rand_alphabet und klar_alphabet zippen 
                where random = rand_alphabet walze      -- dann in Hilfsfunktion nach der Stelle suchen, wo Eingabe = KlarBuchstabe
                      klar = klar_alphabet walze        -- dann im durch das Tupel das "zufällige" Gegenstück zurrückgeben.

uebersetzenMitKombi :: Char -> Walzenkombi -> Char
uebersetzenMitKombi c (w1, w2, w3) = uebersetzen (uebersetzen (uebersetzen c w1) w2) w3-- Buchstaben einmal duch alle Walzen durch übersetzten


zurueckuebersetzen :: Char -> Walze -> Char
zurueckuebersetzen c walze = sucheInWalze (zip klar random) c -- genauso wie uebersetzen 
                        where random = rand_alphabet walze    -- gedacht für den Aufruf nach Verwendung der Umkerwalze
                              klar = klar_alphabet walze

zurueckuebersetzenMitKombi :: Char -> Walzenkombi -> Char
zurueckuebersetzenMitKombi c (w1, w2, w3) = zurueckuebersetzen (zurueckuebersetzen (zurueckuebersetzen c w1) w2) w3-- Buchstaben einmal duch alle Walzen durch übersetzten


-- Umkehrwalzen

umkehrwalze1 :: Umkehrwalze
umkehrwalze1 =   zip   "EJMZALYXVBWFCRQUONTSPIKHGD" alphabet--eddmann1

umkehrwalze2 :: Umkehrwalze
umkehrwalze2 =   zip  "IMETCGFRAYSQBZXWLHKDVUPOJN" alphabet--

umkehrwalze3 :: Umkehrwalze
umkehrwalze3 = zip  "YRUHQSLDPXNGOKMIEBFZCWVJAT" alphabet

-- Umkehrwalzenfunktion
umkehren :: Char -> Umkehrwalze -> Char 
umkehren c umkehrwalze | snd(head umkehrwalze) == c = fst (head umkehrwalze)
                       | otherwise = umkehren c (tail umkehrwalze) 


verschluessle :: Char -> Walzenkombi -> Umkehrwalze -> Plugboard-> Char
verschluessle c (w1, w2, w3) u p = initialesVerschluesseln (zurueckuebersetzenMitKombi (umkehren (uebersetzenMitKombi (initialesVerschluesseln c p)(w1, w2, w3)) u) (w3, w2, w1)) p --hier war der BUG
--wo wird gedreht?-> danach

verschluessleString :: String -> Walzenkombi -> Umkehrwalze -> Plugboard -> String 
verschluessleString [] (w1, w2, w3) u p = []
verschluessleString (x:xs) (w1, w2, w3) u p = (verschluessle x (w1, w2, w3) u p) : (verschluessleString xs (dreheAlleWalzen(w1, w2, w3)) u p)

--Für Testzwecke
--main :: IO()
--main = do
--  print (verschluessleString "HASKELLSUCKSMAJORDICKIWANTTOKILLMYSELF" (walze1, walze2, walze3) umkehrwalze1)
--  print (verschluessleString "BGHQOSEFDTFUKUDQXYARHKEUEBZTGZQBUWVPCD" (walze1, walze2, walze3) umkehrwalze1)

-- Plugboard/Streckbrett

plugboard :: Plugboard
plugboard = []

--Plugboardfunktionen

toList :: Char -> Char -> Plugboard
toList c1 c2 = [(c1, c2)]

changePlugboard :: String -> Plugboard -> Plugboard--länge des Strings im interface checken und ob alle elemente nur einmal sind 
changePlugboard [] board = []
changePlugboard (x:y:xs) board = toList x y ++ toList y x ++ changePlugboard xs board -- doppeltes Abspeichern der Verbindung, sonst zwei Funktionen initialesVerschluesseln

initialesVerschluesseln :: Char -> Plugboard -> Char
initialesVerschluesseln c [] = c
initialesVerschluesseln c board | c == fst(head board) = snd (head board)
                                | otherwise = initialesVerschluesseln c (tail board)

main :: IO()
main = do
    print (plugboard)
    --print(changePlugboard "AXUN" plugboard)
    let plugboard1 =  changePlugboard [] plugboard
    --print(initialesVerschluesseln 'A' changedPlugboard)
    --print(initialesVerschluesseln 'X' changedPlugboard)
    print (verschluessleString "HASKELLSUCKSMAJORDICK" (walze1, walze2, walze3) umkehrwalze1 plugboard1)
    print (verschluessleString "BGHQOSEFDTFUKUDQXYARH" (walze1, walze2, walze3) umkehrwalze1 plugboard1)
    --print (verschluessleString "BGHQOSEFDTFUKUDQXYARH" (walze1, walze2, walze3) umkehrwalze1)

--xa 
--b b
--c c
--d d
