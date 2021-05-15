-- Module for the mechanical parts of the enigma

alphabet :: [Char]
alphabet = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"

--
data Walze = Walze {rand_alphabet:: String, klar_alphabet :: String, umspringbuchstabe ::Char} -- umspringbuchstabe ggf als Übergasbeparameter
    deriving(Show)--testen ob die Walze saich drehen lässt mit mittels show/ terminalausgabe

type Walzenkombi = (Walze, Walze, Walze)--hier eine Kombination für das Drehen aller Walzen


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

uebersetzten :: Char -> Walze -> Char
uebersetzten c walze =  sucheInWalze (zip random klar) c-- walze rand_alphabet und klar_alphabet zippen 
                where random = rand_alphabet walze      -- dann in Hilfsfunktion nach der Stelle suchen, wo Eingabe = KlarBuchstabe
                      klar = klar_alphabet walze        -- dann im durch das Tupel das "zufällige" Gegenstück zurrückgeben.

uebersetztenMitKombi :: Char -> Walzenkombi -> Char
uebersetztenMitKombi c (w1, w2, w3) = uebersetzten (uebersetzten (uebersetzten c w1) w2) w3-- Buchstaben einmal duch alle Walzen durch übersetzten

-- TO-DO Lennart: 
-- Alles sollte wie gewünscht funktionieren. 
-- ggf. Überlegegungen zum Debugging von Teilfunktionen.
-- Nachlesen wie der Reflector funktioniert.
-- "Spiegelsimulation/funktion" implementieren
-- Danach kann mit dem Zusammensetzen der Verschlüsselung begonnen werden.


-- Umkehrwalze

main :: IO()
main = do
   print(walze1)
   -- print("--------------")
   -- print (dreheWalze (walze1))
   --print(dreheAlleWalzen(walze1, walze2, walze3))
 

    --print(preEncodeString[('A','E'),('B','Z')])
