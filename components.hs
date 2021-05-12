-- Module for the mechanical parts of the enigma

alphabet :: [Char]
alphabet = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"

--
data Walze = Walze {rand_alphabet:: String, klar_alphabet :: String, umspringbuchstabe ::Char} -- umspringbuchstabe ggf als Übergasbeparameter

-- Walze
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
dreheWalze (Walze rand_alphabet klar_alphabet umspringbuchstabe) = Walze rand_alphabet_neu klar_alphabet_neu umspringbuchstabe 
        where rand_alphabet_neu = tail rand_alphabet ++ [head rand_alphabet]
              klar_alphabet_neu = tail klar_alphabet ++ [head klar_alphabet]-- beide Alphabete müssen verschoben werden: bspw. W1 A=E -> B=K


checkObDrehen :: Walze -> Bool--sicher?
checkObDrehen walze = umspringbuchstabe walze == head (rand_alphabet walze)--eher klar_alphabet

-- Umkehrwalze


