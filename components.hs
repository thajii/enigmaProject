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
dreheWalze :: Walze -> Walze
dreheWalze Walze (rand_alphabet, klar_alphabet, umspringbuchstabe) = Walze tail rand_alphabet ++ [head rand_alphabet]  alphabet  umspringbuchstabe

checkObDrehen :: Walze -> Bool
checkObDrehen walze = umspringbuchstabe walze == head (rand_alphabet walze)

-- Umkehrwalze


