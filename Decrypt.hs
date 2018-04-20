import System.IO
import Data.Char

--main doet het volgende:
--Vraagt om het uit te ontsleutelen bestand.
--Vraagt om de sleutel voor dit bestand.
--Ontsleutelt de versleutelde tekst.
--Schrijft het ontsleutelde tekst weg in een apart bestand.

main = do
    msg <- getTxtFile
    key <- getKeyFile
    let decryptoText = decode msg key
    writeFile "Decrypted.txt" decryptoText

--getTxtFile doet het volgende:
--Schrijft op scherm "Geef de naam van het te ontsleutelen bestand".
--Doet de input in inp variabele en laat hier readFile op los.
--Stuurt inp terug.

getTxtFile = do
    putStrLn "Geef de naam van het te ontsleutelen bestand"
    inp <- getLine >>= readFile
    return inp

--getKeyFile doet het volgende:
--Schrijft op scherm "Geef de naam van het sleutel bestand".
--Doet de input in inp variabele en laat hier readFile op los.
--Stuurt inp terug.

getKeyFile = do
    putStrLn "Geef de naam van het sleutel bestand"
    inp <- getLine >>= readFile
    return inp

--intify doet het volgende:
--Maakt van een lijst van Chars een lijst van Ints om deze te gebruiken voor ontsleuteling.

intify :: [Char] -> [Int]
intify = map ((\x -> if x < 50 then x + 128 else x) . ord)

--unStringify doet het volgende:
--Maakt van een lijst met Chars een lijst met Ints om deze te gebruiken voor ontsleuteling van het bestand.

unStringify :: [Char] -> [Int]
unStringify [] = []
unStringify (x:y:z:xs) = (read [x, y, z]):(unStringify xs)

--decode doet het volgende:
--Ontsleutelt het bericht werkelijk door msg te 'unStringifyen' en key te 'intifyen'
--Deze van elkaar af te trekken.
--Dit weer omzetten naar Chars.
--Dit in een lijst te stoppen.
--Dit in een apart bestand weg te schrijven.

decode :: [Char] -> [Char] -> [Char]
decode msg key = map (chr . \x -> if x > 127 then x - 128 else x) $ zipWith (-) (unStringify msg) (intify key)