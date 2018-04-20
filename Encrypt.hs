import System.IO
import System.Random
import Data.Char

--main doet het volgende:
--Vraagt bestand om te versleutelen
--Genereert random generator
--Maakt sleutel met zelfde lengte als te versleutelen tekst
--Versleutelt tekst.
--Schrijft versleutelde tekst weg in apart bestand.
--Schrijft sleutel weg in apart bestand.

main = do
    msg <- getTxtFile
    gen <- getStdGen
    let key = fabricateKey gen msg
    let cryptotxt = encrypt msg key
    writeFile "Encrypted.txt" cryptotxt
    writeFile "Key.key" key

--getTxtFile doet het volgende:
--Schrijft op scherm "Geef de naam van het te versleutelen bestand".
--Doet de input in inp variabele en laat hier readFile op los.
--Stuurt inp terug.

getTxtFile = do
    putStrLn "Geef de naam van het te versleutelen bestand"
    inp <- getLine >>= readFile
    return inp

--fabricateKey doet het volgende:
--Genereert een lijst van 25 random Chars.
--Maakt een lijst van deze 25 Chars die even lang is als de tekst.
--Stuurt deze lijst terug.

fabricateKey gen msg = do
    let randomChars = take 25 (randomRs ('a','z') gen)
    key <- take (length msg) (cycle randomChars)
    return key

--intify doet het volgende:
--Maakt van een lijst van Chars een lijst van Ints om deze te gebruiken voor versleutelingen.

intify :: [Char] -> [Int]
intify = map ((\x -> if x < 50 then x + 128 else x) . ord)

--stringify doet het volgende:
--Maakt van een lijst Ints een lijst van Chars om deze weer weg te kunnen schrijven als letters.

stringify :: [Int] -> [Char]
stringify = concat . map show

--encrypt doet het volgende
--Versleutelt het bericht werkelijk door eerst msg en key te 'intifyen'.
--Deze bij elkaar op te tellen.
--Dit dan weer te 'stringifyen'
--En geeft dan het versleutelde bericht terug.

encrypt :: [Char] -> [Char] -> [Char]
encrypt msg key = stringify $ zipWith (+) (intify msg) (intify key)