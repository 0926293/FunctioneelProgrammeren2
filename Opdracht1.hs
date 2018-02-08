import Data.List


--Datatype voor de geomerische figuren.

data Geofig = Vierkant Float Kleur | Rechthoek Float Float Kleur | Driehoek Float Kleur | Cirkel Float Kleur deriving (Show, Eq)


--Datatype om de kleur voor een geometrisch figuur te declaren.

data Kleur = Rood | Blauw | Geel deriving (Eq, Show, Enum)


--Datatype waar de oppervlakte/omtrek van een geofig makkelijk verkrijgbaar is.

data GeofigOppervlakteOmtrek = GeofigOppervlakteOmtrek {opp :: Float, omt :: Float, geofig :: Geofig} deriving (Show)


--Hulpfunctie om vast te stellen of de meegeleverde vorm van het gewenste type is.

isVierkant (Vierkant _ _) = True
isVierkant (Rechthoek _ _ _) = False
isVierkant (Driehoek _ _) = False
isVierkant (Cirkel _ _) = False

isRechthoek (Vierkant _ _) = False
isRechthoek (Rechthoek _ _ _) = True
isRechthoek (Driehoek _ _) = False
isRechthoek (Cirkel _ _) = False

isDriehoek (Vierkant _ _) = False
isDriehoek (Rechthoek _ _ _) = False
isDriehoek (Driehoek _ _) = True
isDriehoek (Cirkel _ _) = False

isCirkel (Vierkant _ _) = False
isCirkel (Rechthoek _ _ _) = False
isCirkel (Driehoek _ _) = False
isCirkel (Cirkel _ _) = True

kleur (Vierkant _ k) = k
kleur (Rechthoek _ _ k) = k
kleur (Driehoek _ k) = k
kleur (Cirkel _ k) = k


--Berekent oppervlakte.

oppervlakte :: Geofig -> Float
oppervlakte (Vierkant a _) = a * a
oppervlakte (Rechthoek a b _) = a * b
oppervlakte (Driehoek a _) = (1/2) * a * a
oppervlakte (Cirkel r _) = r * pi ^ 2


--Berekent omtrek.

omtrek :: Geofig -> Float
omtrek (Vierkant a _) = a * 4
omtrek (Rechthoek a b _) = (a * 2) + (b * 2)
omtrek (Driehoek a _) = a * 3
omtrek (Cirkel r _) = 2 * r * pi


--Laat een versie van de lijst zien waar alleen vierkanten, rechthoeken, driehoeken of cirkels in zitten.

alleenVierkanten [] = []
alleenVierkanten (a:b) = if isVierkant a == True then a:alleenVierkanten b else alleenVierkanten b

alleenRechthoeken [] = []
alleenRechthoeken (a:b) = if isRechthoek a == True then a:alleenRechthoeken b else alleenRechthoeken b

alleenDriehoeken [] = []
alleenDriehoeken (a:b) = if isDriehoek a == True then a:alleenDriehoeken b else alleenDriehoeken b

alleenCirkels [] = []
alleenCirkels (a:b) = if isCirkel a == True then a:alleenCirkels b else alleenCirkels b


--Laat een versie van de lijst zien waar alleen de objecten die hetzelfde zijn als de megeleverde string in zitten.

alleenString :: [Geofig] -> String -> [Geofig]
alleenString a b
    | b == "Vierkant" = alleenVierkanten a
    | b == "Rechthoek" = alleenRechthoeken a
    | b == "Driehoek" = alleenDriehoeken a
    | b == "Cirkel" = alleenCirkels a


--Laat een versie van de lijst zien waar alleen rode, blauwe of gele geofig's in zitten.

alleenRood [] = []
alleenRood (a:b) = if kleur a == Rood then a:alleenRood b else alleenRood b 

alleenBlauw [] = []
alleenBlauw (a:b) = if kleur a == Blauw then a:alleenBlauw b else alleenBlauw b

alleenGeel [] = []
alleenGeel (a:b) = if kleur a == Geel then a:alleenGeel b else alleenGeel b


--Laat een versie van de lijst zien waar alleen de objecten die dezelfde kleur hebben als de megeleverde kleur in zitten.

alleenKleur :: [Geofig] -> Kleur -> [Geofig]
alleenKleur a b
    | b == Rood = alleenRood a
    | b == Blauw = alleenBlauw a
    | b == Geel = alleenGeel a


--Constructor voor datatype GeofigOppervlakteOmtrek.

geofig2GeofigOppervlakteOmtrek :: Geofig -> GeofigOppervlakteOmtrek
geofig2GeofigOppervlakteOmtrek a = GeofigOppervlakteOmtrek{opp = (oppervlakte a), omt = (omtrek a), geofig = a}


--Laat het object met grootste oppervlakte/omtrek uit de lijst zien.

maxOppervlakte :: [GeofigOppervlakteOmtrek] -> GeofigOppervlakteOmtrek
maxOppervlakte a = last (sortOn (opp) a)

maxOmtrek :: [GeofigOppervlakteOmtrek] -> GeofigOppervlakteOmtrek
maxOmtrek a = last (sortOn (omt) a)


--Voegt een geofig toe aan een lijst van geofig's.

voegGeofigToe :: [Geofig] -> Geofig -> [Geofig]
voegGeofigToe a b = b : a


--Maakt van een lijst van geofigOppervlakteOmtrek een lijst waar alleen de oppervlaktes van de geofig2GeofigOppervlakteOmtrek in staan.

totaalOppervlakte [] = []
totaalOppervlakte (a:b) = opp a : totaalOppervlakte b


--Geeft de som van een lijst met GeofigOppervlakteOmtrek.

somOppervlakteTotaal :: [GeofigOppervlakteOmtrek] -> Float
somOppervlakteTotaal a = sum (totaalOppervlakte a)


--Maakt een GeofigOppervlakteOmtrek en een string met het percentage een string met de geofig en het percentage.

percentageTotaal :: GeofigOppervlakteOmtrek -> Float -> String
percentageTotaal (GeofigOppervlakteOmtrek {opp = a, omt = b, geofig = c}) d = "Geofig : " ++show c++ " --> Percentage : " ++show e
    where e = ((a) / (d)) * 100


--Maakt van een lijst van datatype GeofigOppervlakteOmtrek een lijst van string's met het Geofig en het percentage van de Geofig van de lijst als geheel.
--De lijst dient 2 keer megeleverd te worden, dit omdat ik het anders niet werkend kreeg.

oppervlaktePercentageTotaal :: [GeofigOppervlakteOmtrek ] -> [GeofigOppervlakteOmtrek] -> [String]
oppervlaktePercentageTotaal _ [] = []
oppervlaktePercentageTotaal d (a:b) = c : oppervlaktePercentageTotaal d b
    where c = percentageTotaal a (somOppervlakteTotaal d)