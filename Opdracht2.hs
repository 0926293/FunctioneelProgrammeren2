import Data.List


--Type synoniemen voor datatype Boek.

type Titel = String
type Auteur = String
type Prijs = Int


--Een boek met een titel, een auteur en een prijs.

data Boek = Boek Titel Auteur Prijs deriving (Show, Eq)


--Accessor functies voor datatype Boek.

titel (Boek t _ _) = t
auteur (Boek _ a _) = a
prijs (Boek _ _ p) = p


--Instance declaratie voor datatype Boek om alleen op titel te sorteren.

instance Ord Boek where
    compare a b = compare (titel a) (titel b)


--Datatypes Box en Zak.

data Box a = Leeg | Box a deriving (Show)
data Zak a = LeegZak | Zak a deriving (Show)


--Instance declaraties voor datatypes Box en Zak.

instance Functor Box where
    fmap f Leeg = Leeg
    fmap f (Box a) = Box (f a)

instance Functor Zak where
    fmap f LeegZak = LeegZak
    fmap f (Zak a) = Zak (f a)


--Accessor functies voor datatypes Box en Zak.

boekUitBox (Box a) = a
boekUitZak (Zak a) = a
objectUitBox (Box a) = a


--Functies om boeken in een Box en uit een Box te halen.

boekenInBox :: [Boek] -> Box [Boek]
boekenInBox a = Box a
boekenUitBox :: Box [Boek] -> [Boek]
boekenUitBox a = boekUitBox a


--Oneliners om een object in een zak te stoppen en een Object in een box te stoppen.

objectInZak a = Zak (a)
objectInBox a = Box (a)


--Maakt van een lijst met boeken een lijst van boeken in zakken in boxen.

boekenlijstInZakInBox [] = []
boekenlijstInZakInBox (a:b) = objectInBox (objectInZak a) : boekenlijstInZakInBox b


--Maakt van een lijst met getallen een lijst van getallen in zakken in boxen.

getallenlijstInBox [] = []
getallenlijstInBox (a:b) = objectInBox a : getallenlijstInBox b


--Datatype List

data List a = Empty | Head a (List a) deriving (Show, Read, Eq)

pushList :: List a -> [a] -> List a
pushList Empty list = foldr Head Empty list
pushList (Head h rest) list = foldr Head (Head h rest) list

changeContainer :: Box a -> Zak a
changeContainer (Box a) = Zak a

instance Functor List where
    fmap f Empty = Empty
    fmap f (Head x n) = Head (f x) (fmap f n)


--Datatype Tree

data Tree a = EmptyTree | Node a (Tree a) (Tree a) deriving (Show, Read, Eq)

singleton :: a -> Tree a
singleton x = Node x EmptyTree EmptyTree

treeInsert :: (Ord a) => a -> Tree a -> Tree a
treeInsert x EmptyTree = singleton x
treeInsert x (Node a left right)
   | x == a = Node x left right
   | x < a  = Node a (treeInsert x left) right
   | x > a  = Node a left (treeInsert x right)

instance Functor Tree where
    fmap f EmptyTree = EmptyTree
    fmap f (Node x l r) = Node (f x) (fmap f l) (fmap f r)