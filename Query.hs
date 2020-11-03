module Query where

import Data.List.Split
import Data.List
import Data.Maybe

import UserInfo
import Rating
import Movie

type Column = String
type TableSchema = [Column]
type Field = String
type Entry = [Field]

data Table = Table TableSchema [Entry]

type ColSeparator = Char
type LnSeparator = Char


--tableschema string
firstline :: String -> LnSeparator -> String
firstline l sepl = head (splitOn [sepl] l)

--tableschema lista de cuvinte
tableschema :: String -> LnSeparator -> ColSeparator -> [String]
tableschema l sepl sepc = splitOn [sepc] (firstline l sepl)

--am eliminat ultima lista care era goala din cauza split-ului
eliminatelast :: String -> LnSeparator -> [String]
eliminatelast l sepl = reverse (tail (reverse (splitOn [sepl] l)))

--am aplicat split dupa separatorul de coloane pe tot tabelul
split' :: String -> ColSeparator -> LnSeparator -> [[String]]
split' l sepc sepl  = map (splitOn [sepc]) (eliminatelast l sepl)


--lista de entry-uri
entrylist :: String -> ColSeparator -> LnSeparator -> [[String]]
entrylist l sepc sepl = tail (split' l sepc sepl) 

-- TODO 1------------------------------------------------------------------------
read_table :: ColSeparator -> LnSeparator -> String -> Table
read_table sepc sepl l  = Table (tableschema l sepl sepc) (entrylist l sepc sepl)
---------------------------------------------------------------------------------



user_info = read_table '|' '\n' user_info_str
rating = read_table ' ' '\n' rating_str
movie = read_table '|' '\n' movie_str


--functie care creaza transpusa unei liste de liste de string-uri
transpose':: [[String]]->[[String]]
transpose' ([]:_) = []
transpose' x = (map head x) : transpose' (map tail x)


--functie care calculeaza lungimea celui mai lung cuvant dintr-o lista de cuvinte
maxlength :: [String] -> Int
maxlength l = foldl (\acc x -> if (length x) > acc then (length x) else acc) 0 l


--functie care creaza o lista cu lungimile maxime de pe coloane folosind transpusa matricii
maxlengthmatrixcolumns :: [[String]] -> [Int]
maxlengthmatrixcolumns l = foldr(\x acc -> (maxlength x):acc) [] (transpose' l)


--functie care adauga n spatii intr-un cuvant (string)
addspacetoword :: String -> Int -> String
addspacetoword word 0 = word
addspacetoword word 1 = word ++ " "
addspacetoword word n = (addspacetoword word (n-1)) ++ " "


{-functie care adauga la fiecare cuvant dintr-o lista diferenta dintre lungimea maxima transmisa ca
parametru si lungimea cuvantului. Se foloseste acelasi numar maxim deoarece este vorba de matricea
transpusa unde coloana devine linie-}
addSpaceToLine :: [String] -> Int -> [String]
addSpaceToLine l val = foldr(\x acc -> (addspacetoword x (val - (length x))): acc) [] l


{-functie care adauga la fiecare cuvant diferenta dintre lungimea maxima din lista maxv de pe
aceeasi pozitie si lungimea cuvantului folosita cand nu este transpusa-}
addSpaceToLineFinal :: [String] -> [Int] -> [String]
addSpaceToLineFinal [] _ = []
addSpaceToLineFinal l maxv =
    (addspacetoword (head l) (head maxv - (length (head l)))) :
        (addSpaceToLineFinal (tail l) (tail maxv))  

{- functie care adauga spatii in fiecare cuvant de pe fiecare linie din matrice, in functie de
maximul de pe acea linie (lista transpusa)-}
addspacestomatrix :: [[String]] -> [Int] -> [[String]]
addspacestomatrix [] val = []
addspacestomatrix l val = 
    (addSpaceToLine (head l) (head val)) : (addspacestomatrix (tail l) (tail val))


--functie care creaza transpusa finala cu spatiile adaugate
finaltranspose'withspaces :: [[String]] -> [[String]]
finaltranspose'withspaces l = addspacestomatrix (transpose' l) (maxlengthmatrixcolumns l)


--functie care creaza lista finala, transpunand solutia creata pe baza transpusei
finalmatrixwithspaces :: [[String]] -> [[String]]
finalmatrixwithspaces l = transpose' (finaltranspose'withspaces l)


--functie care creaza un string cu "val" cratime
generatedashes :: Int -> String
generatedashes 0 = ""
generatedashes 1 = "-"
generatedashes val = "-" ++ (generatedashes (val - 1))


{-functie care calculeaza latimea unei linii in functie de numarul de caractere de pe o linie care
contine spatii la care se adauga numarul de cuvinte + 1 (numarul de separatori de coloane)-}
sizeofline :: [String] -> [[String]] -> Int
sizeofline l rest = (sum (map length (head (finalmatrixwithspaces (l:rest))))) + (length l) + 1


--genereaza un string cu spatii si separator de coloane pentru header
generateline:: [String] -> [[String]] -> String
generateline l rest = 
    "|" ++ foldr (\x acc -> x ++ "|" ++ acc) []
        (addSpaceToLineFinal l (maxlengthmatrixcolumns (l:rest))) ++ "\n"


--genereaza un string dintr-o lista care are deja spatii pentru entries
generateline2:: [String] -> String
generateline2 l =  "|" ++ foldr (\x acc -> x ++ "|" ++ acc) [] l ++ "\n"

--functie care genereaza un string format din linii deja formatate cum trebuie
generatebody3 :: [[String]] -> String
generatebody3 l = foldr (\x acc ->  (generateline2 x) ++ acc ) [] l

-- TODO 2------------------------------------------------------------------------------------------
instance Show Table where
    show (Table header entries) =
        (generatedashes (sizeofline header entries)) ++ "\n"
            ++ (generateline header entries) 
            ++ (generatedashes (sizeofline header entries)) ++ "\n"
            ++ (generatebody3  (tail (finalmatrixwithspaces (header:entries))))
            ++ (generatedashes (sizeofline header entries)) ++ "\n"
---------------------------------------------------------------------------------------------------



data FilterCondition = Lt Field Integer | Eq Field String | In Field [String] | Not FilterCondition

fromJustToNr :: Maybe a -> a
fromJustToNr (Just a) = a


-- TODO 3
getFilter :: FilterCondition -> TableSchema -> (Entry -> Bool)
getFilter (Lt field value)  tableschema =
    (\entry -> read (entry !! fromJustToNr(elemIndex field tableschema)) < value)
getFilter (Eq field str) tableschema =
    (\entry -> entry !! fromJustToNr(elemIndex field tableschema) == str)
getFilter (Not fcond) tableschema =
    (\entry -> not ( getFilter fcond tableschema entry))
getFilter (In field list) tableschema =
    (\entry -> (elem (entry !! fromJustToNr(elemIndex field tableschema)) list))

--functie care cauta entry-ul in functie de primul field din tableschema
findEntryByFirstField :: String -> [[String]] -> [String]
findEntryByFirstField col table = foldr (\x acc-> if (head x) == col then x else acc) [] table


--functie care selecteaza toate coloanele cu field-urile specificate intr-o lista
selectColumnsByFields ::  [String] -> [[String]] -> [[String]]
selectColumnsByFields [] table = []
selectColumnsByFields list table =
    if (elem (head list) (head table))
        then ((findEntryByFirstField (head list) (transpose' table)) :
            (selectColumnsByFields (tail list) table))
        else (selectColumnsByFields (tail list) table)

{-functie care selecteaza toate coloanele cu tableentry-urile din list, primele nr-1 elemente cu 
ajutorul functiei "take". Este exact aceeasi functie ca la cerinta anterioara, insa, este 
introdusa functia take-}
selectColumnsByFieldsWithLimit ::  [String] -> Integer -> [[String]] -> [[String]]
selectColumnsByFieldsWithLimit [] _ table = []
selectColumnsByFieldsWithLimit list nr table =
    if (elem (head list) (head table))
        then ((take (fromInteger nr) (findEntryByFirstField (head list) (transpose' table))) :
                (selectColumnsByFieldsWithLimit (tail list) (fromInteger nr) table))
        else (selectColumnsByFieldsWithLimit (tail list) (fromInteger nr) table)

--functie care converteste o lista de lista de string-uri la Table
createTable :: [[String]] -> Table
createTable table = Table (head table) (tail table)


--functie care creaza lista de lista de string-uri pe baza unui Table
createlist :: Table -> [[String]]
createlist (Table  header entries) = header:entries

-- TODO 4
data Query = Filter FilterCondition Query |  
             Select [String] Query |
             SelectLimit [String] Integer Query |
             Cosine Query |
             Query :|| Query |
             Atom Table


{-
Pentru fiecare tip de filtru, apelez getFilter pe fiecare entry din tabela
Cu ajutorul foldr, entry-urile conforme sunt adaugate in tabela finala
-}
applyFilter :: [[String]] -> FilterCondition -> [[String]]
applyFilter table (In field list) = foldr (\x acc ->
    if ( getFilter (In field list) (head table) x) == True then x:acc else acc ) [] (tail table)
applyFilter table (Lt field value) = foldr (\x acc ->
    if (getFilter (Lt field value) (head table) x) == True then x:acc else acc) [] (tail table)
applyFilter table (Eq field str) = foldr (\x acc ->
    if (getFilter (Eq field str) (head table) x) == True then x:acc else acc) [] (tail table)
applyFilter table (Not fcond) = foldr (\x acc ->
    if (getFilter (Not fcond) (head table) x) == True then x:acc else acc) [] (tail table)
 

eval :: Query -> Table
eval (Atom table) = table
eval (Select list query) =
    createTable (transpose' ( selectColumnsByFields list (createlist (eval query))) )
eval (SelectLimit list nr query) =
    createTable (transpose' ( selectColumnsByFieldsWithLimit list (nr+1) (createlist (eval query))))
eval (Filter filt query) =
    Table (head (createlist (eval query))) (applyFilter (createlist (eval query)) filt)
eval (q1 :|| q2) =
    Table (head (createlist (eval q1))) ((tail (createlist (eval q1))) ++
        (tail (createlist (eval q2))))

--functie care returneaza pozitia din entry-list a unui entry in functie de id 
returnPosById :: String -> [[String]] -> Int
returnPosById id table = fromJustToNr( elemIndex id (head (transpose table)))

--functie care returneaza zona unui entry in functie de pozitia in entrylist
returnZoneByPos :: Int -> [[String]] -> String
returnZoneByPos pos table = head (reverse (transpose table)) !! pos

--functie care sterge din entry
deleteEntry :: String -> [[String]] -> [[String]]
deleteEntry id table = foldr (\x acc -> if (head x) == id  then acc else x:acc) [] table          

-- TODO 5
same_zone :: String -> Query
same_zone id = Select ["user_id","occupation"]
        (Atom (eval (Filter (Eq "zone" (returnZoneByPos (returnPosById id
        (split' UserInfo.user_info_str '|'  '\n')) (split' UserInfo.user_info_str '|'  '\n')))
        (Atom (Table (tableschema UserInfo.user_info_str '\n' '|')
        (deleteEntry id (entrylist UserInfo.user_info_str    '|' '\n')))))))

--functie care returneaza varsta dintr-un entry
getAgeFromEntry :: [String] -> Integer -> Bool
getAgeFromEntry entry value = if (read (head (tail entry))::Integer) > value then True else False

--functie care sterge un entry cu varsta mai mica decat o valoare
removeIfLowerAge :: Integer -> [[String]] -> [[String]]
removeIfLowerAge value entrylist = foldr (\x acc ->
    if (getAgeFromEntry x value) == True then x:acc else acc) [] entrylist


male_within_age :: Integer -> Integer -> Query
male_within_age n1 n2  =
    Select ["occupation","zone"] (Filter (Eq "sex" "M") (Atom (eval (Filter (Lt "age" n2)
    (Atom (Table (tableschema UserInfo.user_info_str '\n' '|')
    (removeIfLowerAge n1 (entrylist  UserInfo.user_info_str '|' '\n'))))))))

--query auxiliar caruia i s-a aplicat filtr-ul pentru varsta
firstQuery value =
    Atom (eval ((Atom (eval (Filter (Lt "age" (toInteger value))
    (Atom (Table (tableschema UserInfo.user_info_str '\n' '|')
    (entrylist UserInfo.user_info_str '|' '\n'))))))))

--query auxiliar caruia, pe langa filtr-ul de varsta, i s-a aplicat filtrul In pentru zone
secondQuery zones value = (Atom (eval (Filter (In "zone" zones) (firstQuery value) )))

mixed :: [String] -> [String] -> Int -> Query
mixed zones occupations value = 
    Select ["user_id"]
    (Atom (eval (Filter (In "occupation" occupations)(secondQuery zones value))))


