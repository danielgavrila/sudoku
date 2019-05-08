import Data.Maybe
import Data.List
import Debug.Trace
import Data.List.Split
import System.IO
import Text.Read (readMaybe)
import qualified Data.Set as Set
-- Hide the standard versions so we can
-- reimplement them.
import Prelude hiding ((>>=), return)
import System.Environment
import System.TimeIt

-- constants defined
sqrtsize=3
size=sqrtsize*sqrtsize
indexes=[0..size*size-1]






data CellInput = CellInput {
    p :: Int,
    v :: Int
} deriving (Show, Eq)


toCellInput ::[String]->Maybe CellInput
toCellInput  ls = let mm=map  (readMaybe) ls
                      ci=catMaybes mm 
                  in
                  if 2==length ci 
                  then Just (CellInput  (ci!!0) (ci!!1))
                  else Nothing

toListCI::[String] -> [CellInput]                     
toListCI  ls = catMaybes (map (\x-> toCellInput ( words x))  ls)
     
toTable ::[CellInput]-> [Cell]
toTable c = let
              t0 = fill c
              t1 = padding indexes t0
              t2 = sortBy (\x y -> compare (idx x) (idx  y))   t1
            in
            cleanTable t2  


data Value = Filled Int | Candidates [Int] 
   deriving (Eq,Show)


-- helper structure to have the row and column of a index
data Pos=Pos{
row :: Int,
col :: Int
    }deriving (Eq,Show)

    
getPos::Int -> Pos
getPos i = if i< 0 || i >= size*size 
           then error (  show i ++  "   index not valid  " )
           else Pos (i `div` size) (i `mod` size)



data Cell= Cell{
idx :: Int,
val ::Value
} deriving (Eq,Show)

data Node = Node {
act ::[Cell],
contenders ::[[Cell]],
solutions ::[[Cell]]
} deriving (Eq,Show)

--used to sort the cells after the length of candidates list
data OrdCandidates= OrdCandidates{
indx :: Int,
lng ::Int -- length of candidates list
} deriving (Eq,Show)


buildOrdCandidates :: Cell -> OrdCandidates
buildOrdCandidates  c = case val c of
                        Filled _ ->     OrdCandidates (idx c) 0
                        Candidates x -> OrdCandidates (idx c) (length x)

orderedCandidates :: [Cell]-> [OrdCandidates]
orderedCandidates  t = let l= map (buildOrdCandidates) t
                           f= filter (\x -> lng x /= 0)  l
                       in
                       sortBy (\x y -> compare (lng x) (lng y) ) f

-- check to see if the length of candidates is at least 0 for a value
validCell::Bool->Cell->Bool
validCell b c =case (val c) of
              Filled _ -> b
              Candidates x -> if length x > 0 
                              then b
                              else False


-- check to see if the length of candidates is at least 0 for the table
validTable::[Cell]->Bool
validTable t = foldl  validCell  True t



hasDuplicatesWith :: Ord a => Set.Set a -> [a] -> Bool
hasDuplicatesWith seen [] =
    False -- base case: empty lists never contain duplicates
hasDuplicatesWith seen (x:xs) =
    -- If we have seen the current item before, we can short-circuit; otherwise,
    -- we'll add it the the set of previously seen items and process the rest of the
    -- list against that.
    x `Set.member` seen || hasDuplicatesWith (Set.insert x seen) xs


hasDuplicates :: Ord a =>  [a]  -> Bool
hasDuplicates = hasDuplicatesWith Set.empty



--return the value of cell
valOfCell :: Int ->[Cell]->Maybe Int
valOfCell i t = case val (t!!i) of
                    Filled x ->Just x
                    Candidates _ -> Nothing


-- return all the values found in one list of indexes (usually one region : row , column, block)
valRegion::[Int]->[Cell]->[Int]
valRegion is t = catMaybes (map (\i-> valOfCell i t ) is )


-- return all indexes of row r
indexesRow::Int->[Int]
indexesRow r = [size*r+x| x<-[0..size - 1 ]]

-- return all indexes of col  c
indexesCol::Int->[Int]
indexesCol c = [x*size+c| x<-[0..size -1 ]]


-- return all indexes of block b which is the index of top left corner's block
indexesBlock::Int->[Int]
indexesBlock b = [b+size*x+y| x<-[0.. sqrtsize-1], y<-[0.. sqrtsize-1]]


-- list of all region indexes  
allRegions::[[Int]]
allRegions  = let 
                   lsRows= map (\x -> indexesRow x )  [0..size -1] 
                   lsCols= map (\x -> indexesCol x )  [0..size -1] 
                   -- all indexes for top left corner for a block  =[sqrtsize*size*x+ sqrtsize*y| x<-[0.. sqrtsize-1], y<-[0.. sqrtsize-1]]
                   block =[sqrtsize*size*x+ sqrtsize*y| x<-[0.. sqrtsize-1], y<-[0.. sqrtsize-1]]
                   lsBlocks=map (\x -> indexesBlock x ) block 
               in
               lsRows ++ lsCols ++ lsBlocks


--return all values found in the row r
valRow::Int->[Cell]->[Int]
valRow r t = valRegion (indexesRow r ) t


--return all values found in the col c
valCol::Int->[Cell]->[Int]
valCol c t = valRegion ( indexesCol  c) t


--return all values found in the block b  which is the index of top left corner's block
valBlock::Int->[Cell]->[Int]
valBlock b t = valRegion ( indexesBlock b  ) t


--return a list of values (list of ints) in a reagion 
valsInRegions::( Int->[Cell]->[Int] )-> [Int]-> [Cell]-> [[Int]]
valsInRegions f xs table = map (\x -> f x table) xs

--check if the region has duplicates used in foldl
checkRegion ::Bool->[Int]->Bool
checkRegion b xs = b && not (hasDuplicates xs)

checkTable::[Cell]->Bool
checkTable t = let lsRows= valsInRegions valRow [0..size -1] t
                   br=foldl checkRegion (True)  lsRows
                   lsCols= valsInRegions valCol [0..size -1] t
                   bc=foldl checkRegion (True)  lsRows
                   block =[sqrtsize*size*x+ sqrtsize*y| x<-[0.. sqrtsize-1], y<-[0.. sqrtsize-1]]
                   lsBlocks= valsInRegions valBlock block t
                   bb =foldl checkRegion (True)  lsBlocks 
                in
                br && bc && bb




--check to see if the cell is filled  
cellFilled :: Bool->Cell->Bool
cellFilled  b c= case  val c of
                  Filled _ ->b
                  Candidates _ -> False


--check to see if the table is filled  
tableFilled::[Cell]->Bool
tableFilled t = foldl ( cellFilled  ) True  t

-- check to see if  we've found the solution
isSolved :: [Cell] -> Bool
isSolved  t = tableFilled t && checkTable t

-- the first argument is the pivot , could be filled or not
-- the second argument is one cell in neighbourhood (rows, cols or block)
-- return the modified list of candidates  for the second cell ,
-- if the first cell is filled and the second cell  is not filled 
--
cleanCandidate::Cell->Cell->Cell
cleanCandidate pivot c = let v = val c
                             a = val pivot
                         in
                         case a of
                         Filled p ->  
                                 case v of   -- pattern matching for Value which is a sum type
                                 Filled _ -> c
                                 Candidates x -> Cell (idx c) (Candidates (filter (/=p) x ))
                         Candidates _ -> c     

data Neighbours= Neighbours{
       pivot :: Cell,
       rows :: [Cell] ,
       cols ::  [Cell],
       block :: [Cell]
}deriving (Eq,Show)


cleanNeighbours:: Neighbours -> Neighbours
cleanNeighbours n = let 
                          p = pivot n
                          r = map (cleanCandidate p)  (rows n)
                          c = map (cleanCandidate p)   (cols n)
                          b = map (cleanCandidate p)   (block n)
                      in 
                      Neighbours (pivot n ) r c b 


--return a list of cells given the list of indexes
getCells :: [Int] -> [Cell]-> [Cell]
getCells  xs t = map (\x-> t!!x) xs



getRows::Int->[Int]
getRows idx = let line= indexesRow (row (getPos idx)) in
            filter (/=idx) line 

getCols::Int->[Int]
getCols idx = let  line =indexesCol (col (getPos idx)) in
            filter (/=idx) line 

getBlock::Int->[Int]
getBlock idx = let p  =  getPos idx
                   a1 = (row p) `div` sqrtsize
                   a2 = (col p) `div` sqrtsize
                   ul =a1*sqrtsize*size+ a2*sqrtsize --upper left corner
                   line =indexesBlock ul 
               in
               filter (/=idx) line 

fillCell:: CellInput->Cell
fillCell x = Cell (p x) (Filled (v x))   

fillCandidate ::Int -> Cell
fillCandidate i = Cell i (Candidates [1..size])

fill:: [CellInput]->[Cell]
fill xs = map  fillCell xs 


-- used in  padding
isFilled:: Int->[Cell]->Bool
isFilled i t =  elem i (map  (\x ->  idx x) t) 




paddingCell :: Int ->[Cell] -> Maybe Cell
paddingCell  i t = if isFilled i t 
                      then Nothing
                      else Just (fillCandidate i )

padding::[Int]->[Cell]->[Cell]
padding xs t = t ++ catMaybes (map (\i -> paddingCell  i t ) xs )



--structure that contains for a value candidate all the indexes that might  be filled. 
--If can be filled just in one position in region it will be selected in backtracking routine immediately
data CandidatIndexes =  CandidatIndexes
                       {
                       candidate ::Int,
                       idxs ::[Int] --list of indexes where the candidate might be filled
                       } deriving (Show,Eq)


--given a region (row, column or block) find all candiadtes for the not yet filled cell
candidatesRegion::[Int]->[Cell]->[Int]
candidatesRegion r t  = let
                      v = Set.fromList (valRegion r t )
                      c = Set.fromList [1..size]
                   in
               Set.toList (Set.difference c v)


--given a value and a cell return the index of cell if the cell is not filled and the value is a candidate 
addIndexCandidat::Int->[Cell]->Int->Maybe Int
addIndexCandidat r t v = let   c = t!!r in
                         case val (t!!r)  of 
                           Filled _ ->  Nothing
                           Candidates l -> if True== elem v l 
                                           then Just (idx c)
                                           else   Nothing 

--given a value and a region return the indexes of region where the value is a candidate
indexCandidatRegion::[Int]->[Cell]->Int->[Int]
indexCandidatRegion rs t v = catMaybes ( map (\x -> addIndexCandidat x t v ) rs  )

--given a region find the indexes for all possible candidates
indexesCandidates :: [Int]->[Cell]->[CandidatIndexes]
indexesCandidates  r t  = let cd = candidatesRegion r t  in
                          map (\x -> CandidatIndexes x (indexCandidatRegion r t  x)) cd 


--given a list of regions find the indexes for all possible candidates
allCandidatIndexes:: [[Int]]->[Cell]->[CandidatIndexes]
allCandidatIndexes xs t= let ll =foldl (\x y -> x ++  indexesCandidates y t  ) [] xs  in
                       sortBy (\x y -> (length.idxs $x) `compare` (length.idxs $y )) ll


--check to see if the first element in CandidatIndexes has a unique index
uniqCandidate :: [CandidatIndexes] -> Maybe CandidatIndexes
uniqCandidate  x = if length x ==0
                   then Nothing
                   else if length(idxs (head x))   ==1 
                           then Just (head x)
                           else Nothing

-- replace cell in table
rplCell::Cell->[Cell]->[Cell]
rplCell new t = let (x,_:xs) = splitAt (idx new) t in
                x++(new:xs)

--replace table with a list of cell (first argument)
rplTable::[Cell]->[Cell]->[Cell]
rplTable news table = foldr rplCell table  news 

neighboursCells :: Neighbours -> [Cell]
neighboursCells n = (rows n) ++ (cols n) ++ (block n)



getNeighbours::Cell->[Cell]->Neighbours
getNeighbours c t = let p1 = idx c in
                    Neighbours c (getCells (getRows p1) t ) (getCells (getCols p1) t) (getCells (getBlock p1) t)                   



cleanCell::Cell->[Cell]->[Cell]
cleanCell t tbl = let cs= neighboursCells (cleanNeighbours (getNeighbours t tbl)) in
                  rplTable cs tbl

cleanTable::[Cell]->[Cell]
cleanTable t = foldr cleanCell t t 

toContenders:: Cell->[Cell]
toContenders c = case val c of
                 Filled _ -> []
                 Candidates l -> map (\x -> Cell (idx c) (Filled x)) l 

backToNode:: Node->Maybe Node
backToNode  n = let ct = (contenders n) in
                if length ct ==0
                then Nothing
                else Just (Node ( head ct) (tail . contenders $n )   (solutions n) )
                   
--Haskell ad hoc polymorphism or functions overload from C++ 
class ToNextNode a where
      nextNode::Node->a->Node


instance ToNextNode CandidatIndexes where
       nextNode n dc  = 
                let
                   newcell= Cell (head (idxs dc)) (Filled (candidate dc))
                   t =cleanTable (rplCell newcell (act n) )
                in  
                Node t (contenders n)  (solutions n) 

instance ToNextNode Int  where
       nextNode n i  = let c= (act n) !! i in  
                case val c of
                   Filled _ -> n
                   Candidates l -> let ct= toContenders c
                                       a =cleanTable (rplCell (head ct) (act n) )
                                       t = map (\x -> cleanTable ( rplCell x (act n))) (tail ct ) 
                                   in
                                   Node a  (t ++  contenders n)  (solutions n)




addSolution::Node->Node
addSolution n = Node (act n)  (contenders n ) ((act n ):(solutions n )  )


backtracking :: Node-> Node 
backtracking n = let t = act n in 
                 if isSolved  t == True
                 then
                     let addSol= addSolution n
                         backNode = backToNode addSol
                     in
                     case backNode of 
                        Nothing -> addSol
                        Just x -> backtracking x 
                 else let oc = orderedCandidates t
                      in
                      if  length oc ==0 
                      then
                          let  backNode = backToNode n in
                          case backNode of 
                              Nothing -> n
                              Just x -> backtracking x 
                      else 
                           let 
                                i = indx (head oc)     
                                uq=uniqCandidate (allCandidatIndexes  allRegions t)
                           in
                           case uq of 
                              Nothing ->   backtracking (nextNode n i) 
                              Just idUniq -> backtracking (nextNode n idUniq) 
                              



main :: IO ()
main = do 

       a <- getArgs
       print a
       --contents <- readFile "easy01.txt"
       contents <- readFile (head a) -- "expert01.txt"
       let ll = lines contents
       let tbl= toTable (toListCI ll)
       --let sol=backtracking (Node  tbl  [])  
       --print sol
       timeIt $ putStrLn ("Result "  ++ show (solutions (backtracking (Node  tbl  [] []) )))
       print "END"

