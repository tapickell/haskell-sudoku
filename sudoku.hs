import Data.Matrix
import Data.List

data Coor = Coor {row :: Int, col ::  Int} deriving (Show)
data Cell = FilledCell { location :: Coor
                       , value :: Int
                       , tried :: [Int]
                       , avail :: [Int]
                       } |
            EmptyCell { location :: Coor } deriving (Show)



-- still needs to be defined what completed means
-- no emptyCells in board ??
completed :: Matrix -> Bool
completed board = true


rowNumbers :: Coor -> Matrix -> [a]
rowNumbers Coor {row=r, col=c} = getRow r

columnNumbers :: Coor -> Matrix -> [a]
columnNumbers Coor {row=r, col=c} = getCol c

sector :: Coor -> Matrix -> [a]
sector Coor {row=r, col=c} board = True
-- determine submatrix for coor
-- return submatrix

-- wish this was cleaner | smaller
-- if row [1..3] then submatrix( 1 3 _ _)
-- if col [1..3] then submatrix( _ _ 1 3)
-- abc = [1,3]; def = [4,6]; ghi = [7,9]
-- rowMap = [abc, abc, abc, def, def, def, ghi, ghi, ghi]
-- adg = abc; beh = def; cfi = ghi
-- colMap = [adg, adg, adg, beh, beh, beh, cfi, cfi, cfi]
-- [1, 3] = rowSub 2
-- [1, 3] = colSub 3
getSec :: Int -> Int -> Matrix -> Matrix
getSec row col board
  | sectorA = toList(submatrix 1 3 1 3)
  | sectorB = toList(submatrix 1 3 4 6)
  | sectorC = toList(submatrix 1 3 7 9)
  | sectorD = toList(submatrix 4 6 1 3)
  | sectorE = toList(submatrix 4 6 4 6)
  | sectorF = toList(submatrix 4 6 7 9)
  | sectorG = toList(submatrix 7 9 1 3)
  | sectorH = toList(submatrix 7 9 4 6)
  | sectorI = toList(submatrix 7 9 7 9)
  where sectorA = elem row [1..3] && elem col [1..3]
        sectorB = elem row [1..3] && elem col [4..6]

      -- [1..3] [4..6] [7..9]

cellAt :: Coor -> Matrix -> Cell
cellAt Coor {row=r, col=c} = getElem r c

nextCoor :: Coor -> Coor
nextCoor Coor {row=r, col=c}
  | notAtEnd = Coor r c+1
  | endOfCol = Coor r+1 1
  | otherwise = Coor r c
  where notAtEnd = r <= 9 && c < 9
        endOfCol = r < 9 && c >= 9

prevCoor :: Coor -> Coor
prevCoor Coor {row=r, col=c}
  | notAtBeg = Coor r c-1
  | begOfCol = Coor r-1 9
  | otherwise = Coor r c
  where notAtBeg = r <= 9 && c > 1
        begOfCol = r > 1 && c <= 1

numbersAvail :: Coor -> Matrix -> [a]
numbersAvail Coor {row=r, col=c} board =
  let rows = getRow r board
      cols = getCol c board
      secs = getSec r c board
      avls = [1..9]
      unqs = nub(rows ++ cols ++ secs)
   in avls \\ unqs

-- this can work with random later
-- random index of as
valueFromAvail :: [Int] -> Int
valueFromAvail (a:as) = a

coorTuple :: Coor -> (Int, Int)
coorTuple Coor {row=r, col=c} = (r, c)

addCell :: Cell -> Matrix -> Matrix
addCell FilledCell {location=coor, value=v, tried=ts, avail=as} board
  | competed board = board
-- move to next while adding to board for recursion
  | canAddCell = addCell FilledCell { location=(nextCoor coor)
                                    , value= newValue
                                    , tried= newValue:tried
                                    , avail= (delete newValue avail)
                                    }
                          setElem (FilledCell {location=coor, value=v, tried=ts, avail=as})
                                  (coorTuple coor)
                                  board
-- no avail, backup to prev cell and try whats avail in that cell
  | otherwise = addCell FilledCell{ location=(prevCoor coor)
                                  , value = v
                                  , tried =  cell.tried + cell.value
                                  , avail = cell.avail - cell.value
                                  }
                        board
  where canAddCell = not (null as)
        newValue = valueFromAvail (as \\ ts)

-- empty matrix
-- zero 9 9
--  ( 0 0 0 0 0 0 0 0 0 )
--  ( 0 0 0 0 0 0 0 0 0 )
--  ( 0 0 0 0 0 0 0 0 0 )
--  ( 0 0 0 0 0 0 0 0 0 )
--  ( 0 0 0 0 0 0 0 0 0 )
--  ( 0 0 0 0 0 0 0 0 0 )
--  ( 0 0 0 0 0 0 0 0 0 )
--  ( 0 0 0 0 0 0 0 0 0 )
--  ( 0 0 0 0 0 0 0 0 0 )

-- add element to matrix
-- setElem 12 (3, 4) m3

--  ( 0 0 0 0 0 0 0 0 0 )
--  ( 0 0 0 0 0 0 0 0 0 )
--  ( 0 0 0 12 0 0 0 0 0 )
--  ( 0 0 0 0 0 0 0 0 0 )
--  ( 0 0 0 0 0 0 0 0 0 )
--  ( 0 0 0 0 0 0 0 0 0 )
--  ( 0 0 0 0 0 0 0 0 0 )
--  ( 0 0 0 0 0 0 0 0 0 )
--  ( 0 0 0 0 0 0 0 0 0 )

-- sectors
-- A, B, C
-- D, E, F
-- G, H, I
--
-- A submatrix 1 3 1 3
-- B submatrix 1 3 4 6
-- C submatrix 1 3 7 9
-- D submatrix 4 6 1 3
-- E submatrix 4 6 4 6
-- F submatrix 4 6 7 9
-- G submatrix 7 9 1 3
-- H submatrix 7 9 4 6
-- I submatrix 7 9 7 9
