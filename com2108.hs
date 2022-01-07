{-# LANGUAGE FlexibleInstances #-}
-- assignment 
import System.Random
import Data.List (null, sortBy, zip, head, map, drop, take, (!!), filter, tail, elem, length, last, foldr, sum)
import GHC.Types (Bool(False))
import Prelude (Bool(True), Show (show), Eq ((==), (/=)), Enum (succ, pred), Ord (compare, (<=), (>), (>=)), Int, Ordering, String, (++)
                , concat, otherwise, (&&), not, (.), any, notElem, Num ((+), (-)), Integral (div), IO, putStrLn, print, ($), Monad (return), putStr, (||))
import Data.Maybe (Maybe(Nothing, Just), isJust)


----------------------------------------PART 1 -----------------------------------------------------
----------------------------------------DEFINING DATA TYPES ----------------------------------------
-- Suit
data Suit = Hearts | Clubs | Spades | Diamonds  deriving (Show, Enum,Eq,Ord)

-- Pip
data Pip = Ace | Two | Three | Four | Five | Six | Seven |
           Eight | Nine | Ten  | Jack  | Queen | King  deriving (Show, Enum,Eq,Ord)

-- A card with two types either faced up or faced down with show instances 
data Card = UCard Pip Suit | DCard Pip Suit  deriving (Eq)
instance Show Card where
    show (UCard pip suit) = "(" ++  show pip ++ "," ++ show suit ++ ")"
    show (DCard _ _) = "Unkown"

-- data types for 8-of and spider  
type Deck = [Card]
type Foundation = [Card]
type Column = [[Card]]

type Reserve = [Card]
type Stock = [Card]

-- board game constructor with show instances
data Board = EOBoard Foundation Column Reserve | SBoard Foundation Column Stock deriving (Eq)
instance Show Board where
    show (EOBoard fonds cols res) = "EOBoard " ++ "\n" ++ "Foundations " ++ show fonds ++
                            "\n" ++ "columns: " ++ colsToString cols ++ "\n" ++ "Reserve " ++ show res
    show (SBoard fonds cols stk) = "SBoard" ++ "\n" ++ "Foundations" ++ show fonds ++
                            "\n" ++ "columns: " ++ colsToString cols ++ "\n" ++ "Stock " ++ show (length stk) ++ " Deals remaining"
---------special healper method to turn a column to a string used in the instances above---------
colsToString :: Column -> String
colsToString columns = concat ["\n" ++show col | col<- columns]

----------------------------------------BASIC FUNCTIONS----------------------------------------
-- creates 52 cards deck
pack :: Deck
pack = [UCard pip suit | pip <- [Ace .. King], suit <- [Hearts .. Diamonds]] -- initially all cards are faced up

-- returns successor of a card
sCard :: Card -> Card
sCard (UCard pip suit) | pip == King  = UCard Ace suit
                       | otherwise    = UCard (succ pip) suit
sCard (DCard pip suit) | pip == King  = DCard Ace suit
                       | otherwise    = DCard (succ pip) suit

-- returns predecessor of a card
pCard :: Card -> Card
pCard (UCard pip suit) | pip == Ace  = UCard King suit
                       | otherwise   = UCard (pred pip) suit
pCard (DCard pip suit) | pip == Ace  = DCard King suit
                       | otherwise   = DCard (pred pip) suit

-- checks if Ace
isAce :: Card -> Bool
isAce (UCard pip _) = pip == Ace
isAce (DCard pip _) = pip == Ace

-- checks of King
isKing :: Card -> Bool
isKing (UCard pip _) = pip == King
isKing (DCard pip _) = pip == King

--shuffle deck 
cmp :: (Card, Int) -> (Card, Int) -> Ordering --helper method to order randomly assigned integers to cards in shuffle
cmp (_,y1) (_,y2) = compare y1 y2
shuffle :: StdGen -> Deck -> Deck
shuffle gen deck = [x | (x,_)<- sortBy cmp (zip deck (randoms gen))]

--switch state of a card e.g faced up -> faced down 
switch :: Card -> Card
switch (DCard pip suit) = UCard pip suit
switch (UCard pip suit) = DCard pip suit

--helper method to sDeal columns witch set all cards faced down except for the head 
switchcol :: [Card] -> [Card]
switchcol col = map (\c -> if c /= head col then switch c else c) col

----------------------------------------8-OF/SPIDER DEALING FUNCTIONS----------------------------------------
--note : to use the dealing functions in terminal you must type (_Deal (mkStdgen (seed))) 
--because we need Stdgen to shuffle the deck and not an Int
eODeal :: StdGen -> Board
eODeal seed = EOBoard fonds cols res
    where
        --creates shuffled deck
        deck = shuffle seed pack
        --set foundations to empty
        fonds = []
        --fill 8 columns 6 cards each from the suffled card
        cols = [take 6 deck,take 6 (drop 6 deck),take 6 (drop 12 deck),take 6 (drop 18 deck),
            take 6 (drop 24 deck), take 6 (drop 30 deck),take 6 (drop 36 deck),take 6 (drop 42 deck)]
        --fill only 4 reserved spaces with last four cards  
        res = [deck!!48,deck!!49,deck!!50,deck!!51]
sDeal :: StdGen -> Board
sDeal seed = SBoard fonds cols stk
    where
        --creates two shuffled decks
        decks = shuffle seed pack ++ shuffle seed pack
        --set foundations to empty
        fonds = []
        --fills 10 columns 6 cards for the fisrt 4, 5 cards for the rest
        col = [take 6 decks, take 6 (drop 6 decks), take 6 (drop 12 decks), take 6 (drop 18 decks), take 5 (drop 24 decks),
            take 5 (drop 29 decks), take 5 (drop 34 decks), take 5 (drop 39 decks), take 5 (drop 44 decks), take 5 (drop 49 decks)]
        --make columns head only visible
        cols = map switchcol col
        --remaining 50 for stock (also faced down)
        stk = map switch (drop 54 decks)

----------------------------------------8-OF/SPIDER CONSTANTS----------------------------------------
eightof :: Board
eightof = EOBoard [][[UCard Ace Clubs,UCard Seven Diamonds,UCard Ace Hearts,UCard Queen Hearts,UCard King Spades,UCard Four Spades]
            ,[UCard Five Diamonds,UCard Queen Spades,UCard Three Diamonds,UCard Five Spades,UCard Six Spades,UCard Seven Hearts]
            ,[UCard King Hearts,UCard Ten Diamonds,UCard Seven Spades,UCard Queen Diamonds,UCard Five Hearts,UCard Eight Diamonds]
            ,[UCard Jack Spades,UCard Six Hearts,UCard Seven Clubs,UCard Eight Spades,UCard Ten Clubs,UCard Queen Clubs]
            ,[UCard Ace Spades,UCard Eight Clubs,UCard Ace Diamonds,UCard King Diamonds,UCard Jack Hearts,UCard Four Clubs]
            ,[UCard Two Diamonds,UCard Three Hearts,UCard Three Clubs,UCard Ten Hearts,UCard Six Diamonds,UCard Jack Spades]
            ,[UCard Nine Spades,UCard Four Diamonds,UCard Nine Clubs,UCard Nine Hearts,UCard Three Spades,UCard Ten Spades]
            ,[UCard Two Clubs,UCard Two Spades,UCard Four Hearts,UCard Nine Diamonds,UCard King Spades,UCard Eight Hearts]]
                    [UCard Two Hearts,UCard Six Clubs,UCard Five Clubs,UCard Jack Diamonds]

spider :: Board
spider = SBoard [UCard King Hearts] [[UCard Eight Diamonds, UCard Nine Hearts]
            ,[UCard Two Diamonds]
            ,[UCard Ace Spades,UCard Eight Clubs,UCard Nine Clubs,UCard Ten Diamonds,UCard Jack Diamonds,UCard Queen Diamonds,UCard King Diamonds
                ,switch (UCard Nine Hearts),switch (UCard Ace Clubs)]
            ,[UCard Seven Clubs,UCard Eight Diamonds,UCard Nine Diamonds,UCard Ten Diamonds,UCard Jack Diamonds,UCard Queen Diamonds
                ,UCard King Diamonds,UCard Nine Clubs,UCard Ten Hearts,UCard Jack Clubs]
            ,[UCard Ace Hearts,UCard Two Hearts,UCard Three Hearts,UCard Four Hearts,UCard Five Hearts,UCard Six Diamonds,UCard Seven Diamonds
                ,UCard Queen Clubs,UCard King Hearts]
            ,[UCard Two Diamonds,UCard Three Diamonds,UCard Four Diamonds]
            ,[UCard Jack Clubs,UCard Queen Clubs,UCard King Clubs,UCard Two Spades,UCard Three Spades,UCard Four Diamonds,UCard Five Diamonds
                ,UCard Six Diamonds,UCard Seven Hearts,UCard Eight Clubs,UCard Nine Spades,UCard Ten Clubs,UCard Ace Clubs,UCard Two Clubs
                ,UCard Three Clubs,UCard Four Clubs,UCard Five Spades]
            ,[UCard Seven Spades,UCard Eight Spades,UCard Nine Spades,UCard Ten Spades,UCard Jack Spades,UCard Queen Spades,UCard King Spades
                ,switch (UCard Two Diamonds),switch (UCard Nine Spades),switch (UCard Four Hearts)]
            ,[UCard Jack Hearts,UCard Queen Hearts]
            ,[UCard Ace Clubs,UCard Two Clubs]] [switch (UCard Ace Diamonds),switch (UCard Jack Spades)]

----------------------------------------8-OF SPECIAL FUNCTIONS----------------------------------------
{- toFoundation:
    1- find all possible aces in column and move to foundation
    2- find all possible aces in reserved and move to foundation
    3- find all possible moves from columns heads to foundation
    4- find all possible moves from reserved to foundation
    5- find all possible moves from column successors lists to foundation
    6- repeat -}
--the functions numerated for the steps above 
---STEP 1--find all possible aces in column and move to foundation--
colsAcesToFonds :: Board -> Board
colsAcesToFonds (EOBoard fonds cols res) | aceInCols cols/=[] = EOBoard (fonds ++ aceInCols cols)  --update foundation with aces from a column heads
                                              (map (\col -> if (not.null) col && isAce (head col)
                                              then tail col else col) cols) res --remove the ace from that column
                                         | otherwise = EOBoard fonds cols res
colsAcesToFonds (SBoard fonds cols stk) = SBoard fonds cols stk
--helper method returns a list of all the columns heads
colHeads :: [[Card]] -> [Card]
colHeads [[]] = []
colHeads cols = [head x | x<- cols, not(null x)]
--helper method returns a list of all aces found in columns heads
aceInCols :: [[Card]] -> [Card]
aceInCols [[]] = []
aceInCols cols = filter isAce (colHeads cols)

---STEP 2--find all possible aces in reserved and move to foundation--
resAcesToFonds :: Board -> Board
resAcesToFonds (EOBoard fonds cols res) = EOBoard (fonds ++ filter isAce res) cols (filter (not.isAce) res)
resAcesToFonds (SBoard fonds cols stk) = SBoard fonds cols stk

---STEP 3--find all possible moves from columns heads to foundation--
colsToFonds :: Board -> Board
colsToFonds (EOBoard fonds cols res) = EOBoard (map (\c -> if sCard c `elem` colHeads cols && not(isKing c) then sCard c else c) fonds) -- get card successor if it's a head in a column 
                 (map (\col -> if (not.null) col && pCard (head col) `elem` fonds && not(isKing (head col)) then tail col else col) cols) -- remove the head from that column 
                 res
colsToFonds (SBoard fonds cols stk) = SBoard fonds cols stk

---STEP 4--find all possible moves from reserved to foundation--
resToFonds :: Board -> Board
resToFonds (EOBoard fonds cols res) = EOBoard (map (\c -> (if sCard c `elem` res && not(isKing c)then sCard c else c)) fonds) -- get card successor if it's in reserved
                                         cols (filter (\c -> pCard c `notElem` fonds || isKing c) res) -- remove the card taken
resToFonds (SBoard fonds cols stk) = SBoard fonds cols stk
---STEP 5--find all possible moves from column successors lists to foundation--
colNthSuccToFonds :: Board -> Board
colNthSuccToFonds (EOBoard fonds cols res) | colNthSucc cols /=[]&&length res+length (colNthSucc cols)<=8&& not(null fonds) =
                                                EOBoard (map(\c-> if sCard c == head (colNthSucc cols) then last (colNthSucc cols)else c)fonds)
                                                (map(\col-> if (not.null) col && head col == head (colNthSucc cols) then drop (length (colNthSucc cols)) col else col )cols) res
                                        | otherwise = EOBoard fonds cols res
colNthSuccToFonds (SBoard fonds cols res) = SBoard fonds cols res
--STEP 6--REPEAT-------------------------TO FOUNDATIONS--------------------------------------------------
toFoundations :: Board -> Board
toFoundations eoboard@EOBoard {}
            | eoboard/=colsAcesToFonds eoboard = toFoundations (colsAcesToFonds eoboard)
            | eoboard/=resAcesToFonds eoboard = toFoundations (resAcesToFonds eoboard)
            | eoboard/=colsToFonds eoboard = toFoundations (colsToFonds eoboard)
            | eoboard/=resToFonds eoboard = toFoundations (resToFonds eoboard)
            | eoboard/=colNthSuccToFonds eoboard = toFoundations (colNthSuccToFonds eoboard)
            | otherwise = eoboard

toFoundations (SBoard fonds cols stk) = SBoard fonds cols stk

----------------------------------------PART 2 ------------------------------------------------------------------
----------------------------------------PART 2 ------------------------------------------------------------------
----------------------------------------PART 2 ------------------------------------------------------------------

----------------------------------------FIND ALL POSSIBLE MOVES FOR 8-OF ----------------------------------------
findMoves :: Board -> [Board]
findMoves eoboard@EOBoard {} = filter (/=EOBoard [][[]][]) --toFoundations will be called first each time on any resulting board
                                             [if toFoundations eoboard/=eoboard then toFoundations eoboard else EOBoard [][[]][]
                                            , if resKtoEcol eoboard/=eoboard then resKtoEcol eoboard else EOBoard [][[]][]
                                            , if colKtoEcol eoboard/=eoboard then colKtoEcol eoboard  else EOBoard [][[]][]
                                            , if moveResToCol eoboard/=eoboard then moveResToCol eoboard  else EOBoard [][[]][]
                                            , if moveColNthSucc eoboard/=eoboard then moveColNthSucc eoboard else EOBoard [][[]][]
                                            , if moveColToCol eoboard/=eoboard then moveColToCol eoboard else EOBoard [][[]][]
                                            , if movesFromColToRes eoboard/=eoboard then movesFromColToRes eoboard else EOBoard [][[]][]]
findMoves (SBoard fonds cols res) = [SBoard fonds cols res]

{--the possible directions of moves:
        1-from res or col to foundations
        2-from res to col
        3-from col to col
        4-from col to res --}

------------------1-------refer to (toFoundations) function----------------------------
------------------2-------from res to col-----------------------------------
resKtoEcol :: Board -> Board --moving a king from res to empty column (Ecol)
resKtoEcol (EOBoard fonds cols res) | not(isEcols cols)&& isEcol cols && resKings res /= [] = EOBoard fonds
                                            (map (addResKingToEcol (head (resKings res))) cols)
                                            (filter (/=head (resKings res)) res)
                                    |otherwise = EOBoard fonds cols res
resKtoEcol (SBoard fonds cols res) = SBoard fonds cols res
moveResToCol :: Board -> Board --moving successor card from res to column head 
moveResToCol (EOBoard fonds cols res) | resColSucc res cols = EOBoard fonds (map (\col -> if (not.null) col&& pCard (head col) `elem` res
                                        && not (isAce (head col))then pCard (head col):col else col) cols)
                                        (filter (\c -> sCard c `notElem` colHeads cols && not(isKing c)) res)
                                   | otherwise = EOBoard fonds cols res
moveResToCol (SBoard fonds cols res) = SBoard fonds cols res
------------------3-------from col to col------------------------------------
colKtoEcol :: Board -> Board --moving a king from col to Ecol
colKtoEcol (EOBoard fonds cols res) | not(isEcols cols)&& isEcol cols && isKingCol cols = EOBoard fonds (map (\col-> if col/=[]
                                        &&isKing (head col)&& head col == head (colKings cols) then tail col else if null col then col++filter isKing (colHeads cols) else col)cols) res
                                    | otherwise = EOBoard fonds cols res
colKtoEcol (SBoard fonds cols res) = SBoard fonds cols res
moveColToCol :: Board -> Board --moving successor card from column head to another column head
moveColToCol (EOBoard fonds cols res) | colColSucc cols = EOBoard fonds  (map(\col-> if (not.null) col&& pCard (head col) `elem` colHeads cols
                                         then pCard (head col):col else if (not.null) col && sCard (head col) `elem` colHeads cols
                                             then tail col else col)cols) res
                                   | otherwise = EOBoard fonds cols res
moveColToCol (SBoard fonds cols res) = SBoard fonds cols res
moveColNthSucc :: Board -> Board --moving list of successor cards of any length that is legal (== free spaces in reserve) to another column head
moveColNthSucc (EOBoard fonds cols res) | colNthSucc cols /=[]&&length res+length (colNthSucc cols)<=8  = EOBoard fonds
                                                (map(\col-> if (not.null) col &&pCard (head col) == last (colNthSucc cols)
                                                then colNthSucc cols++col else if (not.null) col && head col == head (colNthSucc cols)
                                                then drop (length (colNthSucc cols)) col else col )cols) res
                                        | colNthSucc cols /=[]&&length res+length (colNthSucc cols)>8  = EOBoard fonds
                                                (map(\col-> if (not.null) col &&pCard (head col) == last (colNthSucc cols)
                                                then take (8-length res)(colNthSucc cols)++col else if (not.null) col && head col == head (colNthSucc cols)
                                                then drop (length (take (8-length res)(colNthSucc cols))) col else col )cols) res
                                        | otherwise = EOBoard fonds cols res
moveColNthSucc (SBoard fonds cols res) = SBoard fonds cols res
------------------4-------from col to res-----------------------------------
movesFromColToRes :: Board -> Board --all moves form column to res in one function
movesFromColToRes eoboard@(EOBoard fonds cols res) | singleColToRes eoboard/=eoboard = toFoundations(singleColToRes eoboard)
                                                   | length res <=7 && moveAceSecondCol eoboard/=eoboard = toFoundations(moveAceSecondCol eoboard)
                                                   | length res <=7 && moveKingSecondCol eoboard/=eoboard = toFoundations(moveKingSecondCol eoboard)
                                                   | length res <=7 && moveFondPreSecondCol eoboard/=eoboard = toFoundations(moveFondPreSecondCol eoboard)
                                                   | length res <=7 && moveColPreSecondCol eoboard/=eoboard = toFoundations(moveColPreSecondCol eoboard)
                                                   | otherwise = eoboard
movesFromColToRes (SBoard fonds cols res) = SBoard fonds cols res
singleColToRes :: Board -> Board --moving a card in a column of length one to res
singleColToRes (EOBoard fonds cols res) | getSingleCol cols/=[]&& length res <=7 = EOBoard fonds
                                                (map (\col -> if  (not.null) col&& length col == 1 && (not.isKing)(head col)
                                                && head col == head (getSingleCol cols) then tail col else col) cols)
                                                (res++getSingleCol cols)
                                        | otherwise = EOBoard fonds cols res
singleColToRes (SBoard fonds cols res) = SBoard fonds cols res
moveAceSecondCol :: Board -> Board --moving a card that when put in res, it exposes an ace
moveAceSecondCol (EOBoard fonds cols res) | aceSecondCol cols/=[] = EOBoard fonds (map (\col-> if (not.null) col&& length col>=2&& isAce (head (tail col))&&
                                                [head col] == aceSecondCol cols  then tail col else col) cols) (res++aceSecondCol cols)
                                          | otherwise = EOBoard fonds cols res
moveAceSecondCol (SBoard fonds cols res) = SBoard fonds cols res
moveKingSecondCol :: Board -> Board --moving a card that when put in res, it exposes an king
moveKingSecondCol (EOBoard fonds cols res) | kingSecondCol cols/=[] = EOBoard fonds (map (\col-> if (not.null) col && length col>=3&& isKing (head(tail col))
                                                    && [head col] == kingSecondCol cols  then tail col else col)cols)
                                                    (res++kingSecondCol cols)
                                           | otherwise  = EOBoard fonds cols res
moveKingSecondCol (SBoard fonds cols res) = SBoard fonds cols res
moveFondPreSecondCol :: Board -> Board --moving a card that when put in res, it exposes successor card for fonds
moveFondPreSecondCol (EOBoard fonds cols res) | fondPreSecondCol cols fonds/=[] = EOBoard fonds (map(\col-> if (not.null) col && length col>=2
                                                    && pCard (head (tail col)) `elem` fonds && [head col] == fondPreSecondCol cols fonds
                                                    then tail col else col)cols)(res++fondPreSecondCol cols fonds)
                                              | otherwise = EOBoard fonds cols res
moveFondPreSecondCol (SBoard fonds cols res) = SBoard fonds cols res
moveColPreSecondCol :: Board -> Board --moving a card that when put in res, it exposes successor card for another column head
moveColPreSecondCol (EOBoard fonds cols res) | colPreSecondCol cols /= [] = EOBoard fonds (map(\col-> if(not.null) col &&length col>=2
                                                    && sCard (head (tail col)) `elem` colHeads cols && pCard (head (tail col)) /= head col
                                                    && [head col] == colPreSecondCol cols then tail col else col)cols) (res++colPreSecondCol cols)
                                             | otherwise = EOBoard fonds cols res
moveColPreSecondCol (SBoard fonds cols res) = SBoard fonds cols res

----------------------------------------PLAY GAME 8-OF ----------------------------------------
--choose move will take the first in the list of findMoves because origanilly findMoves have best moves in order
chooseMove :: Board -> Maybe Board
chooseMove eoboard@EOBoard {} | (not.null)(findMoves eoboard) = Just (head (findMoves eoboard))
                              | otherwise = Nothing
chooseMove sboard@(SBoard fonds cols res) | null (findMoves sboard) = Just (SBoard fonds cols res)
                                          | otherwise = Nothing
--to play the game and return a score
playSolitaire :: Board -> Int
playSolitaire eoboard@(EOBoard fonds cols res)
  | areEcols cols && null res = getScore eoboard
  | isJust (chooseMove eoboard) = playSolitaire (maybeToBoard (chooseMove eoboard))
  | otherwise = getScore eoboard
playSolitaire SBoard {} = 0
--to play the game n times using initial seed to shuffle 
playNtimes :: (Int,Int) -> [Int]
playNtimes (seed,numGames) = map (playSolitaire . eODeal . mkStdGen)[seed..seed+(numGames-1)]
--to analyse performance over n games
analyseEO :: (Int,Int) -> (Int,Int)
analyseEO (seed,numGames) = (numberOfWins, averageScore)
                            where
                                numberOfWins = length (filter (==52) scores)
                                scores = playNtimes (seed,numGames)
                                averageScore =  sum scores `div` numGames


----------------------------------------HELPER METHODS ----------------------------------------
----------------------------------------HELPER METHODS FOR MOVES FROM RES TO COL ----------------------------------------
--get list of kings in res 
resKings :: [Card] -> [Card]
resKings [] = []
resKings res = filter isKing res
--heck if there is an Ecol (empty column)
isEcol :: [[Card]] -> Bool
isEcol [] = False
isEcol (col:cols) | null col = True
                  | otherwise = isEcol cols
--check if there is more than one Ecol
isEcols :: [[Card]] -> Bool
isEcols [] = False
isEcols cols | length[x | x<- cols, null x]>1 = True
             | otherwise = False
--add king to an empty column
addResKingToEcol :: Card -> [Card] -> [Card]
addResKingToEcol c col | null col = col++[c]
                       | otherwise = col
--check if there is a king in colHeads
isKingCol :: [[Card]] -> Bool
isKingCol cols | any isKing (colHeads cols) = True
               | otherwise = False
--helper method to get kings in col heads
colKings :: [[Card]] -> [Card]
colKings cols = filter isKing (colHeads cols)
--check if a card in res has its sucsessor in colHeads
resColSucc :: [Card] -> [[Card]] -> Bool
resColSucc [] _ = False
resColSucc _ [] = False
resColSucc _ [[]] = False
resColSucc res cols | any ((==True) . (\c -> if sCard c `elem` colHeads cols then True else False)) res = True
                    | otherwise = False
--get a column of length 1 if exist
getSingleCol :: [[Card]] -> [Card]
getSingleCol [[]] = []
getSingleCol [] = []
getSingleCol (col:cols) | length col == 1 = col
                        | otherwise = getSingleCol cols
----------------------------------------HELPER METHODS FOR MOVES FROM COL TO COL ----------------------------------------
--check if a card in column head have its successor in another column head
colColSucc :: [[Card]] -> Bool
colColSucc [[]] = False
colColSucc [] = False
colColSucc cols | any ((==True) . (\col -> if col/=[]&&sCard (head col) `elem` colHeads cols then True else False)) cols = True
                | otherwise = False
--to get list of successor cards in a column 
getSuccCards :: [Card] -> [Card]
getSuccCards [] = []
getSuccCards [UCard _ _] = []
getSuccCards [DCard _ _] = []
getSuccCards (x:y:xs) | sCard x == y = x:getSuccCards (y:xs)
                      | otherwise = x : ([y | sCard x == y])
--get successor cards (ordered cards of any length ) in a column if there are any
colNthSucc :: [[Card]] -> [Card]
colNthSucc [[]] = []
colNthSucc [] = []
colNthSucc (col:cols) | length (getSuccCards col)>1  = getSuccCards col
                      | otherwise = colNthSucc cols

----------------------------------------HELPER METHODS FOR MOVES FROM COL TO RES ----------------------------------------
---all methods to help move the "the exposer" card from colHeads to res
--check if there is an ace as a second top card in col
aceSecondCol :: [[Card]] -> [Card]
aceSecondCol [[]] = []
aceSecondCol [] = []
aceSecondCol (col:cols) | length col>=2&& isAce (head (tail col)) = [head col]
                        | otherwise = aceSecondCol cols
--check if there is a king as a second top card in col and there is an empty column
kingSecondCol :: [[Card]] -> [Card]
kingSecondCol [] = []
kingSecondCol (col:cols) | length col>=2&& isEcol cols && isKing (head (tail col)) = [head col]
                           | otherwise = kingSecondCol cols
--check if there is pCard in foundations of any column second top card
fondPreSecondCol :: [[Card]] -> [Card] -> [Card]
fondPreSecondCol [[]] _ = []
fondPreSecondCol [] _ = []
fondPreSecondCol (col:cols) fonds |length col>=2&& pCard (head (tail col)) `elem` fonds&& not(isKing (head (tail col))) = [head col]
                                  | otherwise = fondPreSecondCol cols fonds
--check if there is pCard in colHeads of any column second top card and the head is not successor
colPreSecondCol :: [[Card]] -> [Card]
colPreSecondCol [[]] = []
colPreSecondCol [] = []
colPreSecondCol (col:cols) | length col>=2 && sCard (head (tail col)) `elem` colHeads cols && pCard (head (tail col)) /= head col = [head col]
                           | otherwise = colPreSecondCol cols
----------------------------------------HELPER METHODS FOR PLAY GAME 8-OF ----------------------------------------
--helper method to check all columns are empty
areEcols :: [[Card]] -> Bool
areEcols [[]] = True
areEcols [] = False
areEcols (col:cols) | (not.null) col = False
                    | otherwise = areEcols cols
haveWon :: Board -> Bool --winning state if res and cols are empty but the foundation is not 
haveWon (EOBoard fonds cols res) | null res && areEcols cols && (not.null) fonds= True
                                 | otherwise = False
haveWon (SBoard fonds cols res) = False
--helper method to get maybe board to board
maybeToBoard :: Maybe Board -> Board
maybeToBoard (Just (EOBoard fonds cols res)) = EOBoard fonds cols res
maybeToBoard Nothing = EOBoard [] [[]] []
maybeToBoard (Just (SBoard fonds cols res)) = SBoard fonds cols res
--to get the score by calculating how much cards are in fonds
getScore :: Board -> Int
getScore (EOBoard fonds cols res) = 52 - length res - foldr ((+) . length) 0 cols
getScore (SBoard fonds cols res) = 0

--------------------------------------Template Code----------------------------------------------
{- Paste the contents of this file, including this comment, into your source file, below all
     of your code. You can change the indentation to align with your own, but other than this,
     ONLY make changes as instructed in the comments.
   -}
-- Constants that YOU must set:
studentName = "Zahra Hasan"
studentNumber = "200172316"
studentUsername = "acb20za"

initialBoardDefined = eightof {- replace XXX with the name of the constant that you defined
                            in step 3 of part 1 -}
secondBoardDefined = spider {- replace YYY with the constant defined in step 5 of part 1,
                            or if you have chosen to demonstrate play in a different game
                            of solitaire for part 2, a suitable contstant that will show
                            your play to good effect for that game -}

{- Beyond this point, the ONLY change you should make is to change the comments so that the
    work you have completed is tested. DO NOT change anything other than comments (and indentation
    if needed). The comments in the template file are set up so that only the constant eight-off
    board from part 1 and the toFoundations function from part 1 are tested. You will probably
    want more than this tested.

    CHECK with Emma or one of the demonstrators if you are unsure how to change this.

    If you mess this up, your code will not compile, which will lead to being awarded 0 marks
    for functionality and style.
-}

main :: IO()
main =
    do
        putStrLn $ "Output for " ++ studentName ++ " (" ++ studentNumber ++ ", " ++ studentUsername ++ ")"

        putStrLn "***The eight-off initial board constant from part 1:"
        print initialBoardDefined

        let board = toFoundations initialBoardDefined
        putStrLn "***The result of calling toFoundations on that board:"
        print board

    {- Move the start comment marker below to the appropriate position.
    If you have completed ALL the tasks for the assignment, you can
    remove the comments from the main function entirely.
    DO NOT try to submit/run non-functional code - you will receive 0 marks
    for ALL your code if you do, even if *some* of your code is correct.
    -}

        let boards = findMoves board      -- show that findMoves is working
        putStrLn "***The possible next moves after that:"
        print boards

        let chosen = chooseMove board     -- show that chooseMove is working
        putStrLn "***The chosen move from that set:"
        print chosen

        putStrLn "***Now showing a full game"     -- display a full game
        score <- displayGame initialBoardDefined 0
        putStrLn $ "Score: " ++ score
        putStrLn $ "and if I'd used playSolitaire, I would get score: " ++ show (playSolitaire initialBoardDefined)

        {- start comment marker - move this if appropriate
        putStrLn "\n\n\n************\nNow looking at the alternative game:"

        putStrLn "***The spider initial board constant from part 1 (or equivalent if playing a different game of solitaire):"
        print secondBoardDefined          -- show the suitable constant. For spider solitaire this
                                        -- is not an initial game, but a point from which the game
                                        -- can be won

        putStrLn "***Now showing a full game for alternative solitaire"
        score <- displayGame secondBoardDefined 0 -- see what happens when we play that game (assumes chooseMove
                                                -- works correctly)
        putStrLn $ "Score: " ++ score
        putStrLn $ "and if I'd used playSolitaire, I would get score: " ++ show (playSolitaire secondBoardDefined)

    -}

{- displayGame takes a Board and move number (should initially be 0) and
    displays the game step-by-step (board-by-board). The result *should* be
    the same as performing playSolitaire on the initial board, if it has been
    implemented correctly.
    DO NOT CHANGE THIS CODE other than aligning indentation with your own.
-}
displayGame :: Board -> Int ->IO String
displayGame board n =
    if haveWon board
        then return "A WIN"
        else
        do
            putStr ("Move " ++ show n ++ ": " ++ show board)
            let maybeBoard = chooseMove board
            if isJust maybeBoard then
                do
                    let (Just newBoard) = maybeBoard
                    displayGame newBoard (n+1)
            else
                do
                    let score = show (playSolitaire board)
                    return score