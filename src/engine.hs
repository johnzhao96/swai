module Swai where

import Data.Maybe
import Data.Word
import Data.Bits

data Board = Board { whitePieces    :: {-# UNPACK #-} !Word64
                   , blackPieces    :: {-# UNPACK #-} !Word64
                   , kings          :: {-# UNPACK #-} !Word64
                   , queens         :: {-# UNPACK #-} !Word64
                   , bishops        :: {-# UNPACK #-} !Word64
                   , knights        :: {-# UNPACK #-} !Word64
                   , rooks          :: {-# UNPACK #-} !Word64
                   , pawns          :: {-# UNPACK #-} !Word64
                   , canCastle      :: (Bool, Bool, Bool, Bool)
                   , enPassant      :: [ Int ]
                   , currentPlayer  :: Bool -- White is true 
                   } deriving (Show, Eq)

{- Piece Getters -}

whiteKing :: Board -> Word64
whiteKing (Board wh bl ks qs bs ns rs ps _ _ _) = wh .&. ks

blackKing :: Board -> Word64
blackKing (Board wh bl ks qs bs ns rs ps _ _ _) = bl .&. ks

whiteQueens :: Board -> Word64
whiteQueens (Board wh bl ks qs bs ns rs ps _ _ _) = wh .&. qs

blackQueens :: Board -> Word64
blackQueens (Board wh bl ks qs bs ns rs ps _ _ _) = bl .&. qs

whiteBishops :: Board -> Word64
whiteBishops (Board wh bl ks qs bs ns rs ps _ _ _) = wh .&. bs

blackBishops :: Board -> Word64
blackBishops (Board wh bl ks qs bs ns rs ps _ _ _) = bl .&. bs

whiteKnights :: Board -> Word64
whiteKnights (Board wh bl ks qs bs ns rs ps _ _ _) = wh .&. ns

blackKnights :: Board -> Word64
blackKnights (Board wh bl ks qs bs ns rs ps _ _ _) = bl .&. ns

whiteRooks :: Board -> Word64
whiteRooks (Board wh bl ks qs bs ns rs ps _ _ _) = wh .&. rs

blackRooks :: Board -> Word64
blackRooks (Board wh bl ks qs bs ns rs ps _ _ _) = bl .&. rs

whitePawns :: Board -> Word64
whitePawns (Board wh bl ks qs bs ns rs ps _ _ _) = wh .&. ps

blackPawns :: Board -> Word64
blackPawns (Board wh bl ks qs bs ns rs ps _ _ _) = bl .&. ps


-- Takes a rank (8 bits), and makes it into a bitboard with 0s everywhere but that rank
rankToBoard :: Int -> Word64 -> Word64
rankToBoard rank rankContents = shiftL rankContents (8 * (rank - 1))

startingBoard :: Board
startingBoard = Board ((rankToBoard 1 0xff) .|. (rankToBoard 2 0xff))
                      ((rankToBoard 7 0xff) .|. (rankToBoard 8 0xff))
                      ((rankToBoard 1 0x08) .|. (rankToBoard 8 0x08))
                      ((rankToBoard 1 0x10) .|. (rankToBoard 8 0x10))
                      ((rankToBoard 1 0x24) .|. (rankToBoard 8 0x24))
                      ((rankToBoard 1 0x42) .|. (rankToBoard 8 0x42))
                      ((rankToBoard 1 0x81) .|. (rankToBoard 8 0x81))
                      ((rankToBoard 2 0xff) .|. (rankToBoard 7 0xff))
                      (True, True, True, True)
                      []
                      True

emptyBoard :: Board
emptyBoard = Board 0 0 0 0 0 0 0 0 (False, False, False, False) [] True

-- Shows Board, useful for testing
showBoard :: Board -> IO ()
showBoard board = putStrLn $ boardToString board

boardToString :: Board -> [Char]
boardToString board = boardToString' 0 board []

boardToString' :: Int -> Board -> [Char] -> [Char]
boardToString' n board accum = case n of
    64  -> accum
    _   -> 
        let 
            spacing = if n `mod` 8 == 0 then '\n' else ' '
        in
        boardToString' (n+1) board (getPieceAtLocation board n : spacing : accum)

getPieceAtLocation :: Board -> Int -> Char
getPieceAtLocation board idx =
    if      testBit (whitePieces board) idx then fst (getPieceTypeAtLocation board idx)
    else if testBit (blackPieces board) idx then snd (getPieceTypeAtLocation board idx)
    else '.' 

getPieceTypeAtLocation :: Board -> Int -> (Char, Char)
getPieceTypeAtLocation board idx =
    if      testBit (kings board) idx   then ('k', 'K')
    else if testBit (queens board) idx  then ('q', 'Q')
    else if testBit (bishops board) idx then ('b', 'B')
    else if testBit (knights board) idx then ('n', 'N')
    else if testBit (rooks board) idx   then ('r', 'R')
    else if testBit (pawns board) idx   then ('p', 'P')
    else                                     ('.', '.')


-- Represent a word (bitmap) as a string.
wordToString :: Word64 -> String
wordToString w = foldl (\l n -> bitToString w n : spaceStr n l) [] [0..63]
    where bitToString w l = if testBit w l then '1' else '0'
          spaceStr n l = if n `mod` 8 == 0 then '\n':l else l

-- Shift up/down/left/right without worrying about wrapping.
shiftLU :: Int -> Int -> Word64 -> Word64
shiftLU l u w = shift w (l + 8*u) .&. maskL l
    where maskL l = (shift 0xff l .&. 0xff) * 0x0101010101010101


-- Operates a bitwise operation on each Word64 in a board
operateAll :: (Word64 -> Word64) -> Board -> Board
operateAll f (Board wh bl ks qs bs ns rs ps cas enp cur) = Board (f wh) (f bl) (f ks) (f qs) (f bs) (f ns) (f rs) (f ps) cas enp cur


-- Indicate the square you want empty, 0-63
emptySquare :: Int -> Board -> Board
emptySquare square board = operateAll ((.&.) (complement (bit square))) board


-- Places a given piece of a chosen color on a square, 0-63
placeKing :: Bool -> Int -> Board -> Board
placeKing color square (Board wh bl ks qs bs ns rs ps cas enp cur)
   | color = Board (bit square .|. wh) bl (bit square .|. ks) qs bs ns rs ps cas enp cur
   | otherwise = Board wh (bit square .|. bl) (bit square .|. ks) qs bs ns rs ps cas enp cur

placeQueen :: Bool -> Int -> Board -> Board
placeQueen color square (Board wh bl ks qs bs ns rs ps cas enp cur)
   | color = Board (bit square .|. wh) bl ks (bit square .|. qs) bs ns rs ps cas enp cur
   | otherwise = Board wh (bit square .|. bl) ks (bit square .|. qs) bs ns rs ps cas enp cur

placeBishop :: Bool -> Int -> Board -> Board
placeBishop color square (Board wh bl ks qs bs ns rs ps cas enp cur) 
   | color = Board (bit square .|. wh) bl ks qs (bit square .|. bs) ns rs ps cas enp cur
   | otherwise = Board wh (bit square .|. bl) ks qs (bit square .|. bs) ns rs ps cas enp cur

placeKnight :: Bool -> Int -> Board -> Board
placeKnight color square (Board wh bl ks qs bs ns rs ps cas enp cur) 
   | color = Board (bit square .|. wh) bl ks qs bs (bit square .|. ns) rs ps cas enp cur
   | otherwise = Board wh (bit square .|. bl) ks qs bs (bit square .|. ns) rs ps cas enp cur

placeRook :: Bool -> Int -> Board -> Board
placeRook color square (Board wh bl ks qs bs ns rs ps cas enp cur) 
   | color = Board (bit square .|. wh) bl ks qs bs ns (bit square .|. rs) ps cas enp cur
   | otherwise = Board wh (bit square .|. bl) ks qs bs ns (bit square .|. rs) ps cas enp cur

placePawn :: Bool -> Int -> Board -> Board
placePawn color square (Board wh bl ks qs bs ns rs ps cas enp cur) 
   | color = Board (bit square .|. wh) bl ks qs bs ns rs (bit square .|. ps) cas enp cur
   | otherwise = Board wh (bit square .|. bl) ks qs bs ns rs (bit square .|. ps) cas enp cur

-- Move data type, consists of type of move, initial square, end square
data Move = Move {move :: (Int, Int)
                , kind :: Int}

makeMove :: Move -> Board -> Board
makeMove (Move (start, end) kind) board
   | getPieceAtLocation board start == 'K' = placeKing False end (emptySquare start (emptySquare end board))
   | getPieceAtLocation board start == 'k' = placeKing True end (emptySquare start (emptySquare end board))
   | getPieceAtLocation board start == 'Q' = placeQueen False end (emptySquare start (emptySquare end board))
   | getPieceAtLocation board start == 'q' = placeQueen True end (emptySquare start (emptySquare end board))
   | getPieceAtLocation board start == 'B' = placeBishop False end (emptySquare start (emptySquare end board))
   | getPieceAtLocation board start == 'b' = placeBishop True end (emptySquare start (emptySquare end board))
   | getPieceAtLocation board start == 'N' = placeKnight False end (emptySquare start (emptySquare end board))
   | getPieceAtLocation board start == 'n' = placeKnight True end (emptySquare start (emptySquare end board))
   | getPieceAtLocation board start == 'R' = placeRook False end (emptySquare start (emptySquare end board))
   | getPieceAtLocation board start == 'r' = placeRook True end (emptySquare start (emptySquare end board))
   | getPieceAtLocation board start == 'P' = placePawn False end (emptySquare start (emptySquare end board))
   | getPieceAtLocation board start == 'p' = placePawn True end (emptySquare start (emptySquare end board))
