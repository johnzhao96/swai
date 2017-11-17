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

-- Move data type, consists of type of move, initial square, end square
data Move = Move {move :: (Int, Int)
                , kind :: Int}

-- Kind Data type describes kinds of pieces, None is for errors
data Kind  = King | Queen | Bishop | Knight | Rook | Pawn | None

-- Piece Data type is Color, Kind
data Piece = Piece (Bool, Kind) 

-- Show instance of pieces
instance Show Piece where
    show piece = case piece of
        Piece (True, King)    -> "\x2654"
        Piece (False, King)   -> "\x265A"
        Piece (True, Queen)   -> "\x2655"
        Piece (False, Queen)  -> "\x265B"
        Piece (True, Rook)    -> "\x2656"
        Piece (False, Rook)   -> "\x265C"
        Piece (True, Bishop)  -> "\x2657"
        Piece (False, Bishop) -> "\x265D"
        Piece (True, Knight)  -> "\x2658"
        Piece (False, Knight) -> "\x265E"
        Piece (True, Pawn)    -> "\x2659"
        Piece (False, Pawn)   -> "\x265F"
        Piece (_, None)       -> "."


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
        boardToString' (n+1) board (show (getPieceAtLocation board n) ++ (spacing : accum))

getPieceAtLocation :: Board -> Int -> Piece
getPieceAtLocation board idx =
    if      testBit (whitePieces board) idx then Piece (True, (getPieceTypeAtLocation board idx))
    else if testBit (blackPieces board) idx then Piece (False, (getPieceTypeAtLocation board idx))
    else                                         Piece (True, None)

getPieceTypeAtLocation :: Board -> Int -> Kind
getPieceTypeAtLocation board idx =
    if      testBit (kings board)   idx then King
    else if testBit (queens board)  idx then Queen
    else if testBit (bishops board) idx then Bishop
    else if testBit (knights board) idx then Knight
    else if testBit (rooks board)   idx then Rook
    else if testBit (pawns board)   idx then Pawn
    else                                     None


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


-- Operates on a piece of a given type
operatePiece :: Piece -> (Word64 -> Word64) -> Board -> Board
operatePiece (Piece (color, piece)) f (Board wh bl ks qs bs ns rs ps cas enp cur) = case piece of
    King   | color     -> Board (f wh) bl (f ks) qs bs ns rs ps cas enp cur
           | otherwise -> Board wh (f bl) (f ks) qs bs ns rs ps cas enp cur
    Queen  | color     -> Board (f wh) bl ks (f qs) bs ns rs ps cas enp cur
           | otherwise -> Board wh (f bl) ks (f qs) bs ns rs ps cas enp cur
    Bishop | color     -> Board (f wh) bl ks qs (f bs) ns rs ps cas enp cur
           | otherwise -> Board wh (f bl) ks qs (f bs) ns rs ps cas enp cur
    Knight | color     -> Board (f wh) bl ks qs bs (f ns) rs ps cas enp cur
           | otherwise -> Board wh (f bl) ks qs bs (f ns) rs ps cas enp cur
    Rook   | color     -> Board (f wh) bl ks qs bs ns (f rs) ps cas enp cur
           | otherwise -> Board wh (f bl) ks qs bs ns (f rs) ps cas enp cur
    Pawn   | color     -> Board (f wh) bl ks qs bs ns rs (f ps) cas enp cur
           | otherwise -> Board wh (f bl) ks qs bs ns rs (f ps) cas enp cur


-- Makes a square on a board empty
emptySquare :: Int -> Board -> Board
emptySquare square board = operateAll ((.&.) (complement (bit square))) board

-- Places a piece on a certain square
placePiece :: Piece -> Int -> Board -> Board
placePiece piece square board = operatePiece piece (bit square .|.) board


makeMove :: Move -> Board -> Board
makeMove (Move (start, end) kind) board = placePiece
                                          (getPieceAtLocation board start)
                                          end
                                          (emptySquare end (emptySquare start board))