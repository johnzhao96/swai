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
                   , enPassant      :: [(Int, Int)]
                   , currentPlayer  :: Color 
                   } deriving (Show, Eq)

data Piece = Piece { color     :: Color
                   , pieceType :: PieceType
                   } deriving (Show, Eq)

data Color = White | Black deriving (Show, Eq)

data PieceType = King
               | Queen
               | Bishop
               | Knight
               | Rook
               | Pawn   deriving (Show, Eq)


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


{- Bitboard Getters and Setters with respect to the Piece datatype -}

setBitboard :: Board -> PieceType -> Word64 -> Board
setBitboard board piecetype bitboard = case piecetype of
    King   -> board { kings = bitboard }
    Queen  -> board { queens = bitboard }
    Bishop -> board { bishops = bitboard }
    Knight -> board { knights = bitboard }
    Rook   -> board { rooks = bitboard }
    Pawn   -> board { pawns = bitboard }

getBitboard :: Board -> PieceType -> Word64
getBitboard board piecetype = case piecetype of
    King   -> kings board
    Queen  -> queens board
    Bishop -> bishops board
    Knight -> knights board
    Rook   -> rooks board
    Pawn   -> pawns board

getColorBitboard :: Board -> Color -> Word64
getColorBitboard board color = case color of
    White -> whitePieces board
    Black -> blackPieces board


{- Color Operations -}

oppColor :: Color -> Color
oppColor color = case color of
    White -> Black
    Black -> White


{- Gameplay -}

makeMove' :: Board -> (Piece, Int, Int) -> Board
makeMove' = makeMoveBitboard'

makeMoveBitboard' :: Board -> (Piece, Int, Int) -> Board
makeMoveBitboard' board@(Board { whitePieces = wh, blackPieces = bl, currentPlayer = cp }) (piece@(Piece color piecetype), source, destination) = 
    case color of
        White -> let (wh', bl', pieceBitboard) = movePiece' (wh, bl, getBitboard board piecetype) source destination in
                 setBitboard (board { whitePieces = wh', blackPieces = bl', currentPlayer = oppColor cp}) piecetype pieceBitboard
        Black -> let (bl', wh', pieceBitboard) = movePiece' (bl, wh, getBitboard board piecetype) source destination in
                 setBitboard (board { whitePieces = wh', blackPieces = bl', currentPlayer = oppColor cp}) piecetype pieceBitboard

movePiece' :: (Word64, Word64, Word64) -> Int -> Int -> (Word64, Word64, Word64)
movePiece' (currColorBitboard, nextColorBitboard, pieceBitboard) source destination =
    let
        currColorBitboard' = setBit (clearBit currColorBitboard source) destination
        nextColorBitboard' = clearBit nextColorBitboard destination
        pieceBitboard'     = setBit (clearBit pieceBitboard source) destination
    in
    (currColorBitboard', nextColorBitboard', pieceBitboard')


{- Generating Boards -}

-- Takes a rank (8 bits), and makes it into a Word64 with 0s everywhere but that rank
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
                      White


{- Board Printing Functions -}

printBoard :: Board -> IO ()
printBoard = putStrLn . boardToString

-- Takes a Board and returns a string representation of the position
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

-- Gets the character representation of the piece at a particular index in the Board ('.' if there is no piece)
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

