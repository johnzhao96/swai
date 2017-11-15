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
                   , canCastle      :: (Boolean, Boolean, Boolean, Boolean)
                   , enPassant      :: [ Int ]
                   , currentPlayer  :: Boolean -- White is true 
                   } deriving (Show, Eq)

{- Piece Getters -}

whiteKing :: Board -> Word64
whiteKing (Board wh bl ks qs bs ns rs ps _ _ _) = wh .&. ks

blackKing :: Board -> Word64
whiteKing (Board wh bl ks qs bs ns rs ps _ _ _) = bl .&. ks

whiteQueens :: Board -> Word64
whiteKing (Board wh bl ks qs bs ns rs ps _ _ _) = wh .&. qs

blackQueens :: Board -> Word64
whiteKing (Board wh bl ks qs bs ns rs ps _ _ _) = bl .&. qs

whiteBishops :: Board -> Word64
whiteKing (Board wh bl ks qs bs ns rs ps _ _ _) = wh .&. bs

blackBishops :: Board -> Word64
whiteKing (Board wh bl ks qs bs ns rs ps _ _ _) = bl .&. bs

whiteKnights :: Board -> Word64
whiteKing (Board wh bl ks qs bs ns rs ps _ _ _) = wh .&. ns

blackKnights :: Board -> Word64
whiteKing (Board wh bl ks qs bs ns rs ps _ _ _) = bl .&. ns

whiteRooks :: Board -> Word64
whiteKing (Board wh bl ks qs bs ns rs ps _ _ _) = wh .&. rs

blackRooks :: Board -> Word64
whiteKing (Board wh bl ks qs bs ns rs ps _ _ _) = bl .&. rs

whitePawns :: Board -> Word64
whiteKing (Board wh bl ks qs bs ns rs ps _ _ _) = wh .&. ps

blackPawns :: Board -> Word64
whiteKing (Board wh bl ks qs bs ns rs ps _ _ _) = bl .&. ps


-- Takes a rank (8 bits), and makes it into a bitboard with 0s everywhere but that rank
rankToBoard :: Int -> Word8 -> Word64
rankToBoard rank rankContents = shiftL rankContents (8 * (rank - 1))

startingBoard :: Board
startingBoard = Board ((rankToBoard 1 0xff) .|. (rankToBoard 2 0xff))
                      ((rankToBoard 7 0xff) .|. (rankToBoard 8 0xff))
                      ((rankToBoard 1 0x08) .|. (rankToBoard 8 0x08))
                      ((rankToBoard 1 0x10) .|. (rankToBoard 8 0x10))
                      ((rankToBoard 1 0x48) .|. (rankToBoard 8 0x48))
                      ((rankToBoard 1 0x84) .|. (rankToBoard 8 0x84))
                      ((rankToBoard 1 0xf1) .|. (rankToBoard 8 0xf1))
                      ((rankToBoard 2 0xff) .|. (rankToBoard 7 0xff))
                      (True, True, True, True)
                      []
                      True


{-
boardToString :: Board -> [ Char ]
boardToString = boardToString' 64

boardToString' :: Int -> Board -> [ Char ]
boardToString' n board =

getPieceAtLocation :: Board -> Int -> Char
getPieceAtLocation board idx =
    if (whitePieces board) .|. (blackPieces board) 
-}

