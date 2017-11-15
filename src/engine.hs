module Swai where

import Data.Maybe
import Data.Word

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
whiteKing (Board wh bl ks qs bs ns rs ps _ _ _) = wh & ks

blackKing :: Board -> Word64
whiteKing (Board wh bl ks qs bs ns rs ps _ _ _) = bl & ks

whiteQueens :: Board -> Word64
whiteKing (Board wh bl ks qs bs ns rs ps _ _ _) = wh & qs

blackQueens :: Board -> Word64
whiteKing (Board wh bl ks qs bs ns rs ps _ _ _) = bl & qs

whiteBishops :: Board -> Word64
whiteKing (Board wh bl ks qs bs ns rs ps _ _ _) = wh & bs

blackBishops :: Board -> Word64
whiteKing (Board wh bl ks qs bs ns rs ps _ _ _) = bl & bs

whiteKnights :: Board -> Word64
whiteKing (Board wh bl ks qs bs ns rs ps _ _ _) = wh & ns

blackKnights :: Board -> Word64
whiteKing (Board wh bl ks qs bs ns rs ps _ _ _) = bl & ns

whiteRooks :: Board -> Word64
whiteKing (Board wh bl ks qs bs ns rs ps _ _ _) = wh & rs

blackRooks :: Board -> Word64
whiteKing (Board wh bl ks qs bs ns rs ps _ _ _) = bl & rs

whitePawns :: Board -> Word64
whiteKing (Board wh bl ks qs bs ns rs ps _ _ _) = wh & ps

blackPawns :: Board -> Word64
whiteKing (Board wh bl ks qs bs ns rs ps _ _ _) = bl & ps

<<<<<<< HEAD
startingBoard :: Board
startingBoard = Board 0xffff000000000000
                      0x000000000000ffff
                      0x0800000000000008
                      0x1000000000000010
                      0x4800000000000048
                      0x8400000000000084
                      0xf1000000000000f1
                      0x00ff00000000ff00
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
    if (whitePieces board) | (blackPieces board) 
-}

