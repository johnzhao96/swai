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
                   } deriving (Eq)

-- Show instance of pieces
instance Show Piece where
    show piece = case piece of
        Piece White King   -> "\x2654"
        Piece Black King   -> "\x265A"
        Piece White Queen  -> "\x2655"
        Piece Black Queen  -> "\x265B"
        Piece White Rook   -> "\x2656"
        Piece Black Rook   -> "\x265C"
        Piece White Bishop -> "\x2657"
        Piece Black Bishop -> "\x265D"
        Piece White Knight -> "\x2658"
        Piece Black Knight -> "\x265E"
        Piece White Pawn   -> "\x2659"
        Piece Black Pawn   -> "\x265F"

data Color = White | Black deriving (Show, Eq)

data PieceType = King
               | Queen
               | Bishop
               | Knight
               | Rook
               | Pawn   deriving (Show, Eq)

data Move = PieceMove Word64 Word64 Piece
          | PieceCapture Word64 Word64 Piece Piece
          | CastleWK
          | CastleWQ
          | CastleBK
          | CastleBQ
          | EnPassant Word64 Word64
          | Promotion Word64 Word64 Piece


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

setPieceTypeBB :: Board -> PieceType -> Word64 -> Board
setPieceTypeBB board piecetype bitboard = case piecetype of
    King   -> board { kings = bitboard }
    Queen  -> board { queens = bitboard }
    Bishop -> board { bishops = bitboard }
    Knight -> board { knights = bitboard }
    Rook   -> board { rooks = bitboard }
    Pawn   -> board { pawns = bitboard }

getPieceTypeBB :: Board -> PieceType -> Word64
getPieceTypeBB board piecetype = case piecetype of
    King   -> kings board
    Queen  -> queens board
    Bishop -> bishops board
    Knight -> knights board
    Rook   -> rooks board
    Pawn   -> pawns board

setColorBB :: Board -> Color -> Word64 -> Board
setColorBB board color bitboard = case color of
    White -> board { whitePieces = bitboard }
    Black -> board { blackPieces = bitboard }

getColorBB :: Board -> Color -> Word64
getColorBB board color = case color of
    White -> whitePieces board
    Black -> blackPieces board


{- Color Operations -}

oppColor :: Color -> Color
oppColor color = case color of
    White -> Black
    Black -> White


{- Gameplay -}

makeIntMove :: Board -> (Piece, Int, Int) -> Board
makeIntMove board (piece, srcIdx, dstIdx) = makePieceMove board piece (bit srcIdx) (bit dstIdx)

makePieceMove :: Board -> Piece -> Word64 -> Word64 -> Board
makePieceMove board@(Board {currentPlayer = cp}) piece@(Piece _ piecetype) src dst =
    case makePieceMoveBB (getColorBB board cp) (getPieceTypeBB board piecetype) src dst of
        (colorBB, pieceBB) -> (setPieceTypeBB (setColorBB (board { currentPlayer = oppColor cp }) cp colorBB) piecetype pieceBB) 

makePieceMoveBB :: Word64 -> Word64 -> Word64 -> Word64 -> (Word64, Word64)
makePieceMoveBB colorBB pieceBB src dst = (colorBB .&. (complement src) .|. dst, pieceBB .&. (complement src) .|. dst) 


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

emptyBoard :: Board
emptyBoard = Board 0 0 0 0 0 0 0 0 (False, False, False, False) [] White

-- Takes a Board and returns a string representation of the position
boardToString :: Board -> [Char]
boardToString board = boardToString' 0 board []

boardToString' :: Int -> Board -> [Char] -> [Char]
boardToString' n board accum = case n of
    64  -> accum
    _   -> 
        let 
            spacing = if n `mod` 8 == 0 then '\n' else ' '
            displayPiece = case (getPieceAtLocation board n) of
                Just piece -> show piece
                Nothing    -> "."
        in
        boardToString' (n+1) board (displayPiece ++ (spacing : accum))
            

-- Gets the character representation of the piece at a particular index in the Board ('.' if there is no piece)
getPieceAtLocation :: Board -> Int -> Maybe Piece
getPieceAtLocation board idx =
    if      testBit (whitePieces board) idx then Just (Piece White (getPieceTypeAtLocation board idx))
    else if testBit (blackPieces board) idx then Just (Piece Black (getPieceTypeAtLocation board idx))
    else    Nothing

getPieceTypeAtLocation :: Board -> Int -> PieceType
getPieceTypeAtLocation board idx =
    if      testBit (kings board) idx   then King
    else if testBit (queens board) idx  then Queen
    else if testBit (bishops board) idx then Bishop
    else if testBit (knights board) idx then Knight
    else if testBit (rooks board) idx   then Rook
    else if testBit (pawns board) idx   then Pawn
    else error "Not a valid piece."

-- Represent a word (bitmap) as a string.
wordToString :: Word64 -> String
wordToString w = foldl (\l n -> bitToString w n : spaceStr n l) [] [0..63]
    where bitToString w l = if testBit w l then '1' else '0'
          spaceStr n l = if n `mod` 8 == 0 then '\n':l else l

-- Shift up/down/left/right without worrying about wrapping.
-- Note: In the future it might be better to replace uses of this function
--       with simple bit operations and hardcoded masks. No need to over
--       optimize yet though...
shiftLU :: Int -> Int -> Word64 -> Word64
shiftLU l u w = shift w (l + 8*u) .&. maskL l
    where maskL l = (shift 0xff l .&. 0xff) * 0x0101010101010101

-- In the following: blockers are the same color, attackers are opposite.
-- Maybe these should be renamed as 'knightAttacks' and similar.
-- The moves themselves will probably use these functions.
knightMoves :: Word64 -> Word64 -> Word64
knightMoves n blockers = (shiftLU 1  2 n .|. shiftLU 2 1 n .|.
                          shiftLU (-1) 2 n .|. shiftLU 2 (-1) n .|.
                          shiftLU 1 (-2) n .|. shiftLU (-2) 1 n .|.
                          shiftLU (-1) (-2) n .|. shiftLU (-2) (-1) n
                         ) .&. complement blockers

kingMoves :: Word64 -> Word64 -> Word64
kingMoves k blockers = (shiftLU (-1) (-1) k .|. shiftLU (-1) 0 k .|. shiftLU (-1) 1 k .|.
                        shiftLU 0 (-1) k .|. shiftLU 0 1 k .|.
                        shiftLU 1 (-1) k .|. shiftLU 1 0 k .|. shiftLU 1 1 k
                       ) .&. complement blockers

-- EN PASSANT STUFF NOT IMPLEMENTED YET
-- Do we need to treat promotion differently? Probably not here.
pawnMoves :: Bool -> Word64 -> Word64 -> Word64 -> Word64
pawnMoves isWhite p blockers attackers =
    let
        compb = complement (blockers .|. attackers)
        dir = if isWhite then 1 else -1
        hrow = if isWhite then rankToBoard 2 0xff else rankToBoard 7 0xff
        dmoves = shiftLU 0 dir (shiftLU 0 dir (hrow .&. p) .&. compb)
        smoves = shiftLU 0 dir p
        attacks = (shiftLU (-1) dir p .|. shiftLU 1 dir p) .&. attackers
    in
        ((smoves .|. dmoves) .&. compb) .|. attacks
