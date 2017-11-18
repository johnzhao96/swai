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

getPieceBB :: Board -> Piece -> Word64
getPieceBB board (Piece color piecetype) = getPieceTypeBB board piecetype .&. getColorBB board color


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

emptyBoard :: Board
emptyBoard = Board 0 0 0 0 0 0 0 0 (False, False, False, False) [] White


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

getPieceAtBB :: Board -> Word64 -> Piece
getPieceAtBB board bb =
    if      bb .&. whitePieces board /= 0 then Piece White (getPieceTypeAtBB board bb)
    else if bb .&. blackPieces board /= 0 then Piece Black (getPieceTypeAtBB board bb)
    else error "Not a valid piece bitboard."

-- Get the piece type from a bitboard with EXACTLY 1 SET BIT. No errors will be given.
getPieceTypeAtBB :: Board -> Word64 -> PieceType
getPieceTypeAtBB board bb = 
    if      bb .&. kings board   /= 0x0 then King
    else if bb .&. queens board  /= 0x0 then Queen
    else if bb .&. bishops board /= 0x0 then Bishop
    else if bb .&. knights board /= 0x0 then Knight
    else if bb .&. rooks board   /= 0x0 then Rook
    else if bb .&. pawns board   /= 0x0 then Pawn
    else error "Not a valid piece bitboard."

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

{- Generating moves -}
{- WIP, very untested - but it typechecks ;) -}

lowestBit :: Word64 -> Word64
lowestBit x = x .&. (-x)

bitSeq :: Word64 -> [Word64]
bitSeq 0 = []
bitSeq x = (\x' -> x' : bitSeq (x `xor` x')) (lowestBit x)

attackToMove :: Board -> Piece -> Word64 -> Move
attackToMove board (Piece piecetype color) attackBB = undefined

getPieceMoves :: Board -> Piece -> [Move]
getPieceMoves board piece@(Piece color piecetype) =
    let
        oColor = getColorBB board color
        sColor = getColorBB board (oppColor color)
        positions = getPieceBB board piece
        attacks p = getAttacks piecetype color sColor oColor p
        move p =
            let a = attacks p
            in case a .&. oColor of
                0 -> PieceMove p a piece
                p -> PieceCapture p a piece (getPieceAtBB board p)
    in
        map move (bitSeq positions)


{- Getting 'attacks' [WIP]-}

getAttacks :: PieceType -> Color -> Word64 -> Word64 -> Word64 -> Word64
getAttacks piece color blockers attackers positions = case piece of
    King   -> kingAttacks positions blockers
    Queen  -> queenAttacks positions blockers attackers
    Bishop -> bishopAttacks positions blockers attackers
    Knight -> knightAttacks positions blockers
    Rook   -> rookAttacks positions blockers attackers
    Pawn   -> pawnAttacks positions color blockers attackers

-- In the following: blockers are the same color, attackers are opposite.
-- The moves themselves will probably use these functions.
knightAttacks :: Word64 -> Word64 -> Word64
knightAttacks n blockers = (shiftLU 1  2 n .|. shiftLU 2 1 n .|.
                          shiftLU (-1) 2 n .|. shiftLU 2 (-1) n .|.
                          shiftLU 1 (-2) n .|. shiftLU (-2) 1 n .|.
                          shiftLU (-1) (-2) n .|. shiftLU (-2) (-1) n
                         ) .&. complement blockers

kingAttacks :: Word64 -> Word64 -> Word64
kingAttacks k blockers = (shiftLU (-1) (-1) k .|. shiftLU (-1) 0 k .|. shiftLU (-1) 1 k .|.
                        shiftLU 0 (-1) k .|. shiftLU 0 1 k .|.
                        shiftLU 1 (-1) k .|. shiftLU 1 0 k .|. shiftLU 1 1 k
                       ) .&. complement blockers

-- Kogge-Stone algorithm for sliding pieces.
-- There are faster algos that come at the cost of precomputing each rank/file
-- pair and each diagonal/antidiagonal pair.
koggeStone :: Word64 -> Word64 -> Word64 -> Int -> Int -> Word64
koggeStone pos blockers attackers l u =
    let
        open = complement (blockers .|. shiftLU l u attackers)
        pos1 = pos .|. (open .&. shiftLU l u pos)
        open1 = open .&. shiftLU l u open
        pos2 = pos .|. (open .&. shiftLU (2*l) (2*u) pos)
        open2 = open .&. shiftLU (2*l) (2*u) open
        pos3 = pos .|. (open .&. shiftLU (4*l) (4*u) pos)
    in
        pos3

rookAttacks :: Word64 -> Word64 -> Word64 -> Word64
rookAttacks r blockers attackers =
    let ks = koggeStone r blockers attackers
    in  ks (-1) 0 .|. ks 1 0 .|. ks 0 (-1) .|. ks 0 1

bishopAttacks :: Word64 -> Word64 -> Word64 -> Word64
bishopAttacks r blockers attackers =
    let ks = koggeStone r blockers attackers
    in  ks 1 1 .|. ks 1 (-1) .|. ks (-1) 1 .|. ks (-1) (-1)

queenAttacks :: Word64 -> Word64 -> Word64 -> Word64
queenAttacks q blockers attackers = rookAttacks q blockers attackers .|.
                                    bishopAttacks q blockers attackers

-- EN PASSANT STUFF NOT IMPLEMENTED YET
-- Do we need to treat promotion differently? Probably not here.
pawnAttacks :: Word64 -> Color -> Word64 -> Word64 -> Word64
pawnAttacks p color blockers attackers =
    let
        compb = complement (blockers .|. attackers)
        dir = case color of {White -> 1; Black -> -1}
        hrow = case color of {White -> rankToBoard 2 0xff; Black -> rankToBoard 7 0xff}
        dmoves = shiftLU 0 dir (shiftLU 0 dir (hrow .&. p) .&. compb)
        smoves = shiftLU 0 dir p
        attacks = (shiftLU (-1) dir p .|. shiftLU 1 dir p) .&. attackers
    in
        ((smoves .|. dmoves) .&. compb) .|. attacks
