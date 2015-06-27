{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances    #-}

module Game.Stratagem.Board where

import Game.Stratagem.Piece
import qualified Data.Sequence as S
import qualified Data.List     as L
import qualified Data.Foldable as F

type Row = S.Seq Piece

instance Pretty Row where
    pretty o r = "│ " ++ L.intercalate " │ " prettyPieces ++ " │"
               where prettyPieces = map (pretty o) . F.toList $ F.toList r

instance Pretty Board where
    pretty o b = bTop
              ++ lkLnSep bSep (S.take  3 b)               ++ bSep
              ++ pretty o     (S.index b 3)               ++ bLakTop
              ++ pLakeLn      (S.index b 4)               ++ bLakMid
              ++ pLakeLn      (S.index b 5)               ++ bLakBot
              ++ (lkLnSep bSep . S.take  3  $ S.drop 6 b) ++ bSep
              ++ pretty o     (S.index b 9)               ++ bBot
              where bTop       =   "┌───┬───┬───┬───┬───┬───┬───┬───┬───┬───┐\n"
                    bSep       = "\n├───┼───┼───┼───┼───┼───┼───┼───┼───┼───┤\n"
                    bLakTop    = "\n├───┼───┼───┴───┼───┼───┼───┴───┼───┼───┤\n"
                    bLakMid    = "\n├───┼───┤       ├───┼───┤       ├───┼───┤\n"
                    bLakBot    = "\n├───┼───┼───┬───┼───┼───┼───┬───┼───┼───┤\n"
                    bBot       = "\n└───┴───┴───┴───┴───┴───┴───┴───┴───┴───┘"
                    lkLnSep s  = L.intercalate s . F.toList . fmap (pretty o)
                    pLakeLn r  =  "│ " ++ L.intercalate " │ "
                               [ lkLnSep " │ " $ S.take 2 r
                               , lkLnSep "   " . S.take 2 $ S.drop 2 r
                               , lkLnSep " │ " . S.take 2 $ S.drop 4 r
                               , lkLnSep "   " . S.take 2 $ S.drop 6 r
                               , lkLnSep " │ " $ S.drop 8 r
                               ] ++ " │"

type Board = S.Seq Row
type Pos   = (Int, Int) -- (Row, Column)

(!) :: Board -> Pos -> Piece
s ! (r,c) = S.index (S.index s c) r

takeWhile' _ []      = []
takeWhile' p (x:xs)
         | p x       = x : takeWhile' p xs
         | otherwise = [x]

posInRange:: Pos -> Board -> [Pos]
posInRange p@(r,c) b
         | outOfBounds p         = []
         | range piece <= 0      = []
         | otherwise             = filter (canCollide piece . (b!)) targets
         where outOfBounds (x,y) = x < 0 || x > 9 || y < 0 || y > 9
               piece             = b ! p
               posInDir          = concat [ [(r, y)| y <- map (+ c) ranges]
                                          , [(x, c)| x <- map (+ r) ranges]
                                          ] where ranges = ps ++ ns
                                                  ps     = [1..9]
                                                  ns     = [-1, -2 .. -9]
               isBlank pos       = b ! pos == Piece None Empty True
               targets           = filter (not . outOfBounds)
                     $ case rank piece of
                            Scout      -> takeWhile' isBlank posInDir
                            otherwise  -> [ (r, c - 1), (r, c + 1)
                                          , (r - 1, c), (r + 1, c)
                                          ]

placePiece :: Piece -> Pos -> Board -> Board
placePiece p (r,c) b = S.update c (S.update r p $ S.index b c) b

movablePieces :: Player -> Board -> [Pos]
movablePieces p b = filter (canMove p) pieces
                  where pieces        = [(x,y)| x <- [0..9], y <- [0..9]]
                        canMove pl pc = (length $ posInRange pc b) > 0
                                     && owner (b!pc) == pl

movePiece :: Pos -> Pos -> Board -> Board
movePiece p@(r,c) p'@(r',c') b
        | invalidMove      = b
        | otherwise        = placePiece blank p $ placePiece winner p' b
         where invalidMove = notElem p' $ posInRange p b
               attacker    = b ! p
               defender    = b ! p'
               blank       = Piece None Empty True
               winner      = collide attacker defender

gameOver :: Board -> Player
gameOver b | hasLost Blue    = Red
           | hasLost Red     = Blue
           | otherwise       = None
           where hasLost   p = hasNoMove p || hasNoFlag p
                 hasNoMove p = length (movablePieces p b) == 0
                 hasNoFlag p = notElem (Piece p Flag False) pieces
                 pieces      = [b!(x,y)| x <- [0..9], y <- [0..9]]

newBoard :: Board
newBoard = S.fromList
         [ bline, bline, bline, bline, lline, lline, bline, bline, bline, bline
         ] where blank = Piece None Empty True
                 lake  = Piece None Lake  True
                 bline = S.fromList $ replicate 10 blank
                 lline = S.fromList
                       [ blank, blank
                       , lake , lake
                       , blank, blank
                       , lake , lake
                       , blank, blank
                       ]
