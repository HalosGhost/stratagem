{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances    #-}

module Game.Stratagem.Piece where

data Rank = Empty | Flag | Spy | Scout | Miner | Sergeant
          | Lieutenant | Captain | Major | Colonel | General
          | Marshall | Bomb  | Lake
          deriving (Eq, Ord, Enum, Show)

data Player = Red | Blue | None
            deriving (Eq)

class Pretty a where
    pretty :: Player -> a -> String

instance Pretty Rank where
    pretty _ Empty    = " "
    pretty _ Flag     = "⚑"
    pretty _ Spy      = "S"
    pretty _ Marshall = "M"
    pretty _ Bomb     = "☀"
    pretty _ Lake     = " "
    pretty _ r        = show $ (fromEnum r) - 1

instance Show Player where
    show Red  = "\x1b[1m\x1b[31m"
    show Blue = "\x1b[1m\x1b[34m"
    show None = ""

data Piece = Piece
           { owner    :: Player
           , rank     :: Rank
           , revealed :: Bool
           } deriving (Eq)

instance Show Piece where
    show (Piece o r v)
       = show o ++ (if v then show r else "Unrevealed") ++ "\x1b[0m"

instance Pretty Piece where
    pretty owner (Piece o r v)
         = show o ++ (if display then pretty o r else "█") ++ "\x1b[0m"
         where display = v || owner == o || owner == None

canCollide :: Piece -> Piece -> Bool
canCollide a@(Piece o r _) d@(Piece o' r' _)
         = not $ o == o' || r == Lake || r' == Lake

reveal :: Piece -> Piece
reveal (Piece o r _) = Piece o r True

collide :: Piece -> Piece -> Piece
collide a@(Piece _ r _) d@(Piece _ s _)
      | s == Empty                  = a'
      | r == Miner && s == Bomb     = a'
      | r == Spy   && s == Marshall = a'
      | r == s                      = p
      | otherwise                   = max
      where p   = Piece None Empty False
            a'  = reveal a
            d'  = reveal d
            max = if r > s then a' else d'

range :: Piece -> Word
range p = case rank p of
               Empty     -> 0
               Flag      -> 0
               Bomb      -> 0
               Lake      -> 0
               Scout     -> 9
               otherwise -> 1
