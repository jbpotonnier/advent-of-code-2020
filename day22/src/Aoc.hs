module Aoc
  ( module Aoc,
  )
where

import Data.Maybe (fromJust)
import Data.Sequence (Seq (Empty, (:<|)))
import qualified Data.Sequence as Seq
import qualified Data.Set as Set

data Game = Game
  { player1 :: Seq Int,
    player2 :: Seq Int
  }
  deriving (Show, Ord, Eq)

data Player = P1 | P2
  deriving (Show, Eq)

play2 :: Game -> (Player, Game) -- (winner, game)
play2 initGame =
  let endGame@Game {player1, player2} = go Set.empty initGame
   in case (player1, player2) of
        (Empty, _) -> (P2, endGame)
        (_, Empty) -> (P1, endGame)
        _ -> error "Game should be finished"
  where
    go seen g
      | isFinished g = g
      | Set.member g seen = g {player2 = Seq.empty}
      | otherwise = go (Set.insert g seen) (step g)
      where
        step :: Game -> Game
        step game@Game {player1, player2} =
          case (player1, player2) of
            (Empty, _) -> game
            (_, Empty) -> game
            (c1 :<| c1s, c2 :<| c2s)
              | Seq.length c1s >= c1 && Seq.length c2s >= c2 ->
                case play2 Game {player1 = Seq.take c1 c1s, player2 = Seq.take c2 c2s} of
                  (P1, _) -> Game {player1 = c1s <> fromList [c1, c2], player2 = c2s}
                  (P2, _) -> Game {player1 = c1s, player2 = c2s <> fromList [c2, c1]}
              | otherwise ->
                if c1 > c2
                  then Game {player1 = c1s <> fromList [c1, c2], player2 = c2s}
                  else Game {player1 = c1s, player2 = c2s <> fromList [c2, c1]}

    isFinished :: Game -> Bool
    isFinished Game {player1, player2} = Seq.null player1 || Seq.null player2

play :: Game -> Game
play = head' . dropWhile (not . isFinished) . iterate step
  where
    isFinished :: Game -> Bool
    isFinished Game {player1, player2} = Seq.null player1 || Seq.null player2

    step :: Game -> Game
    step game@Game {player1, player2} =
      case (player1, player2) of
        (Empty, _) -> game
        (_, Empty) -> game
        (c1 :<| c1s, c2 :<| c2s)
          | c1 > c2 ->
            Game
              { player1 = c1s <> fromList [c1, c2],
                player2 = c2s
              }
          | otherwise ->
            Game
              { player1 = c1s,
                player2 = c2s <> fromList [c2, c1]
              }

score :: Game -> Int
score game@Game {player1, player2} =
  case (player1, player2) of
    (Empty, cs) -> compute cs
    (cs, Empty) -> compute cs
    _ -> error $ "score: game is not finished " <> show game
  where
    compute = sum . fmap (uncurry (*)) . enumerate . Seq.reverse

enumerate :: Seq b -> Seq (Int, b)
enumerate s = Seq.zip (fromList [1 .. Seq.length s]) s

head' :: [c] -> c
head' = fromJust . viaNonEmpty head
