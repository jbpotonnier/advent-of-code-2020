module Day8
  ( module Day8,
  )
where

import Data.HashMap.Strict ((!?))
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Set as Set
import Data.Text (breakOn, strip, stripPrefix)

data Instr = Acc Int | Jmp Int | Nop
  deriving (Show, Eq)

type Program = HashMap Int Instr

data Env = Env {currentInstr :: Int, global :: Int}
  deriving (Show, Eq)

data IsLooping = Looping Env | NotLooping Env
  deriving (Show, Eq)

initEnv :: Env
initEnv = Env {currentInstr = 0, global = 0}

fixProgram :: Program -> [IsLooping]
fixProgram =
  filter (not . isLooping)
    . fmap (\p -> findLoop (evalProg p initEnv))
    . tryFix
  where
    isLooping = \case Looping _ -> True; NotLooping _ -> False

tryFix :: Program -> [Program]
tryFix p = [HashMap.adjust mutate i p | i <- indices]
  where
    indices = [k | (k, Jmp _) <- HashMap.toList p]
    mutate = \case Jmp _ -> Nop; instr -> instr

findLoop :: [Env] -> IsLooping
findLoop = go Set.empty
  where
    go seen = \case
      [] -> error "should not be empty"
      [e] -> NotLooping e
      e : es
        | Set.member (currentInstr e) seen -> Looping e
        | otherwise -> go (Set.insert (currentInstr e) seen) es

evalProg :: HashMap Int Instr -> Env -> [Env]
evalProg prog env =
  let instr = prog !? currentInstr env
   in case instr of
        Just i ->
          let nextEnv = evalInstr env i
           in nextEnv : evalProg prog nextEnv
        Nothing -> []

evalInstr :: Env -> Instr -> Env
evalInstr env = \case
  Acc i -> env {currentInstr = currentInstr env + 1, global = global env + i}
  Jmp r -> env {currentInstr = currentInstr env + r}
  Nop -> env {currentInstr = currentInstr env + 1}

readProgram :: FilePath -> IO (Maybe Program)
readProgram path =
  fmap mkProgram . traverse parseLine . lines <$> readFileText path
  where
    mkProgram = HashMap.fromList . zip [0 ..]

parseLine :: Text -> Maybe Instr
parseLine s =
  let (instr, arg) = breakOn " " s
      n = readInt arg
   in case instr of
        "acc" -> Acc <$> n
        "jmp" -> Jmp <$> n
        "nop" -> Just Nop
        _ -> Nothing

readInt :: Text -> Maybe Int
readInt =
  readMaybe
    . toString
    . \case (stripPrefix "+" -> Just n) -> n; s -> s
    . strip