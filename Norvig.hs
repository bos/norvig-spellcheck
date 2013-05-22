{-# LANGUAGE BangPatterns, OverloadedStrings #-}

module Main (main) where

import Control.Monad
import Data.List
import Data.Monoid
import Data.Time.Clock
import System.Environment
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B8
import qualified Data.HashMap.Strict as M
import qualified Data.HashSet as S

asciiWords = go . lower
  where go bs | B.null pfx = []
              | otherwise = w : go ws
          where pfx = B.dropWhile (not . interesting) bs
                (w,ws) = B.span interesting pfx
        interesting c = c >= 97 && c <= 122
        lower = B.map l
          where l c | c >= 65 && c <= 90 = c + 32
                    | otherwise          = c

train = foldl' go M.empty . asciiWords
  where go m w = M.insertWith (const (+1)) w (2::Int) m

edits1 word = S.fromList . concat $ [
                [a <> B.tail b | (a,b) <- initSplits]
              , [let (h,t) = B.splitAt 2 b
                 in a <> B.reverse h <> t | (a,b) <- init initSplits]
              , [a <> c <> B.tail b | (a,b) <- initSplits, c <- alphabet]
              , [a <> c <> b | (a,b) <- splits, c <- alphabet]
              ]
  where
    splits = B.inits word `zip` B.tails word
    initSplits = init splits
    alphabet = map B8.singleton ['a'..'z']

known word nwords = S.filter (`M.member` nwords) word

knownEdits2 word nwords = S.foldl' go S.empty (edits1 word)
  where go s w = S.union s (known (edits1 w) nwords)

correct word nwords = snd . S.foldl' best (0,"") $
    known (S.singleton word) nwords ||| known (edits1 word) nwords |||
    knownEdits2 word nwords ||| S.singleton word
  where
    best sw@(score,_) w = case M.lookup w nwords of
                              Just s | s > score -> (s,w)
                              _ -> sw
    a ||| b = if S.null a then b else a
    infixr 2 |||

timed desc act = do
  start <- getCurrentTime
  !ret <- act
  end <- getCurrentTime
  putStrLn $ desc ++ ": " ++ show (end `diffUTCTime` start)
  return ret

main = do
  nwords <- timed "train" (train `fmap` B.readFile "big.txt")
  args <- getArgs
  let corr w = do
        let cw = correct w nwords
        unless (cw == w) $ print (w, cw)
  timed "correct" . forM_ args $ \arg ->
    case arg of
      ('@':name) -> B.readFile name >>= (mapM_ corr . asciiWords)
      _ -> corr (B8.pack arg)
