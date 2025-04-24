{-# LANGUAGE MagicHash, TemplateHaskellQuotes #-}
module ScopedValue (ScopedValue, new, new', get, with, Binding(..), withMany, forkChild) where

import Language.Haskell.TH

import DelimCont
import GHC.Exts (Any, unsafeCoerce#)
import System.IO.Unsafe (unsafePerformIO)

import Data.Maybe
import Control.Concurrent
import Control.Exception
import Data.IORef
import Data.Map.Strict qualified as Map
import Data.Map.Strict (Map)

newtype ScopedValue a = SV (PromptTag (Res a))

new :: IO (ScopedValue a)
new = SV <$> newPromptTag

new' :: String -> TypeQ -> DecsQ
new' s tyq = do
  body <- [| unsafePerformIO new |]
  ty <- [t| ScopedValue $(tyq) |]
  ensureNoFreeVars ty
  pure
    [ SigD nm ty
    , FunD nm [Clause [] (NormalB body) []]
    , PragmaD $ InlineP nm NoInline ConLike AllPhases
    ]
  where
    nm = mkName s
    ensureNoFreeVars _ = pure () -- todo

data Res a = Call !(IO a -> IO (Res a)) | Done Any

get :: ScopedValue a -> IO a
get (SV tag) = control0 tag \k -> pure (Call k)

with :: ScopedValue a -> a -> IO r -> IO r
with sv@(SV tag) val act0 = do
  me <- myThreadId
  bracket (register me) (\_ -> unregister me) $ \_ -> interpret (Done . unsafeCoerce# <$> act0)
  where
    interpret act = prompt tag act >>= \case
      Call k -> interpret $ k (pure val)
      Done x -> pure (unsafeCoerce# x)
    register me = atomicModifyIORef' activeScopedVars \m -> (Map.insertWith (++) me [sv ::= val] m, ())
    unregister me = atomicModifyIORef' activeScopedVars \m -> (Map.adjust (drop 1) me m, ())

data Binding where (::=) :: forall a. ScopedValue a -> a -> Binding

-- later bindings win, if there's a conflict
withMany :: [Binding] -> IO r -> IO r
withMany = flip $ foldr \(s ::= v) io -> with s v io

-- * concurrency stuff

forkChild :: IO () -> IO ThreadId
forkChild act = do
  me <- myThreadId
  binds <- fromMaybe [] . Map.lookup me <$> readIORef activeScopedVars
  forkIO (withMany binds act)

activeScopedVars :: IORef (Map ThreadId [Binding])
activeScopedVars = unsafePerformIO (newIORef mempty)
{-# NOINLINE activeScopedVars #-}

