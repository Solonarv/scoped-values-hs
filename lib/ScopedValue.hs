{-# LANGUAGE MagicHash, QuasiQuotes #-}
module ScopedValue (ScopedValue, new, new', get, with, Binding(..), withMany) where

import Language.Haskell.TH

import DelimCont
import GHC.Exts (Any, unsafeCoerce#)
import System.IO.Unsafe (unsafePerformIO)

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
    ensureNoFreeVars = pure () -- todo

data Res a = Call !(IO a -> IO (Res a)) | Done Any

get :: ScopedValue a -> IO a
get (SV tag) = control0 tag \k -> pure (Call k)

with :: ScopedValue a -> a -> IO r -> IO r
with (SV tag) val act0 = interpret (Done . unsafeCoerce# <$> act0)
  where
    interpret act = prompt tag act >>= \case
      Call k -> interpret $ k (pure val)
      Done x -> pure (unsafeCoerce# x)

data Binding where (::=) :: forall a. ScopedValue a -> a -> Binding

-- later bindings win, if there's a conflict
withMany :: [Binding] -> IO r -> IO r
withMany = flip $ foldr \(s ::= v) io -> with s v io