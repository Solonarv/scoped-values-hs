{-# LANGUAGE MagicHash, UnboxedTuples #-}
module DelimCont where

import GHC.Exts
import GHC.IO (IO(..))

data PromptTag a = PT (PromptTag# a)

newPromptTag :: IO (PromptTag a)
newPromptTag = IO \s -> let !(# s', tag# #) = newPromptTag# s in (# s', PT tag# #)

prompt :: forall a. PromptTag a -> IO a -> IO a
prompt (PT tag#) act = coerce (prompt# @a tag#) act

control0 :: forall a b. PromptTag a -> ((IO b -> IO a) -> IO a) -> IO b
control0 (PT tag#) = coerce (control0# @a @b tag#)