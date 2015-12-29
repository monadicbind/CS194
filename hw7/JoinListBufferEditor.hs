--JoinListBufferEditor.hs

{-# LANGUAGE GeneralizedNewtypeDeriving, FlexibleInstances, InstanceSigs,FlexibleContexts #-}

module Main where

import Editor
import JoinListBuffer
import Sized
import Scrabble
import JoinList
import Buffer

main = runEditor editor $ ((Buffer.fromString (unlines
         [ "This buffer is for notes you don't want to save, and for"
         , "evaluation of steam valve coefficients."
         , "To load a different file, type the character L followed"
         , "by the name of the file."
         ])) :: JoinList (Score,Size) String )
