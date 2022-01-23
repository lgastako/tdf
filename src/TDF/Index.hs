{-# LANGUAGE NoImplicitPrelude #-}

module TDF.Index
  ( Index
  ) where

data Index idx = Index
  { indexes :: Vec
  }
