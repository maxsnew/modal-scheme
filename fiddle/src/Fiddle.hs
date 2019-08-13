{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Fiddle where

import Control.Monad.Except
import Control.Monad.Writer

-- | AST
data Value
  = VBool Bool
  | VThnk Computation
  | VVal String

data Computation
  = CForce Value
  | CReturn Value
  | CBind Computation String Computation
  | CIf Value Computation Computation
  | CLambda String Computation
  | CApp Computation Value
  | CPrint Value Computation

newtype RTErr = RTErr { displayErr :: String }
  deriving (Show, Read, Eq, Ord)

mkErr :: String -> RTErr
mkErr = RTErr

newtype Trace = Trace { readOut :: [Bool] }
  deriving (Show, Read, Eq, Ord, Semigroup, Monoid)

data SemVal
  = SemBool Bool
  | SemThunk SemComp
  deriving (Show)

data SemComp
  = SemComp { returner :: Fiddler SemVal
            , takeArg  :: SemVal -> SemComp
            }

newtype Fiddler a = Fiddler { unFiddler :: ExceptT RTErr (Writer Trace) a }
  deriving (Show, Read, Eq, Ord, Monad, Applicative, Functor, MonadError RTErr, MonadWriter Trace)

runFiddler :: Fiddler a -> (Trace, Either RTErr a)
runFiddler = swap . runWriter . runExceptT . unFiddler
  where swap (a,b) = (b,a)

instance Show SemComp where
  show (SemComp ret tA) = "SemComp (" ++ show ret ++ ") *function*"

type Values = [(String, SemVal)]

interpV :: Value -> Values -> Either RTErr SemVal
interpV v g = case v of
  VBool b -> return $ SemBool b
  _ -> Left (RTErr "not yet implemented")

interpC :: Computation -> Values -> SemComp
interpC c g = case c of
  CReturn v -> case interpV v g of
                 Left  e -> errC e
                 Right v -> retC v
  CBind c x k -> bindC (interpC c g) (\v -> interpC k ((x,v) : g))
  _ -> errC $ RTErr "not yet implemented"

retC :: SemVal -> SemComp
retC v = SemComp
  { returner = return v
  , takeArg  = \ _ -> errC $ mkErr "Tried to return, but there was an argument on the stack"
  }

errC :: RTErr -> SemComp
errC e = SemComp
  { returner = throwError e
  , takeArg  = \ _ -> errC e
  }

bindC :: SemComp -> (SemVal -> SemComp) -> SemComp
bindC c k = case (runFiddler $ returner c) of
              (t, mayV) -> printAll t $ either errC k mayV

printAll :: Trace -> SemComp -> SemComp
printAll t c = SemComp
  { returner = tell t >> returner c
  , takeArg = \x -> printAll t $ takeArg c x
  }
  
-- printThen :: Bool -> SemComp -> SemComp
-- b `printThen` c = SemComp
--   { returner = tell ()
--   }
