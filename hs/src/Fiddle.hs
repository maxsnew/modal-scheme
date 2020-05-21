{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
module Fiddle where

import Control.Monad.Except
import Control.Monad.Writer

import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)

import qualified Data.Text as T
import Data.Text (Text)

-- | AST
data Value
  = VBool Bool
  | VThunk Computation
  | VVar Var
  deriving (Show, Read, Eq, Ord)
data Computation
  = CReturn Value
  | CBind Computation Var Computation
  | CForce Value
  | CIf Value Computation Computation
  | CLambda Var Computation
  | CApp Computation Value
  | CCaseLam Computation Var Computation
  | CPrint Value Computation
  deriving (Show, Read, Eq, Ord)
type Var = Text
newtype RTErr = RTErr { displayErr :: Text }
  deriving (Show, Read, Eq, Ord)

mkErr :: Text -> RTErr
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

type Values = Map Var SemVal

interpV :: Value -> Values -> Either RTErr SemVal
interpV v g = case v of
  VBool b -> return $ SemBool b
  VThunk c -> return $ SemThunk (interpC c g)
  VVar x -> maybe (Left $ RTErr ("Variable " <> x <> " not in scope.")) Right $ Map.lookup x g
interpRet :: Value -> Values -> SemComp
interpRet v g = case interpV v g of
  Left e  -> errC e
  Right v -> retC v

interpC :: Computation -> Values -> SemComp
interpC c g = case c of
  CReturn v -> interpRet v g
  CBind c x k -> bindC (interpC c g) (\v -> interpC k (Map.insert x v g))
  CForce v -> bindC (interpRet v g) $ \case
    SemThunk c -> c
    v       -> errC $ mkErr "Tried to force a non-thunk"
  CIf v tk fk -> bindC (interpRet v g) $ \case
    SemBool b -> (`interpC` g) $ if b then tk else fk
    v         -> errC $ mkErr "Tried to do an if on a non-bool"
  CLambda x m -> lamC $ \v -> interpC m (Map.insert x v g)
  CApp m v    -> bindC (interpRet v g) $ takeArg (interpC m g)
  CCaseLam r x m -> SemComp
    { returner = returner $ interpC r g
    , takeArg  = \v -> interpC m (Map.insert x v g)
    }    
  CPrint v m  -> bindC (interpRet v g) $ \case
    SemBool b -> printAll (single b) $ interpC m g
    v         -> errC $ mkErr "Tried to print a non-bool"
  where
    single b = Trace [b]

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

lamC :: (SemVal -> SemComp) -> SemComp
lamC f = SemComp
  { returner = throwError . mkErr $ "Tried to grab an arg on the stack, but there was none"
  , takeArg = f
  }

printAll :: Trace -> SemComp -> SemComp
printAll t c = SemComp
  { returner = tell t >> returner c
  , takeArg = \x -> printAll t $ takeArg c x
  }
  
-- printThen :: Bool -> SemComp -> SemComp
-- b `printThen` c = SemComp
--   { returner = tell ()
--   }

-------------------------------
-- | Examples
-------------------------------
tru = VBool True
fls = VBool False
ret = CReturn
prnt = CPrint
ex1 = ret tru
ex2 = prnt tru $ prnt fls $ ret fls
lam = CLambda

w :: Value -> Computation
w t = CLambda x (CApp (CForce t) (VThunk $ CApp (CForce (VVar x)) (VVar x)))
  where x = "x"
y = CLambda f $ CApp (w (VVar f)) (VThunk (w (VVar f)))
  where f = "f"

recC :: Var -> Computation -> Computation
recC x m = CApp y (VThunk $ CLambda x m)

constantly = CLambda "x" $ recC "constantly" $ CCaseLam (ret (VVar "x")) "_" (CForce (VVar "constantly"))

pretty c = runFiddler $ returner c
