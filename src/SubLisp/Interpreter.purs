module SubLisp.Interpreter where

import Prelude
import Control.Monad.Error.Class (class MonadError, class MonadThrow, throwError)
import Control.Monad.Except (ExceptT, runExceptT)
import Control.Monad.Rec.Class (class MonadRec)
import Control.Monad.State (class MonadState, StateT, evalStateT, get, gets, modify_)
import Data.Either (Either(..))
import Data.Identity (Identity)
import Data.List (List(..), foldr, last, length, zip, (:))
import Data.Map (Map)
import Data.Map as M
import Data.Maybe (Maybe(..), maybe)
import Data.Newtype (class Newtype, unwrap)
import Data.Traversable (sequence, traverse)
import SubLisp.Const (Const(..))
import SubLisp.Term (Term(..))
import SubLisp.Value (Value(..))
import Text.Parsing.Parser (ParseError)

data InterpretError
  = SyntaxError ParseError
  | UnboundVariable String
  | TooFewArguments Term
  | TooManyArguments Term
  | InvalidApplication Term
  | InvalidFunctionDefinition Term
  | InvalidDefinition Term
  | InvalidArguments Term

derive instance eqInterpretError :: Eq InterpretError

instance showInterpretError :: Show InterpretError where
  show (SyntaxError err) = "syntax error: " <> show err
  show (UnboundVariable v) = "Unbound variable " <> show v
  show (TooFewArguments t) = "Too few arguments: " <> show t
  show (TooManyArguments t) = "Too many arguments: " <> show t
  show (InvalidApplication t) = "Invalid application: " <> show t
  show (InvalidFunctionDefinition t) = "Invalid function definition: " <> show t
  show (InvalidDefinition t) = "Invalid definition: " <> show t
  show (InvalidArguments t) = "Invalid arguments: " <> show t

type Env
  = Map String Value

data InterpretState
  = InterpretState Env

type Interpreter
  = InterpreterT Identity

newtype InterpreterT m a
  = Interpreter (ExceptT InterpretError (StateT InterpretState m) a)

derive instance newtypeInterpreterT :: Newtype (InterpreterT m a) _

runInterpreterT :: forall m a. Monad m => Env -> InterpreterT m a -> m (Either InterpretError a)
runInterpreterT env i = evalStateT (runExceptT (unwrap i)) (InterpretState env)

runInterpreter :: forall a. Env -> Interpreter a -> Either InterpretError a
runInterpreter env = unwrap <<< runInterpreterT env

derive newtype instance functorInterpreterT :: Functor m => Functor (InterpreterT m)

derive newtype instance applyInterpreterT :: Monad m => Apply (InterpreterT m)

derive newtype instance applicativeInterpreterT :: Monad m => Applicative (InterpreterT m)

derive newtype instance bindInterpreterT :: Monad m => Bind (InterpreterT m)

derive newtype instance monadInterpreterT :: Monad m => Monad (InterpreterT m)

derive newtype instance monadRecInterpreterT :: MonadRec m => MonadRec (InterpreterT m)

derive newtype instance monadStateInterpreterT :: Monad m => MonadState InterpretState (InterpreterT m)

derive newtype instance monadThrowInterpreterT :: Monad m => MonadThrow InterpretError (InterpreterT m)

derive newtype instance monadErrorInterpreterT :: Monad m => MonadError InterpretError (InterpreterT m)

interpret :: Term -> Interpreter Value
interpret (TmConst c) = pure $ VConst c

interpret (TmQuoted t@(TmQuoted _)) = pure $ VQuoted t

interpret (TmQuoted (TmVar v)) = pure $ VSymbol v

interpret (TmQuoted (TmPair h t)) = foldr VPair (VConst CNil) <$> sequence ((interpret h) : (map interpret t))

interpret (TmQuoted t) = interpret t

interpret (TmVar v) =
  gets (\(InterpretState env) -> M.lookup v env)
    >>= maybe (throwError $ UnboundVariable v) pure

interpret pair@(TmPair head tail) =
  interpret head
    >>= case _ of
        -- Special forms
        VConst CIf -> case tail of
          Cons cond (Cons _then (Cons _else Nil)) ->
            interpret cond
              >>= case _ of
                  VConst CNil -> interpret _else
                  _ -> interpret _then
          _ -> throwArgsNumError 3
        VConst CQuote -> case tail of
          Cons t Nil -> pure $ VQuoted t
          _ -> throwArgsNumError 1
        VConst CFun -> case tail of
          Cons args body -> case args of
            TmConst CNil -> pure $ VFun Nil body
            TmPair x xs -> case traverse maybeTmVar (x : xs) of
              Nothing -> throwError $ InvalidFunctionDefinition pair
              Just vs -> pure $ VFun vs body
            _ -> throwError $ InvalidFunctionDefinition pair
          _ -> throwError $ TooFewArguments pair
        VConst CDef -> case tail of
          Cons (TmVar var) (Cons value Nil) ->
            interpret value
              >>= \val -> do
                  modify_ \(InterpretState env) -> InterpretState $ M.insert var val env
                  pure val
          _ -> throwError $ InvalidDefinition pair
        -- Built-in functions
        VConst CAtom -> case tail of
          Cons t Nil ->
            interpret t
              >>= case _ of
                  VPair _ _ -> pure $ VConst CNil
                  _ -> pure $ VConst CT
          _ -> throwArgsNumError 1
        VConst CEq -> case tail of
          Cons t1 (Cons t2 Nil) -> VConst <<< (if _ then CT else CNil) <$> (eq <$> interpret t1 <*> interpret t2)
          _ -> throwArgsNumError 2
        VConst CHead -> case tail of
          Cons tm Nil ->
            interpret tm
              >>= case _ of
                  VPair h _ -> pure h
                  _ -> throwError $ InvalidArguments pair
          _ -> throwArgsNumError 1
        VConst CTail -> case tail of
          Cons tm Nil ->
            interpret tm
              >>= case _ of
                  VPair _ t -> pure t
                  _ -> throwError $ InvalidArguments pair
          _ -> throwArgsNumError 1
        VConst CCons -> case tail of
          Cons h (Cons t Nil) -> VPair <$> interpret h <*> interpret t
          _ -> throwArgsNumError 2
        -- User-defined functions
        VSymbol s ->
          interpret (TmVar s)
            >>= case _ of
                VFun args body -> callFun args body
                _ -> throwError $ InvalidApplication pair
        VFun args body -> callFun args body
        _ -> throwError $ InvalidApplication pair
  where
  throwArgsNumError n = throwError $ if length tail < n then TooFewArguments pair else TooManyArguments pair

  maybeTmVar = case _ of
    TmVar v -> Just v
    _ -> Nothing

  callFun args body = case compare (length tail) (length args) of
    LT -> throwError $ TooFewArguments pair
    GT -> throwError $ TooManyArguments pair
    EQ -> do
      InterpretState env <- get
      newEnv <- M.union env <<< M.fromFoldable <$> zip args <$> traverse interpret tail
      case runInterpreter newEnv (traverse interpret body) of
        Left err -> throwError err
        Right vs -> maybe (pure $ VConst CNil) pure (last vs)
