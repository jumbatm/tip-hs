module Interpreter where

import Control.Monad
import Data.IORef
import Data.List
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Parser.TipParser as TP

data Value = Integer Int | Record (Map String Value) | Cell CellValue | Null deriving (Show)

newtype CellValue = CellValue (IORef Value)

instance Show CellValue where
  show _ = "Cell (<ioref>)"

type Error = String

data Function = Function [String] [TP.Located TP.Statement]

data Env = Env (Map String (IORef Value)) (Map String Function)

newEnv :: Env
newEnv = Env Map.empty Map.empty

newtype Interpreter a = Interpreter {run :: Env -> IO (Either Error a)}

instance Functor Interpreter where
  fmap f (Interpreter fv) = Interpreter $ \env -> fmap (fmap f) (fv env)

instance Applicative Interpreter where
  pure v = Interpreter $ \_ -> pure $ Right v
  (Interpreter interpf) <*> (Interpreter interpv) = Interpreter $ \env -> do
    ef <- interpf env
    case ef of
      Left err -> pure $ Left err
      Right f -> fmap f <$> interpv env

instance Monad Interpreter where
  return = pure

  (Interpreter iv) >>= fm = Interpreter $ \env -> do
    ev <- iv env
    case ev of
      Left err -> pure . Left $ err
      Right v -> run (fm v) env

liftIO :: IO a -> Interpreter a
liftIO action = Interpreter $ \_ -> Right <$> action

getAddr :: String -> Interpreter (IORef Value)
getAddr var = Interpreter $ \(Env m _) -> pure $ case Map.lookup var m of
  Nothing -> Left $ "invalid variable " ++ var
  Just v -> Right v

get :: String -> Interpreter Value
get var = Interpreter $ \env -> do
  eref <- run (getAddr var) env
  case eref of
    Left err -> pure $ Left err
    Right ref -> Right <$> readIORef ref

put :: String -> Value -> Interpreter ()
put var value = Interpreter $ \env@(Env m _) -> do
  case Map.lookup var m of
    Nothing -> run (panic $ "no variable named " ++ var ++ " in scope") env
    Just ref -> Right <$> writeIORef ref value

panic :: String -> Interpreter a
panic = Interpreter . const . pure . Left

uncell :: Value -> Interpreter Value
uncell (Cell (CellValue ref)) = do
  liftIO $ readIORef ref
uncell value = pure value

-- TODO: Alas, again, the allure of using recursion scheme. Must... resist...
evalExpr :: TP.Expression -> Interpreter Value
evalExpr (TP.Int i) = pure $ Integer i
evalExpr (TP.Id name) = get name
evalExpr (TP.Binary op lhs rhs) = do
  l <- evalExpr lhs
  r <- evalExpr rhs
  evalBinOp op l r
  where
    evalBinOp bop (Integer a) (Integer b) = pure . Integer $ case bop of
      TP.Add -> a + b
      TP.Subtract -> a - b
      TP.Multiply -> a * b
      TP.Divide -> a `div` b
      TP.GreaterThan -> if a > b then 1 else 0
      TP.Equal -> if a == b then 1 else 0
    evalBinOp _ _ _ = panic $ "no " ++ show op ++ " defined on " ++ show lhs ++ " and " ++ show rhs
evalExpr (TP.Unary TP.Dereference (TP.Id name)) = get name
evalExpr (TP.Unary TP.AddressOf (TP.Id name)) = do
  addr <- getAddr name
  pure $ Cell (CellValue addr)
evalExpr (TP.Unary op v) = do
  x <- evalExpr v
  evalUnOp op x
  where
    evalUnOp TP.Negate (Integer n) = pure $ Integer (-n)
    evalUnOp _ _ = panic $ "no " ++ show op ++ " defined for " ++ show v
evalExpr (TP.Alloc expr) = do
  ev <- evalExpr expr
  ref <- liftIO $ newIORef ev
  pure $ Cell $ CellValue ref
evalExpr (TP.Record bindings) = do
  Record . Map.fromList <$> traverse go bindings
  where
    go (name, expr) = do
      v <- evalExpr expr
      pure (name, v)
evalExpr (TP.RecordAccess e s) = do
  v <- evalExpr e
  case v of
    (Record m) -> case Map.lookup s m of
      Just fv -> pure fv
      Nothing -> do
        cv <- uncell v
        panic $ "no such field " ++ s ++ " in " ++ show cv
    _ -> do
      cv <- uncell v
      panic $ "looked up field " ++ s ++ " in non-record value " ++ show cv
evalExpr (TP.Call fname args) = undefined

withVar :: String -> Interpreter a -> Interpreter a
withVar s interp = Interpreter $ \(Env m f) -> do
  ref <- newIORef Null
  let m' = Map.insert s ref m
  run interp (Env m' f)

withVars :: [String] -> Interpreter a -> Interpreter a
withVars vars interp = foldr withVar interp vars

evalStatements :: Interpreter a -> [TP.Statement] -> Interpreter a
evalStatements = foldr evalStatement

evalStatement :: TP.Statement -> Interpreter a -> Interpreter a
evalStatement (TP.VariableDeclaration names) next = withVars names next
evalStatement (TP.Output expr) next = do
  v <- evalExpr expr
  s <- liftIO $ output v
  _ <- liftIO $ putStrLn s
  next
  where
    output :: Value -> IO String
    output (Integer i) = pure $ show i
    output (Record m) = do
      fields <- forM (Map.toList m) $ \(field, value) -> do
        v <- output value
        pure $ field ++ ": " ++ v
      pure $ intercalate ", " fields
    output (Cell (CellValue ref)) = do
      v <- readIORef ref
      s <- output v
      pure $ "&{" ++ s ++ "}"
    output Null = pure "<null>"
evalStatement (TP.If cond tblock fblock) next = undefined
evalStatement (TP.Return expr) _ = do
  val <- maybe (pure Null) evalExpr expr
  pure val
evalStatement (TP.Assignment lhs rhs) next = undefined
evalStatement (TP.Expression expr) next = undefined
evalStatement (TP.While cond block) next = undefined
evalStatement (TP.Block stms) next = undefined
evalStatement (TP.Error err) next = undefined
