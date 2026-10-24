module TipInterpreter where

import Control.Monad
import Data.Foldable
import Data.Functor.Identity
import Data.IORef
import Data.List
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe
import qualified Parser.Internal as TP
import qualified Parser.TipParser as TP
import Text.Read (readMaybe)

data Value = LValue CellValue | RValue RValue deriving (Show)

data RValue = Integer Int | Record (Map String RValue) | FunctionValue Function | AddressOf CellValue | Null deriving (Show)

newtype CellValue = CellValue (IORef Variable) deriving (Eq)

instance Show CellValue where
  show _ = "Cell (<ioref>)"

data Error = Error String [Frame]

renderError :: Error -> String
renderError (Error msg frames) = "error: " <> msg <> "\n" <> unlines (fmap renderFrame frames)
  where
    renderLoc :: TP.SourceLocation -> String
    renderLoc (TP.SourceLocation (line, col)) = show line <> ":" <> show col

    renderFrame :: Frame -> String
    renderFrame (Frame floc str) = "\tat " <> renderLoc floc <> ": " <> str

data Function = Function String [String] [TP.Located TP.Statement]

instance Show Function where
  show (Function name _ _) = "Function " <> name

data Frame = Frame TP.SourceLocation String

data Variable = Live RValue | Dead deriving (Show)

newtype Context = Context [Frame]

data Env = Env {context :: Context, variables :: Map String CellValue, functions :: Map String Function, input :: IO String}

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

getEnv :: Interpreter Env
getEnv = Interpreter $ \env -> pure $ Right env

getInput :: Interpreter String
getInput = do
  env <- getEnv
  str <- liftIO $ input env
  pure str

lookupAddr :: String -> Interpreter (Maybe CellValue)
lookupAddr var = do
  (Env _ m _ _) <- getEnv
  pure $ Map.lookup var m

getAddr :: String -> Interpreter CellValue
getAddr var = do
  found <- lookupAddr var
  case found of
    Just v -> pure v
    Nothing -> panic ("invalid variable " ++ var)

get :: String -> Interpreter Value
get var = Interpreter $ \env@(Env _ _ functions _) -> do
  eref <- run (getAddr var) env
  case eref of
    Right ref -> pure $ Right $ LValue ref
    Left err -> pure $ case Map.lookup var functions of
      Just fn -> Right $ RValue $ FunctionValue fn
      Nothing -> Left err

panic :: String -> Interpreter a
panic msg = Interpreter $ \(Env (Context frames) _ _ _) -> do
  pure $ Left $ Error msg frames

readValue :: Value -> Interpreter RValue
readValue (LValue (CellValue ref)) = do
  var <- liftIO $ readIORef ref
  readVar var
readValue (RValue value) = pure value

readVar :: Variable -> Interpreter RValue
readVar var = case var of
  Live l -> pure l
  Dead -> panic $ "read of dead variable"

bool :: RValue -> Interpreter a -> Interpreter a -> Interpreter a
bool (Integer 1) t _ = t
bool (Integer _) _ f = f
bool v _ _ = panic $ "invalid condition " ++ show v

evalExpr :: TP.Expression -> Interpreter Value
evalExpr (TP.Int i) = pure $ RValue $ Integer i
evalExpr (TP.Id name) = get name
evalExpr (TP.Binary op (TP.Located _ lhs) (TP.Located _ rhs)) = do
  l <- evalExpr lhs >>= readValue
  r <- evalExpr rhs >>= readValue
  RValue <$> evalBinOp op l r
  where
    evalBinOp bop (Integer a) (Integer b) = pure . Integer $ case bop of
      TP.Add -> a + b
      TP.Subtract -> a - b
      TP.Multiply -> a * b
      TP.Divide -> a `div` b
      TP.GreaterThan -> if a > b then 1 else 0
      TP.Equal -> if a == b then 1 else 0
    evalBinOp _ l r = do
      panic $ "no " ++ show op ++ " defined on " ++ show l ++ " and " ++ show r
evalExpr (TP.Unary op (TP.Located _ v)) = do
  x <- evalExpr v
  evalUnOp op x
  where
    evalUnOp :: TP.UnOp -> Value -> Interpreter Value
    evalUnOp TP.AddressOf (LValue cell) = pure $ RValue $ AddressOf cell
    evalUnOp rop other = do
      r <- readValue other
      evalUnOpRValue rop r

    evalUnOpRValue :: TP.UnOp -> RValue -> Interpreter Value
    evalUnOpRValue TP.Negate (Integer n) = pure $ RValue $ Integer (-n)
    evalUnOpRValue TP.Dereference (AddressOf ref) = pure $ LValue ref
    evalUnOpRValue _ value = panic $ "no " ++ show op ++ " defined for " ++ show value
evalExpr (TP.Alloc (TP.Located _ expr)) = do
  ev <- evalExpr expr >>= readValue
  ref <- liftIO $ newIORef (Live ev)
  pure $ RValue $ AddressOf (CellValue ref)
evalExpr (TP.Record bindings) = do
  RValue . Record . Map.fromList <$> traverse go bindings
  where
    go :: (String, TP.Located TP.Expression) -> Interpreter (String, RValue)
    go (name, TP.Located _ expr) = do
      v <- evalExpr expr >>= readValue
      pure (name, v)
evalExpr (TP.RecordAccess (TP.Located _ e) (TP.Located _ s)) = do
  v <- evalExpr e >>= readValue
  case v of
    (Record m) -> case Map.lookup s m of
      Just fv -> pure $ RValue fv
      Nothing -> do
        panic $ "no such field " ++ s ++ " in " ++ show v
    _ -> do
      panic $ "looked up field " ++ s ++ " in non-record value " ++ show v
evalExpr (TP.Call (TP.Located loc fname) args) = do
  argValues <- traverse (\(TP.Located _ x) -> evalExpr x >>= readValue) args
  functionValue <- evalExpr fname >>= readValue
  case functionValue of
    FunctionValue fn@(Function name _ _) -> withFrame (Frame loc ("call to " <> name)) $ RValue <$> evalFunction fn argValues
    _ -> do
      panic $ "invalid function value " <> show functionValue
evalExpr TP.InputLiteral = do
  _ <- liftIO $ putStr "input >"
  line <- getInput
  case readMaybe line :: Maybe Int of
    Just number -> pure $ RValue $ Integer number
    Nothing -> panic "invalid input"
evalExpr TP.NullLiteral = pure (RValue Null)

local :: (Env -> Env) -> Interpreter a -> Interpreter a
local modifyEnv interp = Interpreter $ \env -> run interp (modifyEnv env)

withVar :: RValue -> String -> Interpreter a -> Interpreter a
withVar v s interp = do
  ref <- liftIO $ newIORef (Live v)
  local (\env -> env {variables = Map.insert s (CellValue ref) (variables env)}) interp

withVars :: [String] -> Interpreter a -> Interpreter a
withVars vars interp = foldr (withVar Null) interp vars

withFrame :: Frame -> Interpreter a -> Interpreter a
withFrame fr = local (\env@(Env (Context frames) _ _ _) -> env {context = Context (fr : frames)})

evaluate :: String -> IO (Either String RValue)
evaluate prog = case TP.parseProgram prog of
  TP.ParseError _ loc e -> pure $ Left $ "Parse error at " ++ show loc ++ ": expected " ++ show e
  TP.ParseOk _ p -> evalProgram p getLine

evalProgram :: TP.TipProgram -> IO String -> IO (Either String RValue)
evalProgram ast input =
  let fns = buildProgram ast
      env = Env (Context []) Map.empty fns input
      main = Map.lookup "main" fns
   in case main of
        Nothing -> pure $ Left "missing main"
        Just m -> do
          result <- run (evalFunction m []) env
          pure $ case result of
            Right r -> Right r
            Left err -> Left $ renderError err
  where
    buildProgram :: TP.TipProgram -> Map String Function
    buildProgram (TP.TipProgram decls) = Map.fromList (fmap buildFunction decls)
      where
        buildFunction :: TP.Located TP.Function -> (String, Function)
        buildFunction (TP.Located _ (TP.Function name args stmts)) = (name, Function name args stmts)

evalStatements :: [TP.Located TP.Statement] -> Interpreter a -> (RValue -> Interpreter a) -> Interpreter a
evalStatements stmts after ret = foldr (\(TP.Located loc stm) next -> evalStatement stm next ret) after stmts

cleanupEnv :: Env -> Env -> IO ()
cleanupEnv inner outer = traverse_ (\(CellValue ref) -> writeIORef ref Dead) expiredLocals
  where
    expiredLocals = Map.differenceWith (\i o -> if o == i then Nothing else Just i) (variables inner) (variables outer)

withBlock :: (Interpreter a -> (RValue -> Interpreter a) -> Interpreter a) -> Interpreter a -> (RValue -> Interpreter a) -> Interpreter a
withBlock block next ret = Interpreter $ \env -> do
  let cleanupThenNext = finishEnv env next
  let cleanupThenRet v = finishEnv env (ret v)
  run (block cleanupThenNext cleanupThenRet) env
  where
    finishEnv :: Env -> Interpreter a -> Interpreter a
    finishEnv outer interp = Interpreter $ \inner -> do
      cleanupEnv inner outer
      run interp outer

evalFunction :: Function -> [RValue] -> Interpreter RValue
evalFunction (Function _ params stmts) args = withFreshEnv $ withBlock (\next ret -> withArguments params args (evalStatements stmts next ret)) (pure Null) pure
  where
    withArguments :: [String] -> [RValue] -> Interpreter a -> Interpreter a
    withArguments [] [] next = next
    withArguments (p : ps) (v : vs) next = withVar v p (withArguments ps vs next)
    withArguments _ _ _ = panic "wrong number of arguments to function"

    withFreshEnv :: Interpreter a -> Interpreter a
    withFreshEnv = local (\env -> env {variables = Map.empty})

output :: RValue -> IO String
output (Integer i) = pure $ show i
output (Record m) = do
  fields <- forM (Map.toList m) $ \(field, value) -> do
    v <- output value
    pure $ field ++ ": " ++ v
  pure $ intercalate ", " fields
output (AddressOf (CellValue ref)) = do
  var <- readIORef ref
  case var of
    Live v -> do
      s <- output v
      pure $ "&{" ++ s ++ "}"
    Dead -> do
      pure "<DEAD>"
output Null = pure "<null>"
output (FunctionValue fn) = pure $ show fn

evalStatement :: TP.Statement -> Interpreter a -> (RValue -> Interpreter a) -> Interpreter a
evalStatement (TP.VariableDeclaration names) next _ = withVars names next
evalStatement (TP.Output (TP.Located loc expr)) next _ = do
  v <- evalExpr expr >>= readValue
  s <- liftIO $ output v
  _ <- liftIO $ putStrLn s
  next
evalStatement (TP.If (TP.Located loc cond) (TP.Located _ tblock) lfblock) next ret = do
  v <- evalExpr cond
  v' <- readValue v
  withBlock
    ( \n r -> do
        let fb = case lfblock of
              Just (TP.Located _ fblock) -> evalStatement fblock n r
              Nothing -> n
        bool v' (evalStatement tblock n r) fb
    )
    next
    ret
evalStatement (TP.Return optExpr) _ ret = do
  val <- maybe (pure Null) (\(TP.Located _ expr) -> evalExpr expr >>= readValue) optExpr
  ret val
evalStatement (TP.Assignment (TP.Located lloc lhs) (TP.Located rloc rhs)) next _ = do
  r <- withFrame (Frame rloc "right hand side of assignment expression") $ evalExpr rhs
  rr <- readValue r
  withFrame (Frame lloc "left hand side of assignment expression") $ do
    l <- evalExpr lhs
    case l of
      LValue (CellValue var) -> do
        var' <- liftIO $ readIORef var
        case var' of
          Live _ -> liftIO $ writeIORef var (Live rr)
          Dead -> panic "write to dead memory"
      RValue v -> panic $ "cannot assign to " <> show v
  next
evalStatement (TP.Expression (TP.Located _ expr)) next _ = do
  _ <- evalExpr expr
  next
evalStatement (TP.While (TP.Located _ cond) (TP.Located _ block)) next ret = go
  where
    go = do
      c <- evalExpr cond >>= readValue
      let ifTrue =
            withBlock
              (evalStatement block)
              go
              ret
      bool c ifTrue next
evalStatement (TP.Block stms) next ret = withBlock (\n r -> evalStatements stms n r) next ret
evalStatement (TP.Error (TP.Located _ err)) _ _ = do
  v <- evalExpr err
  u <- readValue v
  msg <- liftIO $ output u
  panic msg
