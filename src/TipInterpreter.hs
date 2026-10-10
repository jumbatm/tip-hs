module TipInterpreter where

import Control.Monad
import Data.Functor.Identity
import Data.IORef
import Data.List
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe
import qualified Parser.Internal as TP
import qualified Parser.TipParser as TP

data Value = LValue CellValue | RValue RValue deriving (Show)

data RValue = Integer Int | Record (Map String RValue) | FunctionValue Function | AddressOf CellValue | Null deriving (Show)

newtype CellValue = CellValue (IORef RValue)

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

newtype Context = Context [Frame]

data Env = Env Context (Map String (IORef RValue)) (Map String Function)

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

lookupAddr :: String -> Interpreter (Maybe (IORef RValue))
lookupAddr var = do
  (Env _ m _) <- getEnv
  pure $ Map.lookup var m

getAddr :: String -> Interpreter (IORef RValue)
getAddr var = do
  found <- lookupAddr var
  case found of
    Just v -> pure v
    Nothing -> panic ("invalid variable " ++ var)

get :: String -> Interpreter Value
get var = Interpreter $ \env@(Env _ _ functions) -> do
  eref <- run (getAddr var) env
  case eref of
    Right ref -> pure $ Right $ LValue $ CellValue ref
    Left err -> pure $ case Map.lookup var functions of
      Just fn -> Right $ RValue $ FunctionValue fn
      Nothing -> Left err

put :: String -> RValue -> Interpreter ()
put var value = do
  found <- lookupAddr var
  case found of
    Nothing -> panic $ "no variable named " ++ var ++ " in scope"
    Just ref -> liftIO $ writeIORef ref value

panic :: String -> Interpreter a
panic msg = Interpreter $ \(Env (Context frames) _ _) -> do
  pure $ Left $ Error msg frames

uncell :: Value -> Interpreter RValue
uncell (LValue (CellValue ref)) = do
  liftIO $ readIORef ref
uncell (RValue value) = pure value

bool :: Value -> Interpreter a -> Interpreter a -> Interpreter a
bool (RValue (Integer 1)) t _ = t
bool (RValue (Integer _)) _ f = f
bool v _ _ = do
  v' <- uncell v
  panic $ "invalid condition " ++ show v'

evalExpr :: TP.Expression -> Interpreter Value
evalExpr (TP.Int i) = pure $ RValue $ Integer i
evalExpr (TP.Id name) = get name
evalExpr (TP.Binary op (TP.Located _ lhs) (TP.Located _ rhs)) = do
  l <- evalExpr lhs >>= uncell
  r <- evalExpr rhs >>= uncell
  RValue <$> evalBinOp op l r
  where
    evalBinOp bop (Integer a) (Integer b) = pure . Integer $ case bop of
      TP.Add -> a + b
      TP.Subtract -> a - b
      TP.Multiply -> a * b
      TP.Divide -> a `div` b
      TP.GreaterThan -> if a > b then 1 else 0
      TP.Equal -> if a == b then 1 else 0
    evalBinOp _ _ _ = panic $ "no " ++ show op ++ " defined on " ++ show lhs ++ " and " ++ show rhs
evalExpr (TP.Unary op (TP.Located _ v)) = do
  x <- evalExpr v
  evalUnOp op x
  where
    evalUnOp :: TP.UnOp -> Value -> Interpreter Value
    evalUnOp TP.AddressOf (LValue cell) = pure $ RValue $ AddressOf cell
    evalUnOp rop other = do
      r <- uncell other
      evalUnOpRValue rop r

    evalUnOpRValue :: TP.UnOp -> RValue -> Interpreter Value
    evalUnOpRValue TP.Negate (Integer n) = pure $ RValue $ Integer (-n)
    evalUnOpRValue TP.Dereference (AddressOf ref) = pure $ LValue ref
    evalUnOpRValue _ value = panic $ "no " ++ show op ++ " defined for " ++ show value
evalExpr (TP.Alloc (TP.Located _ expr)) = do
  ev <- evalExpr expr >>= uncell
  ref <- liftIO $ newIORef ev
  pure $ RValue $ AddressOf (CellValue ref)
evalExpr (TP.Record bindings) = do
  RValue . Record . Map.fromList <$> traverse go bindings
  where
    go :: (String, TP.Located TP.Expression) -> Interpreter (String, RValue)
    go (name, TP.Located _ expr) = do
      v <- evalExpr expr >>= uncell
      pure (name, v)
evalExpr (TP.RecordAccess (TP.Located _ e) (TP.Located _ s)) = do
  v <- evalExpr e >>= uncell
  case v of
    (Record m) -> case Map.lookup s m of
      Just fv -> pure $ RValue fv
      Nothing -> do
        panic $ "no such field " ++ s ++ " in " ++ show v
    _ -> do
      panic $ "looked up field " ++ s ++ " in non-record value " ++ show v
evalExpr (TP.Call (TP.Located loc fname) args) = do
  argValues <- traverse (\(TP.Located _ x) -> evalExpr x >>= uncell) args
  functionValue <- evalExpr fname >>= uncell
  case functionValue of
    FunctionValue fn@(Function name _ _) -> withFrame (Frame loc ("call to " <> name)) $ RValue <$> evalFunction fn argValues
    _ -> do
      panic $ "invalid function value " <> show functionValue

withVar :: RValue -> String -> Interpreter a -> Interpreter a
withVar v s interp = Interpreter $ \(Env c m f) -> do
  ref <- newIORef v
  let m' = Map.insert s ref m
  run interp (Env c m' f)

withVars :: [String] -> Interpreter a -> Interpreter a
withVars vars interp = foldr (withVar Null) interp vars

withFrame :: Frame -> Interpreter a -> Interpreter a
withFrame fr interp = Interpreter $ \(Env (Context c) m f) -> do
  let newContext = Context (fr : c)
  run interp (Env newContext m f)

evaluate :: String -> IO (Either String RValue)
evaluate prog = case runIdentity $ TP.runParser TP.tipProgramP prog of
  TP.ParseError _ loc e -> pure $ Left $ "Parse error at " ++ show loc ++ ": expected " ++ show e
  TP.ParseOk _ (p, _) -> evalProgram p

evalProgram :: TP.TipProgram -> IO (Either String RValue)
evalProgram ast =
  let fns = buildProgram ast
      env = Env (Context []) Map.empty fns
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
evalStatements stmts after ret = foldr (\(TP.Located loc stm) next -> evalStatement stm loc next ret) after stmts

withBlock :: (Interpreter a -> Interpreter a) -> Interpreter a -> Interpreter a
withBlock block next = Interpreter $ \env -> do
  let afterBlock = withEnv env next
  run (block afterBlock) env
  where
    -- Run an interpreter with a specific environment rather than the outer one.
    withEnv :: Env -> Interpreter a -> Interpreter a
    withEnv env interp = Interpreter $ \_ -> run interp env

evalFunction :: Function -> [RValue] -> Interpreter RValue
evalFunction (Function _ params stmts) args = withFreshEnv $ do
  withArguments params args (evalStatements stmts (pure Null) pure)
  where
    withArguments :: [String] -> [RValue] -> Interpreter a -> Interpreter a
    withArguments [] [] next = next
    withArguments (p : ps) (v : vs) next = withVar v p (withArguments ps vs next)
    withArguments _ _ _ = panic "wrong number of arguments to function"

    withFreshEnv :: Interpreter a -> Interpreter a
    withFreshEnv interp = Interpreter $ \(Env c _ f) -> do
      run interp (Env c Map.empty f)

output :: RValue -> IO String
output (Integer i) = pure $ show i
output (Record m) = do
  fields <- forM (Map.toList m) $ \(field, value) -> do
    v <- output value
    pure $ field ++ ": " ++ v
  pure $ intercalate ", " fields
output (AddressOf (CellValue ref)) = do
  v <- readIORef ref
  s <- output v
  pure $ "&{" ++ s ++ "}"
output Null = pure "<null>"
output (FunctionValue fn) = pure $ show fn

evalStatement :: TP.Statement -> TP.SourceLocation -> Interpreter a -> (RValue -> Interpreter a) -> Interpreter a
evalStatement (TP.VariableDeclaration names) pos next _ = withVars names next
evalStatement (TP.Output (TP.Located loc expr)) pos next _ = do
  v <- evalExpr expr >>= uncell
  s <- liftIO $ output v
  _ <- liftIO $ putStrLn s
  next
evalStatement (TP.If (TP.Located loc cond) tblock fblock) pos next ret = do
  v <- evalExpr cond
  bool v (evalStatements tblock next ret) (evalStatements (fromMaybe [] fblock) next ret)
evalStatement (TP.Return optExpr) pos _ ret = do
  val <- maybe (pure Null) (\(TP.Located _ expr) -> evalExpr expr >>= uncell) optExpr
  ret val
evalStatement (TP.Assignment (TP.Located lloc lhs) (TP.Located rloc rhs)) pos next _ = do
  r <- withFrame (Frame rloc "right hand side of assignment expression") $ evalExpr rhs
  rr <- uncell r
  withFrame (Frame lloc "left hand side of assignment expression") $ do
    case lhs of
      (TP.Id name) -> do
        put name rr
      _ -> do
        ll <- evalExpr lhs
        case ll of
          LValue (CellValue c) -> liftIO $ writeIORef c rr
          _ -> panic $ "cannot assign to " <> show ll
  next
evalStatement (TP.Expression (TP.Located _ expr)) pos next _ = do
  _ <- evalExpr expr
  next
evalStatement (TP.While (TP.Located _ cond) block) pos next ret = go
  where
    go = do
      c <- evalExpr cond
      let runBlock = do
            evalStatements block go ret
      bool c runBlock next
evalStatement (TP.Block stms) pos next ret = withBlock (\n -> evalStatements stms n ret) next
evalStatement (TP.Error (TP.Located _ err)) pos _ _ = do
  v <- evalExpr err
  u <- uncell v
  msg <- liftIO $ output u
  panic msg
