{-# LANGUAGE ForeignFunctionInterface, EmptyDataDecls #-}

import System.Environment
import Text.ParserCombinators.Parsec hiding ( spaces )
import Monad
import Control.Monad.Error
import IO hiding (try)
import Data.IORef


import Foreign.C.Types
import Foreign.C.String
import Foreign.Ptr
import Network
import System.IO
import Data.IORef

foreign import ccall safe "openWindow" openWindow
         :: IO CInt

data ViewController_struct
type ViewController = Ptr ViewController_struct


type RunStr = ViewController -> CString -> IO ()
foreign import ccall safe "wrapper" wrapFuncInvoke :: RunStr -> IO (FunPtr RunStr)
foreign import ccall safe "setLispEval" setLispEval :: ViewController -> FunPtr RunStr -> IO ()

foreign import ccall safe "addToResult" addToResult :: ViewController -> CString -> IO ()

openLogger :: IO Handle
openLogger = return stderr


type ViewDidLoad = ViewController -> IO ()
foreign import ccall safe "wrapper" mkViewDidLoad :: ViewDidLoad -> IO (FunPtr ViewDidLoad)
foreign import ccall safe "setViewDidLoad" setViewDidLoad :: FunPtr ViewDidLoad -> IO ()

          
main :: IO ()
main = do
     log <- openLogger
     hPutStrLn log "Starting the Lisp system"

     env <- primitiveBindings -- the Lisp environment
     
     -- execute a line of List and call the callback with the result
     runALine <- wrapFuncInvoke $ \vc line -> do
                                              toEval <- peekCString line
                                              res <- evalString env toEval
                                              back <- withCString res $ \back -> addToResult vc back
                                              return ()

     -- the initial callback that sets up the rest
     vdl <- mkViewDidLoad $ \vc -> do
        hPutStrLn log $ show vc++" viewDidLoad called back"
        setLispEval vc runALine
        return ()

     setViewDidLoad vdl
     openWindow
     hPutStrLn log "Shutting down"
     return ()

symbol :: Parser Char
symbol = oneOf "!$%&|*+-/:<=?>@^_~#"

readOrThrow parser input = case parse parser "lisp" input of 
            Left err -> throwError $ Parser err
            Right val -> return val

readExpr = readOrThrow parseExpr
readExprList = readOrThrow (endBy parseExpr spaces)

--readExpr :: String -> ThrowsError LispVal
--readExpr input = case parse parseExpr "lisp" input of
--    Left err -> throwError $ Parser err
--    Right val -> return val

spaces :: Parser ()
spaces = skipMany1 space

data LispVal = Atom String
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | Number Integer
             | String String
             | Bool Bool
             | PrimitiveFunc ([LispVal] -> ThrowsError LispVal)
             | Func {params :: [String], vararg :: (Maybe String),
                     body :: [LispVal], closure :: Env}
             | IOFunc ([LispVal] -> IOThrowsError LispVal)
             | Port Handle

parseString :: Parser LispVal
parseString = do char '"'
                 x <- many (noneOf "\"")
                 char '"'
                 return $ String x

parseAtom :: Parser LispVal
parseAtom = do first <- letter <|> symbol
               rest <- many (letter <|> digit <|> symbol)
               let atom = [first] ++ rest
               return $ case atom of
                          "#t" -> Bool True
                          "#f" -> Bool False
                          otherwise -> Atom atom

parseNumber :: Parser LispVal
parseNumber = liftM (Number . read) $ many1 digit

-- parseList :: Parser LispVal
parseList = liftM List $ sepBy parseExpr spaces

parseDottedList = do
  head <- endBy parseExpr spaces
  tail <- char '.' >> spaces >> parseExpr
  return $ DottedList head tail

-- moo
parseQuoted = do
  char '\''
  x <- parseExpr
  return $ List [Atom "quote", x]

parseExpr :: Parser LispVal
parseExpr = parseAtom
         <|> parseString
         <|> parseNumber
         <|> parseQuoted
         <|> do char '('
                x <- (try parseList) <|> parseDottedList
                char ')'
                return x

showVal (String contents) = "\"" ++ contents ++ "\""
showVal (Atom name) = name
showVal (Number number) = show number
showVal (Bool True) = "#t"
showVal (Bool False) = "#f"
showVal (List contents) = "(" ++ unwordsList contents ++ ")"
showVal (DottedList head tail) = "(" ++ unwordsList head ++ 
            " . " ++ showVal tail ++ ")"
showVal (PrimitiveFunc _) = "<primative>"
showVal (Func {params = args, vararg = varargs, body = body, closure = env}) =
        "(lambda (" ++ unwords (map show args) ++
           (case varargs of
                 Nothing -> ""
                 Just arg -> " . " ++ arg) ++ ") ...)"
showVal (Port _) = "<IO port>"
showVal (IOFunc _) = "<IO Primitive>"

unwordsList = unwords . map showVal

instance Show LispVal where show = showVal

eval :: Env -> LispVal -> IOThrowsError LispVal
eval env val@(String _) = return val
eval env val@(Number _) = return val
eval env val@(Bool _) = return val
eval env (Atom id) = getVar env id
eval env (List [Atom "load", String filename]) =
     load filename >>= liftM last . mapM (eval env)
eval env (List [Atom "quote", val]) = return val
eval env (List [Atom "if", pred, conseq, alt]) =
     do result <- eval env pred
        case result of
          Bool False -> eval env alt
          _ -> eval env conseq
eval env (List [Atom "set!", Atom var, form]) = eval env form >>= setVar env var
eval env (List [Atom "define", Atom var, form]) = eval env form >>= defineVar env var
eval env (List (Atom "define" : List (Atom var : params) : body)) =
     makeNormalFunc env params body >>= defineVar env var
eval env (List (Atom "define" : DottedList(Atom var : params) varargs : body)) =
     makeVarargs varargs env params body >>= defineVar env var
eval env (List (Atom "lambda" : List params : body)) =
     makeNormalFunc env params body
eval env (List (Atom "lambda" : DottedList params varargs : body)) =
     makeVarargs varargs env params body
eval env (List (function : args)) = do
     func <- eval env function
     argVals <- mapM (eval env) args
     apply func argVals
-- eval env (List [Atom "define", Atom var, form]) = eval env form >>= defineVar env var
-- eval env (List (Atom func : args)) = mapM (eval env) args >>= liftThrows . apply func
eval env badForm = throwError $ BadSpecialForm "Unrecognized special form" badForm

--apply func args = maybe (throwError $ NotFunction 
--      "Unrecognized primative function args" func)
--      ($ args) (lookup func primitives)
apply :: LispVal -> [LispVal] -> IOThrowsError LispVal
apply (PrimitiveFunc func) args = liftThrows $ func args
apply (Func params varargs body closure) args =
      if num params /= num args && varargs == Nothing
         then throwError $ NumArgs (num params) args
         else (liftIO $ bindVars closure $ zip params args) >>= bindVarArgs varargs >>= evalBody
      where remainingArgs = drop (length params) args
            num = toInteger . length
            evalBody env = liftM last $ mapM (eval env) body
            bindVarArgs arg env = case arg of
              Just argName -> liftIO $ bindVars env [(argName, List $ remainingArgs)]
              Nothing -> return env

primitives = [("+", numericBinop (+)),
              ("-", numericBinop (-)),
              ("*", numericBinop (*)),
--              ("/", numericBinop (/)),
              ("mod", numericBinop mod),
              ("=", numBoolBinop (==)),
              ("<", numBoolBinop (<)),
              (">", numBoolBinop (>)),
              ("/=", numBoolBinop (/=)),
              (">=", numBoolBinop (>=)),
              ("<=", numBoolBinop (<=)),
              ("&&", boolBoolBinop (&&)),
              ("||", boolBoolBinop (||)),
              ("string=?", strBoolBinop (==)),
              ("string?", strBoolBinop (>)),
              ("string<=?", strBoolBinop (<=)),
              ("string>=?", strBoolBinop (>=)),
              ("quotient", numericBinop quot),
              ("car", car),
              ("cdr", cdr),
              ("cons", cons),
              ("eq?", eqv),
              ("eqv?", eqv),
              ("equal?", equal),
              ("remainder", numericBinop rem)]


boolBinop :: (LispVal -> ThrowsError a) -> (a -> a -> Bool) -> [LispVal] -> ThrowsError LispVal
boolBinop unpacker op args = case args of 
                               [a, b] -> do left <- unpacker a
                                            right <- unpacker b
                                            return $ Bool $ left `op` right
                               _ -> throwError $ NumArgs 2 args
 
numBoolBinop = boolBinop unpackNum
strBoolBinop = boolBinop unpackStr
boolBoolBinop = boolBinop unpackBool

numericBinop :: (Integer -> Integer -> Integer) -> [LispVal] -> ThrowsError LispVal
numericBinop op singleVal@[_] = throwError $ NumArgs 2 singleVal
numericBinop op params = mapM unpackNum params >>= return . 
             Number . foldl1 op

unpackStr (String s) = return s
unpackStr (Number n) = return $ show n
unpackStr (Bool b) = return $ show b
unpackStr notString = throwError $ TypeMismatch "string" notString

unpackBool (Bool b) = return b
unpackBool notBool = throwError $ TypeMismatch "boolean" notBool


-- unpackNum :: LispVal -> ThrowsError Integer
unpackNum (Number n) = return n
unpackNum (String s) = let parsed = reads s in 
                           case parsed of
                             [(n,"")] -> return n
                             _ -> throwError $ TypeMismatch "number" $ String s
unpackNum (List [n]) = unpackNum n
unpackNum notNum = throwError $ TypeMismatch "number" notNum

car :: [LispVal] -> ThrowsError LispVal
car [List (x : xs)] = return x
car [DottedList (x : xs) _] = return x
car [badArg] = throwError $ TypeMismatch "pair" badArg
car badArgList = throwError $ NumArgs 1 badArgList

cdr [List (x : xs)] = return $ List xs
cdr [DottedList (_ : xs) x] = return  $ DottedList xs x
cdr [DottedList [_] x] = return x
cdr [badArg] = throwError $ TypeMismatch "pair" badArg
cdr badArgList = throwError $ NumArgs 1 badArgList

cons :: [LispVal] -> ThrowsError LispVal
cons [x1, List []] = return $ List [x1]
cons [x, List xs] = return $ List $ [x] ++ xs
cons [x, DottedList xs xlast] = return $ DottedList ([x] ++ xs) xlast
cons [x1, x2] = return $ DottedList [x1] x2
cons badArgList = throwError $ NumArgs 2 badArgList

eqv :: [LispVal] -> ThrowsError LispVal
eqv [(Bool arg1), (Bool arg2)] = return $ Bool $ arg1 == arg2
eqv [(Number arg1), (Number arg2)] = return $ Bool $ arg1 == arg2
eqv [(String arg1), (String arg2)] = return $ Bool $ arg1 == arg2
eqv [(Atom arg1), (Atom arg2)] = return $ Bool $ arg1 == arg2
eqv [(DottedList xs x), (DottedList ys y)] = eqv [List $ xs ++ [x], List $ ys ++ [y]]
eqv [(List arg1), (List arg2)] = return $ Bool $
                                 (length arg1 == length arg2) &&
                                 (and $ map eqvPair $ zip arg1 arg2)
                             where eqvPair (x1, x2) = case eqv [x1, x2] of
                                                        Left err -> False
                                                        Right (Bool val) -> val
eqv [_, _] = return $ Bool False
eqv badArgList = throwError $ NumArgs 2 badArgList

data LispError = NumArgs Integer [LispVal]
               | TypeMismatch String LispVal
               | Parser ParseError
               | BadSpecialForm String LispVal
               | NotFunction String String
               | UnboundVar String String
               | Default String

showError (UnboundVar message varname) = 
         message ++ ": " ++ varname
showError (BadSpecialForm message form) =
         message ++ ": " ++ show form
showError (NotFunction message func) =
         message ++ ": " ++ show func
showError (NumArgs expected found) =
         "Expected " ++ show expected ++
           " args: found values "++
           unwordsList found
showError (TypeMismatch expected found) =
          "Invalid type: expected " ++ expected ++
            ", found " ++ show found
showError (Parser parseErr) = "Parse error at " ++ show parseErr

instance Show LispError where show = showError

instance Error LispError where
   noMsg = Default "An error has occurred"
   strMsg = Default

type ThrowsError = Either LispError

trapError action = catchError action (return . show)

extractValue (Right val) = val

data Unpacker = forall a . Eq a => AnyUnpacker (LispVal -> ThrowsError a)

unpackEquals :: LispVal -> LispVal -> Unpacker -> ThrowsError Bool
unpackEquals arg1 arg2 (AnyUnpacker unpacker) =
             do unpacked1 <- unpacker arg1
                unpacked2 <- unpacker arg2
                return $ unpacked1 == unpacked2
         `catchError` (const $ return False)

equal :: [LispVal] -> ThrowsError LispVal
equal [arg1, arg2] = do
      primitiveEquals <- liftM or $ mapM (unpackEquals arg1 arg2)
                        [AnyUnpacker unpackNum, AnyUnpacker unpackStr, AnyUnpacker unpackBool]
      eqvEquals <- eqv [arg1, arg2]
      return $ Bool $ (primitiveEquals || let (Bool x) = eqvEquals in x)

equal badArgList = throwError $ NumArgs 2 badArgList

flushStr :: String -> IO ()
flushStr str = putStr str >> hFlush stdout

readPrompt prompt = flushStr prompt >> getLine

until_ :: Monad m => (a -> Bool) -> m a -> (a -> m ()) -> m ()
until_ pred prompt action = do
       result <- prompt
       if pred result
         then return ()
         else action result >> until_ pred prompt action

type Env = IORef [(String, IORef LispVal)]

nullEnv = newIORef []

type IOThrowsError = ErrorT LispError IO

-- liftThrows :: ThrowsError a -> IOThrowsError a
liftThrows (Left err) = throwError err
liftThrows (Right val) = return val

runIOThrows action = runErrorT (trapError action) >>= return . extractValue

isBound envRef var = readIORef envRef >>= return . maybe False (const True) . lookup var

getVar envRef var = do env <- liftIO $ readIORef envRef
                       maybe (throwError $ UnboundVar "Getting an unbound variable" var)
                             (liftIO . readIORef)
                             (lookup var env)

setVar envRef var value = do env <- liftIO $ readIORef envRef
                             maybe (throwError $ UnboundVar "Setting an unbound variable" var)
                                   (liftIO . (flip writeIORef value))
                                   (lookup var env)
                             return value

applyProc [func, List args] = apply func args
applyProc (func : args) = apply func args


-- defineVar :: Env -> String -> LispVal -> IOThrowsError LispVal
defineVar envRef var value = do
          alreadyDefined <- liftIO $ isBound envRef var
          if alreadyDefined
             then setVar envRef var value >> return value
             else liftIO $ do
                valueRef <- newIORef value
                env <- readIORef envRef
                writeIORef envRef ((var, valueRef) : env)
                return value

bindVars envRef bindings = readIORef envRef >>= extendEnv bindings >>= newIORef
         where extendEnv bindings env = liftM (++ env) (mapM addBinding bindings)
               addBinding (var, value) = do ref <- newIORef value
                                            return (var, ref)

evalAndPrint env expr = evalString env expr >>= putStrLn

evalString :: Env -> String -> IO String
evalString env expr = runIOThrows $ liftM show $ (liftThrows $ readExpr expr) >>= eval env

runOne :: [String] -> IO ()
runOne args = do
       env <- primitiveBindings >>= flip bindVars [("args", List $ map String $ drop 1 args)]
       output <- (runIOThrows $ liftM show $ eval env (List [Atom "load", String (args !! 0)]))
       hPutStrLn stderr output
--          >>= putStrLn stderr

runRepl = primitiveBindings >>= until_ (== "quit") (readPrompt "Lisp>>> ") . evalAndPrint

primitiveBindings :: IO Env
primitiveBindings = nullEnv >>= (flip bindVars $ map (makeFunc IOFunc) ioPrimitives
                                               ++ map (makeFunc PrimitiveFunc) primitives)
          where makeFunc constructor (var, func) = (var, constructor func) 

makeFunc varargs env params body = return $ Func (map showVal params) varargs body env

makeNormalFunc = makeFunc Nothing

makeVarargs = makeFunc . Just . showVal

ioPrimitives = [("apply", applyProc),
                ("open-input-file", makePort ReadMode),
                ("open-output-file", makePort WriteMode),
                ("close-input-port", closePort),
                ("close-output-port", closePort),
                ("read", readProc),
                ("write", writeProc),
                ("read-contents", readContents),
                ("read-all", readAll)]

makePort mode [String filename] = liftM Port $ liftIO $ openFile filename mode


closePort [Port port] =  liftIO $ hClose port >> (return $ Bool True)
closePort _ = return $ Bool False

readProc [] = readProc [Port stdin]
readProc [Port port] = (liftIO $ hGetLine port) >>= liftThrows . readExpr

writeProc [obj] = writeProc [obj, Port stdout]
writeProc [obj, Port port] = liftIO $ hPrint port obj >> (return $ Bool True)

readContents [String filename] = liftM String $ liftIO $ readFile filename

load filename = (liftIO $ readFile filename) >>= liftThrows . readExprList

readAll [String filename] = liftM List $ load filename