import Control.Monad.State
import Control.Monad.Writer
import Keys.Abs
import Keys.Par
import System.Exit (exitFailure)

type SourceTarget = [(Hex, [Targ])]

data St = St
  { a :: SourceTarget,
    b :: SourceTarget,
    c :: SourceTarget,
    r :: SourceTarget
  }
  deriving (Show)

type Interpret = State St

interpret :: Defs -> St
interpret defs = execState (interpretDefs defs) state
  where
    state = (St {a = [], b = [], c = [], r = []})

interpretDefs :: Defs -> Interpret ()
interpretDefs (Definitions defs) = mapM_ interpretDef defs

interpretDef :: Def -> Interpret ()
interpretDef (Definition source targets) = interpretSource source targets

interpretSource :: Source -> [Targ] -> Interpret ()
interpretSource (SSource s) targs = do
  r <- gets r
  modify $ \st -> st {r = r ++ [(s, targs)]}
interpretSource (DSource (Hex s') s) targs = case s' of
  "2A" -> do
    a <- gets a
    modify $ \st -> st {a = a ++ [(s, targs)]}
  "2B" -> do
    b <- gets b
    modify $ \st -> st {b = b ++ [(s, targs)]}
  "2C" -> do
    c <- gets c
    modify $ \st -> st {c = c ++ [(s, targs)]}

type Env = St

type Output = [String]

type Generate = Writer Output

generate :: Env -> Output
generate env = snd $ runWriter (generateOutput env)

generateOutput :: Env -> Generate ()
generateOutput (St {a, b, c, r}) = do
  generateSourceTarget "a" a
  generateSourceTarget "b" b
  generateSourceTarget "c" c
  generateSourceTarget "r" r

generateSourceTarget :: String -> SourceTarget -> Generate ()
generateSourceTarget name ((Hex s, targs) : rest) = do
  generateAssignments name s targs
  generateSourceTarget name rest
generateSourceTarget name [] = pure ()

generateAssignments :: String -> String -> [Targ] -> Generate ()
generateAssignments name source targets = generateAssignments' name source targets 0
  where
    generateAssignments' :: String -> String -> [Targ] -> Integer -> Generate ()
    generateAssignments' name source (target : targets) i = do
      tell [generateAssignment name source target i]
      generateAssignments' name source targets $ i + 1
    generateAssignments' _ _ [] _ = pure ()

generateAssignment :: String -> String -> Targ -> Integer -> String
generateAssignment name source (Target device (KeyCode (Ident code))) i =
  name ++ "[" ++ source ++ "][" ++ show i ++ "] = " ++ code

main :: IO ()
main = do
  f <- readFile "./keys.def"
  parsed <- parse f
  writeFile "out.c" $ unlines $ generate $ interpret parsed
  return ()

parse :: String -> IO Defs
parse s =
  case pDefs $ myLexer s of
    Left err -> do
      putStrLn "Error"
      putStrLn err
      exitFailure
    Right prog -> do
      return prog
