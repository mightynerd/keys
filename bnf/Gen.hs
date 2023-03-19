import Control.Monad.State
import Control.Monad.Writer
import Keys.Abs
import Keys.Par
import System.Exit (exitFailure)

type SourceTarget = [(Hex, [Targ])]

data St = St
  { a :: SourceTarget,
    aMax :: Int,
    b :: SourceTarget,
    bMax :: Int,
    c :: SourceTarget,
    cMax :: Int,
    r :: SourceTarget,
    rMax :: Int
  }
  deriving (Show)

type Interpret = State St

interpret :: Defs -> St
interpret defs = execState (interpretDefs defs) state
  where
    state = (St {a = [], b = [], c = [], r = [], aMax = 1, bMax = 1, cMax = 1, rMax = 1})

interpretDefs :: Defs -> Interpret ()
interpretDefs (Definitions defs) = mapM_ interpretDef defs

interpretDef :: Def -> Interpret ()
interpretDef (Definition source targets) = interpretSource source targets

interpretSource :: Source -> [Targ] -> Interpret ()
interpretSource (SSource s) targs = do
  r <- gets r
  rMax <- gets rMax
  modify $ \st -> st {r = r ++ [(s, targs)], rMax = max rMax (length targs)}
interpretSource (DSource (Hex s') s) targs = case s' of
  "2A" -> do
    a <- gets a
    aMax <- gets aMax
    modify $ \st -> st {a = a ++ [(s, targs)], aMax = max aMax (length targs)}
  "2B" -> do
    b <- gets b
    bMax <- gets bMax
    modify $ \st -> st {b = b ++ [(s, targs)], bMax = max bMax (length targs)}
  "2C" -> do
    c <- gets c
    cMax <- gets cMax
    modify $ \st -> st {c = c ++ [(s, targs)], cMax = max cMax (length targs)}

type Env = St

type Output = [String]

type Generate = Writer Output

-- Generate array definitions
generateArrays :: Env -> Output
generateArrays (St {aMax = a, bMax = b, cMax = c, rMax = r}) =
  generateArray "a" a
    ++ generateArray "b" b
    ++ generateArray "c" c
    ++ generateArray "r" r
    ++ [""]

-- Generate a single array definition
generateArray :: String -> Int -> Output
generateArray name max = case max of
  0 -> [""]
  _ -> ["struct target " ++ name ++ "[256][" ++ show max ++ "];"]

generate :: Env -> Output
generate env =
  prefix
    ++ generateArrays env
    ++ initFPrefix
    ++ tab (snd (runWriter $ generateOutput env))
    ++ initFSuffix
  where
    prefix :: Output
    prefix =
      lines
        "#define DEVICE_KEYBOARDD 0x01\n\
        \#define DEVICE_MOUSE 0x02\n\
        \#define DEVICE_CONSUMER 0x03\n\n\
        \struct target {\n\
        \  byte type;\n\
        \  byte code;\n\
        \};\n\n"
    initFPrefix :: Output
    initFPrefix = lines "void init_keys() {\n"
    initFSuffix :: Output
    initFSuffix = lines "}"
    tab :: Output -> Output
    tab = map ("  " ++)

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
      tell [generateDeviceAssignment name source target i]
      tell [generateCodeAssigment name source target i]
      generateAssignments' name source targets $ i + 1
    generateAssignments' _ _ [] _ = pure ()

generateDeviceAssignment :: String -> String -> Targ -> Integer -> String
generateDeviceAssignment name source (Target device _) i =
  name
    ++ "[0x"
    ++ source
    ++ "]["
    ++ show i
    ++ "].type = "
    ++ dev device
    ++ ";"
  where
    dev :: Device -> String
    dev device = case device of
      Keyboard -> "DEVICE_KEYBOARD"
      Mouse -> "DEVICE_MOUSE"
      Consumer -> "DEVICE_CONSUMER"

generateCodeAssigment :: String -> String -> Targ -> Integer -> String
generateCodeAssigment name source (Target _ (KeyCode (Ident code))) i =
  name
    ++ "[0x"
    ++ source
    ++ "]["
    ++ show i
    ++ "].code = "
    ++ code
    ++ ";"

parse :: String -> IO Defs
parse s =
  case pDefs $ myLexer s of
    Left err -> do
      putStrLn "Error"
      putStrLn err
      exitFailure
    Right prog -> do
      return prog

main :: IO ()
main = do
  f <- readFile "./keys.def"
  parsed <- parse f
  writeFile "out.c" $ unlines $ generate $ interpret parsed
  return ()