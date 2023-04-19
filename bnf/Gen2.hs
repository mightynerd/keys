import Control.Monad.State
import Control.Monad.Writer (Writer)
import Keys.Abs
import Keys.Par
import Lib
import System.Exit (exitFailure)

type SourceTarget = (Hex, [Targ])

type SourceTargets = [SourceTarget]

data St = St
  { a :: SourceTargets,
    aMax :: Int,
    b :: SourceTargets,
    bMax :: Int,
    c :: SourceTargets,
    cMax :: Int,
    r :: SourceTargets,
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
-- `size` is greater by one because of the null target
generateArrays :: Env -> Output
generateArrays (St {aMax, a, bMax, b, cMax, c, rMax, r}) =
  [ generateArray "a" (aMax + 1) a,
    generateArray "b" (bMax + 1) b,
    generateArray "c" (cMax + 1) c,
    generateArray "r" (rMax + 1) r
  ]

-- Generate a single array definition
generateArray :: String -> Int -> SourceTargets -> String
generateArray name size sts =
  "const PROGMEM struct target "
    ++ name
    ++ "[256]["
    ++ show size
    ++ "] = {"
    ++ joins (map (`genTargets` sts) (take 256 [0 ..])) ","
    ++ "};"

-- Generate targets "{{a, b}, {c, d}}"
genTargets :: Int -> SourceTargets -> String
genTargets i sts = case current of
  Just (_, targs) -> "{" ++ joins (map genTarget targs) "," ++ ",{0,0}}"
  Nothing -> "{{0,0},{0,0}}"
  where
    current = find sts (\(Hex source, _) -> i == parseHex source)

-- Generate a single target {a, b}
genTarget :: Targ -> String
genTarget ((Target device (KeyCode key))) = "{" ++ dev device ++ ", " ++ key ++ "}"
  where
    dev :: Device -> String
    dev device = case device of
      Keyboard -> "DEVICE_KEYBOARD"
      Mouse -> "DEVICE_MOUSE"
      Consumer -> "DEVICE_CONSUMER"

generate :: Env -> Output
generate env = prefix ++ generateArrays env
  where
    prefix :: Output
    prefix =
      lines
        "#define DEVICE_KEYBOARD 0x01\n\
        \#define DEVICE_MOUSE 0x02\n\
        \#define DEVICE_CONSUMER 0x03\n\n\
        \struct target {\n\
        \  byte type;\n\
        \  byte code;\n\
        \};\n\n"
    tab :: Output -> Output
    tab = map ("  " ++)

parse :: String -> IO Defs
parse s =
  case pDefs $ myLexer s of
    Left err -> do
      putStrLn "Error"
      putStrLn err
      exitFailure
    Right prog -> return prog

main :: IO ()
main = do
  f <- readFile "./keys.def"
  parsed <- parse f
  writeFile "out.c" $ unlines $ generate $ interpret parsed
  return ()
