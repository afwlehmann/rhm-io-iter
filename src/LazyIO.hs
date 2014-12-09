module LazyIO where

import Control.Monad
import Data.Digest.Pure.MD5
import System.Environment
import System.IO
import qualified Data.ByteString.Lazy as BL

-- Resources closed "unexpectedly" as soon as the IO action returns
problem1 :: IO ()
problem1 = do
  [fileName] <- getArgs
  str <- withFile fileName ReadMode hGetContents
  putStrLn str

-- Solving problem1 by evaluating the contents of the file inside withFile's IO
-- action
problem1' :: IO ()
problem1' = do
  [fileName] <- getArgs
  withFile fileName ReadMode $ hGetContents >=> putStrLn

-- Using too many resources at once
--
-- For this example to work it may be necessary to set an "artificial" resource
-- limit by executing `ulimit -n 128` in bash before `cabal run lazyio-demo`.
--
-- Example:
-- > ulimit -n 128
-- > cabal run lazyio-demo $(find /usr/include -name a\*.h)
problem2 :: IO ()
problem2 = getArgs >>= mapM (BL.readFile >=> return . show . md5) >>= print

main :: IO ()
main = problem1
