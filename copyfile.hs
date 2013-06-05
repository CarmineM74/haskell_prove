import System.Environment
import qualified Data.ByteString.Lazy as B

main = do
     (fname1:fname2:_) <- getArgs
     copyFile fname1 fname2
       
copyFile :: FilePath -> FilePath -> IO ()
copyFile f1 f2 = do
     contents <- B.readFile f1
     B.writeFile f2 contents