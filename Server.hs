{-# LANGUAGE CPP #-}
#if PRODUCTION
import Controller (withFortuneSearch)
import Network.Wai.Handler.Warp (run)

main :: IO ()
main = withFortuneSearch $ run 3000
#else
import Controller (withFortuneSearch)
import System.IO (hPutStrLn, stderr)
import Network.Wai.Middleware.Debug (debug)
import Network.Wai.Handler.Warp (run)

main :: IO ()
main = do
    let port = 3000
    hPutStrLn stderr $ "Application launched, listening on port " ++ show port
    withFortuneSearch $ run port . debug
#endif
