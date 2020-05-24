{-# OPTIONS -syslib posix #-}
-----------------------------------------------------------------------------------------
-- Haskell binding for daVinci API
-- 
-- Sven Panne <Sven.Panne@informatik.uni-muenchen.de> 1997/99
-----------------------------------------------------------------------------------------

module DaVinci(
   module DaVinciTypes,
   DaVinciProcess, startDaVinci, stopDaVinci, sendCmd, receiveAnswer
   )
where

import DaVinciTypes
import Monad(when)
import IO(stderr, Handle, BufferMode(..), hSetBuffering, hGetLine, hPutStrLn, hClose)
import Posix(forkProcess,executeFile,
             Fd, createPipe, dupTo, fdClose, intToFd, fdToHandle, handleToFd)
import PosixUtil(ProcessID)
import Directory(setCurrentDirectory)

-- A *nix-classic... (but I wouldn't mind a popen/pclose in GHC's Posix lib)
runPiped :: FilePath			    -- Command
         -> [String]			    -- Arguments
         -> Maybe [(String, String)]	    -- Environment
         -> Maybe FilePath		    -- Working directory    
         -> IO (ProcessID, Handle, Handle)  -- (pid, fromChild, toChild)
runPiped path args env dir = do
   (rd1, wd1) <- createPipe
   (rd2, wd2) <- createPipe
   maybePid   <- forkProcess
   case maybePid of
      -- child
      Nothing   -> do maybe (return ()) setCurrentDirectory dir
                      dupTo rd1 (intToFd 0)
                      dupTo wd2 (intToFd 1)
                      mapM_ fdClose [rd1, wd1, rd2, wd2]
                      executeFile path True args env
                      ioError (userError "runPiped")
      -- parent
      Just pid  -> do mapM_ fdClose [rd1, wd2]
                      fromChild <- fdToHandle rd2
                      toChild   <- fdToHandle wd1
                      hSetBuffering fromChild LineBuffering
                      hSetBuffering toChild   LineBuffering
                      return (pid, fromChild, toChild)

data DaVinciProcess = DaVinciProcess ProcessID   -- daVinci's process id
                                     Handle      -- from daVinci
                                     Handle      -- to daVinci
                                     Bool        -- Trace?

startDaVinci :: Bool -> IO DaVinciProcess
startDaVinci trc = do
   (pid, fromDV, toDV) <- runPiped "daVinci" ["-pipe"] Nothing Nothing
   let d = DaVinciProcess pid fromDV toDV trc
   getOk d
   return d

stopDaVinci :: DaVinciProcess -> IO ()
stopDaVinci (DaVinciProcess _ fromDV toDV trc) = do
   when trc (hPutStrLn stderr "stopDaVinci")
   hClose toDV
   hClose fromDV

sendCmd :: DaVinciProcess -> DaVinciCmd -> IO ()
sendCmd d@(DaVinciProcess _ _ toDV trc) cmd = do
   let str = show cmd
   when trc (hPutStrLn stderr ("sendCmd: " ++ str))
   hPutStrLn toDV str
   getOk d

receiveAnswer :: DaVinciProcess -> IO DaVinciAnswer
receiveAnswer (DaVinciProcess _ fromDV _ trc) = do
   reply <- hGetLine fromDV
   case reads reply of
      ((answer,[]):_) -> do when trc (hPutStrLn stderr ("receiveAnswer: " ++ show answer))
                            return answer
      _               -> ioError (userError ("malformed daVinci reply '" ++ reply ++ "'"))

getOk :: DaVinciProcess -> IO ()
getOk d = do
   reply <- receiveAnswer d
   case reply of
      Ok                   -> return ()
      Quit                 -> ioError (userError "getOk: daVinci terminated")
      CommunicationError e -> ioError (userError ("getOk: " ++ show e))
      _                    -> getOk d
