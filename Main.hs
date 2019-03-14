{-# LANGUAGE FlexibleInstances, RecordWildCards,LambdaCase #-}
module Main where
import Control.Concurrent.MonadIO hiding (throwTo)
import Control.Concurrent.STM.MonadIO
import Control.Exception.Lifted
import Control.Monad.Reader
import Network
import System.IO
import Text.Printf


data Msg = Move Move | GameOver deriving (Eq,Show)

data Move = DoneAndAttack | Done deriving (Eq,Show)

type Stats = [Int]

data ControlMsg = OK | RequestStats

data FightType = Defend | Attack | Both deriving Show
data Fight = Fight  FightType Stats

instance Show Fight where
  show (Fight fType stats) = unwords $ show fType : map show stats

data Client = Client { controlChan :: Chan ControlMsg
                     , moveChan   :: Chan Move
                     , statsChan  :: Chan Stats
                     , fightChan  :: Chan Fight }

data Env = Env {ids :: TVar [ThreadId], mvar :: MVar ()}

type App = ReaderT Env IO

instance HasFork App where
  fork a = ask >>= liftIO . forkIO . runReaderT a

main :: IO ()
main = runServer

runServer :: IO ()
runServer = withSocketsDo $ do
  port <- read <$> readFile "portConfig.txt"
  hSetBuffering stdout LineBuffering
  sock <- listenOn $ PortNumber port
  putStrLn $ "Listening on port " ++ show port
  forever $ handleGameSession sock
 
handleGameSession :: Socket -> IO ()
handleGameSession sock = do
  putStrLn "waiting for clients"
  env <- Env <$> newTVar [] <*> newEmptyMVar 
  flip runReaderT env $ do
    fork runStopWatcher
    c1 <- acceptClient sock
    c2 <- acceptClient sock
    smartFork $ runControl c1 c2

runStopWatcher :: App ()
runStopWatcher = do
  asks mvar >>= readMVar
  ids <- asks ids >>= readTVar
  forM_ ids $ flip throwTo $ SomeException ThreadKilled

acceptClient :: Socket -> App Client
acceptClient sock = do
  (handle, host, port') <- liftIO $ accept sock
  liftIO $ do
    printf "Accepted connection from %s: %s\n" host (show port')
    hSetNewlineMode handle universalNewlineMode
    -- hSetBuffering handle LineBuffering
  client <- Client <$> newChan <*> newChan <*> newChan <*> newChan
  smartFork (runClient handle client `finally` cleanup handle)
  return client
  where 
    cleanup handle = do
      closeHandle handle
      say "closed client handle"
      stop

-- a fork which also keeps track of the ids of threads that were created
smartFork :: App () -> App ()
smartFork a = fork a >>= \id -> asks ids >>= flip modifyTVar_ (id:)

-- handling the connection to one client
runClient :: Handle -> Client -> App ()
runClient handle c@Client {..} = forever $ do
  msg <- getClientMsg
  say "waiting for client move"
  say $ "received: " ++ msg
  let move = parseMsg msg
  say $ "parsed move: " ++ show move
  when (move==Move Done) $ sendToClient "endturn received"
  case move of
    GameOver -> stop
    Move move -> writeChan moveChan move >> response
  where
    -- helper functions to make the logic more readable
    getClientMsg :: App String
    getClientMsg = liftIO $ hGetLine handle
    sendToClient :: String -> App ()
    sendToClient = liftIO . hPutStrLn handle
    response :: App ()
    response = readChan controlChan >>= \case
        OK -> sendToClient "ok"
        RequestStats -> handleFight
    handleFight :: App ()
    handleFight = do
      sendToClient "sendData"
      parseStats <$> getClientMsg >>= writeChan statsChan
      readChan fightChan >>= sendToClient . show

parseMsg :: String -> Msg 
parseMsg "gameover" = GameOver
parseMsg "attack" = Move DoneAndAttack
parseMsg "endturn" = Move Done
parseMsg _ = error "invalid Move"

parseStats :: String -> Stats
parseStats str = case map read $ words str of
  xs | length xs == 4 -> xs
  _ -> error "invalid stats recieved"

-- the thread which handles control/overall logic
runControl :: Client -> Client -> App ()
runControl (Client chanA moveA statsA fightA) (Client chanB moveB statsB fightB) = forever $ do
  say "new turn"
  (,) <$> readChan moveA <*> readChan moveB >>= logic -- receive clients move
  where logic :: (Move,Move) -> App ()
        logic (Done,Done) = writeChan chanA OK >> writeChan chanB OK
        logic (Done,DoneAndAttack) = manageFight Defend Attack
        logic (DoneAndAttack,Done) = manageFight Attack Defend
        logic (DoneAndAttack,DoneAndAttack) = manageFight Both Both
        manageFight a b = do
          statsA <- writeChan chanA RequestStats >> readChan statsA
          statsB <- writeChan chanB RequestStats >> readChan statsB
          writeChan fightA $ Fight a statsB
          writeChan fightB $ Fight b statsA

-- helper functions

stop :: App ()
stop = asks mvar >>= flip putMVar ()

closeHandle :: Handle -> App ()
closeHandle = liftIO . hClose

say :: MonadIO m => String -> m ()
say = liftIO . putStrLn