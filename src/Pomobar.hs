{-# LANGUAGE OverloadedStrings #-}

module Pomobar
  (
    Colour,
    TimerConfig (..),
    defaultTimerConfig,
    initialise
  ) where

import Control.Concurrent
import Control.Monad
import Data.Int (Int16)
import Data.Time
import Data.Time.Clock
import DBus.Client
import Text.Printf (printf)
import System.IO (hSetBuffering, stdout, BufferMode(..))
import System.Process (spawnCommand)

data TimerConfig = TimerConfig {
  width :: Int,
  runningFgColour     :: Maybe Colour,
  pausedFgColour      :: Maybe Colour,
  terminatingFgColour :: Maybe Colour,
  terminatedFg1Colour :: Maybe Colour,
  terminatedBg1Colour :: Maybe Colour,
  terminatedFg2Colour :: Maybe Colour,
  terminatedBg2Colour :: Maybe Colour,
  terminatedBgDelay   :: Maybe Int,
  startedShellCmd  :: Maybe String,
  terminatedShellCmd  :: Maybe String
}

data TimerStatus = Running | Standing | Paused | Terminated deriving Eq
data TimerState = TimerState {
  status        :: TimerStatus,
  duration      :: Int, -- how long timer will run for in seconds
  split         :: Int, -- how many shifts in timer
  started       :: UTCTime, -- what time started
  refreshThread :: MVar ThreadId }

data Timer = Timer (MVar TimerState) TimerConfig

type Colour = String

defaultTimerConfig :: TimerConfig
defaultTimerConfig = TimerConfig
                       (50)
                       (Just "green")
                       (Just "#4682B4")
                       (Just "orange")
                       (Just "red")
                       Nothing
                       (Just "red")
                       (Just "yellow")
                       (Just 500000)         -- 0.5 seconds
                       Nothing
                       Nothing

initialise :: TimerConfig -> IO ()
initialise timerConfig = do
  hSetBuffering stdout LineBuffering
  timer <- newTimer timerConfig
  startDBus timer
  putStrLn "<fc=#51afef><fn=2>P</fn></fc>"
  waitForever

executeCmd :: Maybe String -> IO ()
executeCmd Nothing    = return ()
executeCmd (Just cmd) = spawnCommand cmd >> return ()

newTimer :: TimerConfig -> IO Timer
newTimer timerConfig = do
  thread <- newEmptyMVar
  now <- getCurrentTime
  state <- newMVar (TimerState Terminated 0 0 now thread)
  return $ Timer state timerConfig


startTimer :: Timer -> Int -> Int -> IO ()
startTimer timer@(Timer mvarState timerConfig) dur spl = do
  state <- takeMVar mvarState
  tryTakeMVar (refreshThread state) >>= tryKillThread
  rtID <- forkIO $ timerRefreshThread timer
  putMVar (refreshThread state) rtID
  now <- getCurrentTime
  executeCmd $ startedShellCmd timerConfig
  putMVar mvarState $ TimerState Running (fromIntegral dur) (fromIntegral spl) now (refreshThread state)
  where tryKillThread (Just threadId) = killThread threadId
        tryKillThread Nothing = return ()

pauseTimer :: Timer -> IO ()
pauseTimer (Timer mvarState timerConfig) = do
  state <- takeMVar mvarState
  case (status state) of
    Running -> do
      now <- getCurrentTime
      takeMVar (refreshThread state) >>= killThread
      let remaining = calculateRemaining now state
      let newState = state { status = Paused, duration = remaining }
      putStrLn $ formatOutput remaining (status newState) timerConfig
      putMVar mvarState newState
    _ -> putMVar mvarState state

-- |Add minutes to the timer.
timerAdd :: Timer -> Int -> IO ()
timerAdd timer@(Timer mvarState timerConfig) x = do
  state <- takeMVar mvarState
  now <- getCurrentTime
  let diffSec = x * 60
  let remaining = calculateRemaining now state
  let newRemaining = remaining + diffSec
  if newRemaining < 0 && ((status state) /= Terminated)
    then putMVar mvarState state
    else do
      let newState = state { duration = (duration state) + diffSec }
      case (status newState) of
        Running    -> do
          takeMVar (refreshThread newState) >>= killThread
          rtID <- forkIO $ timerRefreshThread timer
          putMVar (refreshThread newState) rtID
          putMVar mvarState newState
        Paused     -> do
          putStrLn $ formatOutput newRemaining Paused timerConfig
          putMVar mvarState newState
        Terminated -> do
          putMVar mvarState newState
          startTimer timer diffSec (split state)

resumeTimer :: Timer -> IO ()
resumeTimer timer@(Timer mvarState _) = do
  state <- readMVar mvarState
  case (status state) of
    Paused -> startTimer timer (duration state) (split state)
    _      -> return ()

terminateTimer :: Timer -> IO ()
terminateTimer (Timer mvarState timerConfig) = do
  state <- takeMVar mvarState
  putMVar mvarState $ state { status = Terminated }
  executeCmd $ terminatedShellCmd timerConfig
  forM_ [0,(-1)..(-20)] blink
  where blink x = do
          putStrLn $ formatOutput x Terminated timerConfig
          threadDelay $ delay $ terminatedBgDelay timerConfig
        delay Nothing  = 0
        delay (Just x) = x

timerRefreshThread :: Timer -> IO ()
timerRefreshThread timer@(Timer mvarState timerConfig) = do
  state <- readMVar mvarState
  now <- getCurrentTime
  let 
    durDiff = calculateRemaining now state
    mins    = formatOutput durDiff (status state) timerConfig
    line    = (timerLine (width timerConfig) (split state) now state)
  if durDiff <= 0
    then terminateTimer timer
    else do putStrLn $ (line ++ " ∙ " ++ mins)
            threadDelay $ 1000000
            timerRefreshThread timer





formatOutput :: Int -> TimerStatus -> TimerConfig -> String
formatOutput x s c = xmobarString (printf "%02d" mins ++ ":" ++ printf "%02d" secs) (fgColour s) (bgColour s) where
  mins :: Int
  mins
    | x > 60    = floor (fromIntegral x / 60)
    | otherwise = 0
  secs :: Int
  secs
    | x > 0     = x `mod` 60
    | otherwise = 0
  fgColour Paused = pausedFgColour c
  fgColour Running
    | x >= 60   = runningFgColour c
    | otherwise = terminatingFgColour c
  fgColour Terminated = if x `rem` 2 == 0 then terminatedFg1Colour c else terminatedFg2Colour c
  bgColour Terminated = if x `rem` 2 == 0 then terminatedBg1Colour c else terminatedBg2Colour c
  bgColour _          = Nothing

calculateRemaining :: UTCTime -> TimerState -> Int
calculateRemaining time state =
  (duration state) - round (diffUTCTime time (started state))

startDBus :: Timer -> IO ()
startDBus timer@(Timer mvarState _) = do
  client <- connectSession
  _ <- requestName client "org.pomobar" []
  timerSwitchState <- newMVar 0
  export client "/org/pomobar"
    [
      autoMethod "org.Pomobar" "startTimer" dbusStart,
      autoMethod "org.Pomobar" "startTimerGraph" dbusStartGraph,
      autoMethod "org.Pomobar" "pauseResumeTimer" dbusPauseResume,
      autoMethod "org.Pomobar" "timerAddMin" dbusTimerAdd,
      autoMethod "org.Pomobar" "startTimerSwitch" (dbusStartTimerSwitch timerSwitchState)
    ]
  where 
    dbusStart :: Int16 -> IO ()
    dbusStart durationMin = startTimer timer (fromIntegral durationMin * 60) 1 
    dbusPauseResume = do
      state <- readMVar mvarState
      case (status state) of
        Running -> pauseTimer timer
        Paused  -> resumeTimer timer
        _       -> return ()
    dbusStartGraph :: [Int16] -> IO()
    dbusStartGraph [t,k] = startTimer timer (fromIntegral t * 60) (fromIntegral k)
    dbusStartGraph _ = error "bad!"
    dbusTimerAdd :: Int16 -> IO()
    dbusTimerAdd = timerAdd timer . fromIntegral
    dbusStartTimerSwitch :: MVar Int -> [Int16] -> IO ()
    dbusStartTimerSwitch switchState xs = do
      now <- getCurrentTime
      state <- readMVar mvarState
      i <- if (diffUTCTime now (started state)) > 1.0
             then swapMVar switchState 0 >> return 0
             else modifyMVar switchState (\x -> return (x+1,x+1))
      startTimer timer (60 * fromIntegral ((cycle xs) !! i)) 1

xmobarString :: String -> Maybe String -> Maybe String -> String
xmobarString s Nothing _ = s
xmobarString s (Just fg) bg = "<fc=" ++ fg ++ stringBg bg ++ ">" ++ s ++ "</fc>"
  where stringBg Nothing  = ""
        stringBg (Just c) = "," ++ c

blue :: String -> String
blue str = "<fc=#51afef>" ++ str ++ "</fc>"
yellow :: String -> String
yellow str = "<fc=#FB8B24>" ++ str ++ "</fc>"
green :: String -> String
green str = "<fc=#5CFF95>" ++ str ++ "</fc>"


waitForever :: IO ()
waitForever = forever $ threadDelay maxBound

------ GRAPH TIMER STUFF ------
line :: Int -> [String]
line n = replicate (n) ("-")

splitVals :: Int -> [Double]
splitVals n = [ 1 * (fromIntegral k /fromIntegral n) | k <- [1..n] ]

fracLen :: Int -> Double -> Int
fracLen n r = round (fromIntegral n * r)

placeChar :: String -> [String] -> Int -> [String]
placeChar str graph k  = 
  [if i == k then str else c | (i,c) <- zip [0..] graph ] 

dot :: String -> Double -> [String] -> [String]
dot str r graph = placeChar str graph (fracLen (length graph) r)

addBreaks :: Int -> String -> [String] -> [String] 
addBreaks n str graph = foldr (dot str) graph (splitVals n)   

wrap :: [String] -> (String,String) -> [String]
wrap xs (l,r) = [l] ++ xs ++ [r]

bst :: UTCTime -> String 
bst t = 
  let t' = addUTCTime (60 * 60 :: NominalDiffTime) t
  in (formatTime defaultTimeLocale "%R" t')

-- adds breaks, wraps in [], adds start/end time to both ends
myLine :: Int -> Int -> Double -> (String, String) -> String
myLine n k r (start,end) = concat (wrap now (blue $ start ++ " [", blue $ "] " ++ end))
  where 
    (hd,tl) = splitAt (fracLen n r) (line n)
    elapsed = (map green hd ++ map blue tl)
    breaks  = (addBreaks k (yellow "o") elapsed)
    now     = dot (blue "*") r breaks

  
timerFrac :: UTCTime -> TimerState -> Double
timerFrac time state = 
  let t = (duration state)
  in fromIntegral (t - calculateRemaining time state) / fromIntegral t

startAndEndTimes :: TimerState -> (String, String)
startAndEndTimes state = 
    (bst (started state), 
      bst (addUTCTime (fromIntegral (duration state) :: NominalDiffTime) (started state)))

timerLine :: Int -> Int -> UTCTime -> TimerState -> String
timerLine n k time state = 
  myLine n k (timerFrac time state) (startAndEndTimes state)

