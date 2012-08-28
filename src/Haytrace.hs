module Main
    where

import Data
import Vector
import Raytrace
import Lang
import PPM
import System.FilePath (takeBaseName)
import System.Environment (getArgs)

info = "Haytrace 0.10\n usage: Haytrace <Scene File Path>\n"
main = do
  args <- getArgs
  if null args
    then putStrLn info
    else renderFile $ head args

renderFile :: FilePath -> IO ()
renderFile filename = do 
  input <- readFile filename
  let Right sl = runSceneParser input
      saveName = takeBaseName filename ++ ".ppm"
      scene = evaluate sl
      w = width scene
      h = height scene
  savePPM w h saveName $ raytrace 50 scene
