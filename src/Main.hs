-- |Main module, producing an executable that calls the appropriate functions from the Series module.
module Main where
import Series

import Control.Monad(liftM)

-- |Reads parameters to construct the series, prints out the results of the various functions,
-- and finally reads more parameters for the "special number 2" and prints out the result.
main :: IO ()
main = 
   do 
     putStr "Enter x: "
     xParam <- liftM (read :: String -> Double) getLine
     putStr "Enter y: "
     yParam <- liftM (read :: String -> Double) getLine
     putStr "Enter the length of the series: "
     lengthParam <- liftM (read :: String -> Int) getLine
     let fstNum = first xParam
     let growthRate = grow fstNum yParam
     let ss = take lengthParam $ series fstNum growthRate
     let ssRounded = seriesN fstNum growthRate lengthParam
     putStrLn "The series is: "
     if null ss
        then putStrLn "[]" 
        else mapM_ print ss
     putStrLn "The series rounded and sorted is: "
     if null ssRounded
        then putStrLn "[]" 
        else mapM_ print ssRounded   
     putStrLn $ "Number1 is: " ++ either id show (special1 ssRounded)
     putStr "For the approximateNumber, enter a y: "
     y2Param <- liftM (read :: String -> Double) getLine
     putStr "For the approximateNumber, enter a z: "
     zParam <- liftM (read :: String -> Double) getLine
     putStrLn $ "Number2 is: " ++ either id show (special2 y2Param zParam ssRounded)