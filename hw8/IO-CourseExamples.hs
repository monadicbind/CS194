--IO-CourseExamples.hs
module IOCourseExmpls where

 main :: IO ()
 main = putStrLn "Please enter a number : " >> (getLine >>= (\x -> putStrLn ("The number is " ++ x)))

 test1 :: IO ()
 test1 = do
        putStrLn "Hello , what is your name"
        name <- getLine
        putStrLn ("Hey " ++ name ++ " how are you?")