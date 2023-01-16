import Prelude
import Data.List
import Data.Char


super_digit_locked::Int -> Int -> Int
super_digit_locked x acc|x `div` 10 == 0 = acc + x
                        |otherwise = super_digit_locked new_x new_acc
                            where
                            new_x = (x `div` 10)
                            new_acc = acc + (x `mod` 10)


super_digit::Int -> Int
super_digit x|new_x `div` 10 == 0 = new_x
             |otherwise = super_digit new_x
                where
                    new_x = super_digit_locked x 0


solve::Int -> Int -> Int
solve x k = super_digit new_x
    where
        new_x = k * (super_digit x)

sum_chars::String -> Int -> Int
sum_chars [] acc = acc
sum_chars (c:cs) acc = sum_chars cs (acc + (digitToInt c))

main :: IO ()
main = do
  val <- getLine
  let 
    [x,k] = words val 
    ans = solve (read x ::Int) (read k ::Int)
  print ans