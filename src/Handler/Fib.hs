module Handler.Fib where

import Import

-- A standard Haskell technique for creating an infinite list of fibonacci values.
fibs :: [Int]
fibs = 1 : 1 : zipWith (+) fibs (tail fibs)

fac :: Int -> Int
fac 0 = 1
fac n = n * fac (n-1)

-- Unlike Handler.Home, our getFibR returns a Value result, which is the datatype
-- used for JSON values. We return our result as a JSON object, and place
-- our integral result under the "value" key.
getFibR :: Int -> Handler Value
getFibR i = return $ object ["value" .= (fibs !! abs i)]

getFacR :: Int -> Handler Value
getFacR n = return $ object ["value" .= (fac n)]