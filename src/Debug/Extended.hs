module Debug.Extended
  ( todo,
    dbg,
    dbg',
    unwrapEither,
    getElem,
    unwrapMaybe,
    justOr,
    showFloatN,
    trace,
  )
where

import Debug.Trace (trace)
import GHC.Stack
import Numeric

todo :: String -> a
todo msg = error ("not yet implemented: " ++ msg)

dbg :: (Show a, HasCallStack) => a -> a
dbg a = trace ("[" <> filename <> ":" <> lineNumber <> ":" <> col <> "] " <> show a) a
  where
    (_funcName, info) = last $ getCallStack callStack
    filename = srcLocFile info
    lineNumber = show $ srcLocStartLine info
    col = show $ srcLocStartCol info

dbg' :: (Show a, HasCallStack) => String -> a -> a
dbg' string a = trace ("[" <> filename <> ":" <> lineNumber <> ":" <> col <> "] " <> string <> " " <> show a) a
  where
    (_funcName, info) = last $ getCallStack callStack
    filename = srcLocFile info
    lineNumber = show $ srcLocStartLine info
    col = show $ srcLocStartCol info

unwrapEither :: (Show a) => Either a b -> b
unwrapEither (Left e) = error $ show e
unwrapEither (Right value) = value

unwrapMaybe :: Maybe a -> a
unwrapMaybe Nothing = error "Expected Value in Maybe got: Nothing"
unwrapMaybe (Just value) = value

justOr :: Maybe a -> err -> Either err a
justOr Nothing left = Left left
justOr (Just value) _ = Right value

showFloatN :: (RealFloat a) => a -> Int -> String
showFloatN floatNum numOfDecimals = showFFloat (Just numOfDecimals) floatNum ""

getElem :: [a] -> Int -> Maybe a
getElem [] _ = Nothing
getElem (x : xs) index
  | index <= 0 = Just x
  | otherwise = getElem xs (index - 1)