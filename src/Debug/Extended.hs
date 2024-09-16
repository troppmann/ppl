module Debug.Extended
  ( todo,
    dbg,
    unwrap,
  )
where

import Debug.Trace (trace)
import GHC.Stack

todo :: String -> a
todo msg = error ("not yet implemented: " ++ msg)

dbg :: (Show a, HasCallStack) => a -> a
dbg a = trace ("[" <> filename <> ":" <> lineNumber <> ":" <> col <> "] " <> show a) a
  where
    (_funcName, info) = last $ getCallStack callStack
    filename = srcLocFile info
    lineNumber = show $ srcLocStartLine info
    col = show $ srcLocStartCol info

unwrap :: (Show a) => Either a b -> b
unwrap (Left e) = error $ show e
unwrap (Right value) = value