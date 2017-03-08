module Main where
import Test.Tasty
import qualified Codec.MIME.TypeTests as TYP
import qualified Codec.MIME.DecodeTests as DEC

main :: IO ()
main = 
  defaultMain 
    (testGroup "-" 
      [ TYP.test
      , DEC.test
      ])
