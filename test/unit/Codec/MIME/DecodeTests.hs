{-# LANGUAGE OverloadedStrings, RecordWildCards, ScopedTypeVariables #-}
module Codec.MIME.DecodeTests where
import qualified Codec.MIME.Base64 as B64
import           Codec.MIME.Decode
import qualified Codec.MIME.QuotedPrintable as QP
import           Data.Char (isSpace, toLower, ord)
import           Test.Tasty
import           Test.Tasty.HUnit

test :: TestTree
test = testGroup "Codec.MIME.Decode" tests

tests :: [TestTree]
tests = 
  [ testGroup "DecodeWord"
--       [ testCase "examples" (do
--           isXmlType nullType @?= False)

      [ testCase "Decode word-encoded string with US-ASCII characters and no escaped chars" (do
          let val        = "Short header value"
              encodedVal = "=?us-ascii?q?Short header value?="
          decodeWord encodedVal @?= Just (val, ""))
      , testCase "Decode word-encoded string with US-ASCII characters and escaped chars" (do
          let val        = "Short header=value"
              encodedVal = "=?us-ascii?q?Short header=3Dvalue?="
          decodeWord encodedVal @?= Just (val, ""))
      , testCase "Decode word-encoded string with US-ASCII characters and escaped chars and newlines" (do
          let val        = "Short header=value\n Second line of value"
              encodedVal = "=?us-ascii?q?Short header=3Dvalue?=\n Second line of value"
          decodeWords encodedVal @?= val)
      , testCase "Encode a single line string as quoted printable" (do
          let val        = "Short header=value"
              encodedVal = "=?us-ascii?q?Short header=3Dvalue?="
          encodeHeaderLine "us-ascii" "q" val @?= encodedVal)
      , testCase "Encode a multi-line string as base64" (do
          let val        = "Short header=value\n Second line of value\n"
              encodedVal = "=?us-ascii?b?U2hvcnQgaGVhZGVyPXZhbHVl?=\n =?us-ascii?b?U2Vjb25kIGxpbmUgb2YgdmFsdWU?=\n"
          simpleEncodeHeaderLines "us-ascii" "b" val @?= encodedVal)
      , testCase "Encode a multi-line string with mixed quoted-printable and base64 and unicode" (do
          -- Euro sign: U+20AC, hex: \x20ac, decimal: \8364, UTF-8: \xe2 \x82 \xac, 
          let val        = "Short header=value\n A Euro sign looks like \8364\n"
              encodedVal = "=?us-ascii?q?Short header=3Dvalue?=\n =?us-ascii?b?QSBFdXJvIHNpZ24gbG9va3MgbGlrZSCs?=\n"
          encodeHeaderLines "us-ascii" val @?= encodedVal)
      -- TODO need to be able to decode UTF-8
      , testCase "Encode a multi-line string with mixed quoted-printable and base64 and UTF-8" (do
          -- Euro sign: U+20AC, hex: \x20ac, decimal: \8364, UTF-8: \xe2\x82\xac, 
          let val        = "Short header=value\n A Euro sign looks like \xe2\x82\xac\n"
              encodedVal = "=?us-ascii?q?Short header=3Dvalue?=\n =?us-ascii?b?QSBFdXJvIHNpZ24gbG9va3MgbGlrZSDigqw?=\n"
          encodeHeaderLines "us-ascii" val @?= encodedVal)
      ]
  ]


encodeHeaderLine :: String -> String -> String -> String
encodeHeaderLine cs cte val = prefix ++ "=?" ++ cs ++ "?" ++ cte ++ "?" ++ (encode' val') ++ "?="
  where (prefix, val') = span isSpace val
        encode' s = case map toLower cte of
          "q" -> QP.encode s
          "b" -> B64.encodeRawString False s
          _   -> error $ "Bad encoding strategy " ++ cte

-- Applies same encoding strategy to each line.
simpleEncodeHeaderLines :: String -> String -> String -> String
simpleEncodeHeaderLines cs cte val = unlines . map (encodeHeaderLine cs cte). lines $ val

encodeHeaderLines :: String -> String -> String
encodeHeaderLines _ val  = unlines
                         . map (\(cte, s) -> encodeHeaderLine "us-ascii" cte s)
                         . map (\s -> (selectEncodingStrategy s, s))
                         . lines
                         $ val

selectEncodingStrategy :: String -> String
selectEncodingStrategy xs = if any (> 127) (map ord xs) then "b" else "q"

