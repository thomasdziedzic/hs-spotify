import Test.HUnit

import Data.Char (ord)

import Bindings.Spotify.Toplist (sp_toplist_region)

testSpToplistRegion :: Test
testSpToplistRegion = TestCase $ assertEqual
    "The correct region code should be returned"
    21317 -- 'S' << 8 | 'E'
    (sp_toplist_region (chr2cchr 'S') (chr2cchr 'E'))
  where
    chr2cchr = fromIntegral . ord

main :: IO Counts
main = runTestTT $ TestList
  [ testSpToplistRegion
  ]
