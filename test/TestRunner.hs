import Test.HUnit

import Bindings.Spotify.Toplist (sp_toplist_region)

testSpToplistRegion :: Test
testSpToplistRegion = TestCase $ assertEqual
    "The correct region should be returned"
    21317 -- 'S' << 8 | 'E'
    sp_toplist_region 'S' 'E'

main :: IO Counts
main = runTestTT $ TestList
  [ testSpToplistRegion
  ]
