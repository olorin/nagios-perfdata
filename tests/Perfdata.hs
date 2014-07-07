{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Nagios.Perfdata
import Test.Hspec
import Test.HUnit
import Data.ByteString (ByteString)
import Data.Either.Utils

defaultTemplateData :: ByteString
defaultTemplateData = 
    "DATATYPE::SERVICEPERFDATA\tTIMET::1388445486\tHOSTNAME::node4.example.com\tSERVICEDESC::diskio\tSERVICEPERFDATA::overall_read_count=32294c;;;;; overall_write_count=6497849c;;;;; overall_read_bytes=468035584c;;;;; overall_write_bytes=40444282880c;;;;; dm-1_read_count=1343c;;;;; dm-1_write_count=14379c;;;;; dm-1_read_bytes=5500928c;;;;; dm-1_write_bytes=58896384c;;;;; vda1_read_count=235c;;;;; vda1_write_count=102c;;;;; vda1_read_bytes=937984c;;;;; vda1_write_bytes=141312c;;;;; dm-0_read_count=17308c;;;;; dm-0_write_count=4922832c;;;;; dm-0_read_bytes=227918848c;;;;; dm-0_write_bytes=20163174400c;;;;; vda2_read_count=13408c;;;;; vda2_write_count=1560536c;;;;; vda2_read_bytes=233677824c;;;;; vda2_write_bytes=20222070784c;;;;;\tSERVICECHECKCOMMAND::check_diskio\tHOSTSTATE::UP\tHOSTSTATETYPE::HARD\tSERVICESTATE::OK\tSERVICESTATETYPE::HARD"

suite :: Spec
suite =
    describe "perfdataFromDefaultTemplate" $ do
        it "extracts perfdata from Nagios perfdata template"  $
            perfdataFromDefaultTemplate defaultTemplateData `shouldSatisfy` good
        it "extracts timestamp correctly" $
            (perfdataTimestamp . fromRight $ perfdataFromDefaultTemplate defaultTemplateData) @?= 1388445486000000000
  where
    good (Left _) = False
    good (Right _) = True

main :: IO ()
main = hspec suite

