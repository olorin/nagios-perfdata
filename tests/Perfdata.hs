{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Nagios.Perfdata
import Test.Hspec
import Test.HUnit
import Data.ByteString (ByteString)
import Data.Either.Utils
import Control.Monad.IO.Class

defaultTemplateData :: ByteString
defaultTemplateData = 
    "DATATYPE::SERVICEPERFDATA\tTIMET::1388445486\tHOSTNAME::node4.example.com\tSERVICEDESC::diskio\tSERVICEPERFDATA::overall_read_count=32294c;;;;; overall_write_count=6497849c;;;;; overall_read_bytes=468035584c;;;;; overall_write_bytes=40444282880c;;;;; dm-1_read_count=1343c;;;;; dm-1_write_count=14379c;;;;; dm-1_read_bytes=5500928c;;;;; dm-1_write_bytes=58896384c;;;;; vda1_read_count=235c;;;;; vda1_write_count=102c;;;;; vda1_read_bytes=937984c;;;;; vda1_write_bytes=141312c;;;;; dm-0_read_count=17308c;;;;; dm-0_write_count=4922832c;;;;; dm-0_read_bytes=227918848c;;;;; dm-0_write_bytes=20163174400c;;;;; vda2_read_count=13408c;;;;; vda2_write_count=1560536c;;;;; vda2_read_bytes=233677824c;;;;; vda2_write_bytes=20222070784c;;;;;\tSERVICECHECKCOMMAND::check_diskio\tHOSTSTATE::UP\tHOSTSTATETYPE::HARD\tSERVICESTATE::OK\tSERVICESTATETYPE::HARD"

cpuTemplateData :: ByteString
cpuTemplateData = 
    "DATATYPE::SERVICEPERFDATA\tTIMET::1388445486\tHOSTNAME::some.example.com\tSERVICEDESC::cpu\tSERVICEPERFDATA::CpuUtilisation=0.00%;;;0;100; CpuUser=1092132c;;;;;\tSERVICESTATE::OK\tHOSTSTATE::OK"

defaultCheckResult :: ByteString
defaultCheckResult =
    "Monitoring cluster 'chevalier-01' | cluster_nodes=2;;;;; cluster_master_eligible_nodes=2;;;;; cluster_data_nodes=2;;;;; cluster_active_shards=40;;;;; cluster_relocating_shards=0;;;;; cluster_initialising_shards=0;;;;; cluster_unassigned_shards=0;;;;; cluster_total_shards=40;;;;; storesize=59531193B;;;;; documents=344051;;;;; index_ops=45185399c;;;;; index_time=12068244c;;;;; flush_ops=7334c;;;;; flush_time=1261970c;;;;; throttle_time=3442491c;;;;; index_ops=45185399c;;;;; index_time=12068244c;;;;; delete_ops=3c;;;;; delete_time=2c;;;;; get_ops=34805703c;;;;; get_time=3474984c;;;;; exists_ops=33849630c;;;;; exists_time=3454577c;;;;; missing_ops=956073c;;;;; missing_time=20407c;;;;; query_ops=3647965c;;;;; query_time=1011691c;;;;; fetch_ops=750991c;;;;; fetch_time=77923c;;;;; merge_ops=440c;;;;; refresh_ops=63770c;;;;; refresh_time=2847079c;;;;;"

suite :: Spec
suite = do
    describe "perfdataFromDefaultTemplate" $ do
        it "extracts perfdata from Nagios perfdata template"  $
            perfdataFromDefaultTemplate defaultTemplateData `shouldSatisfy` good
        it "extracts timestamp correctly" $
            (perfdataTimestamp . fromRight $ perfdataFromDefaultTemplate defaultTemplateData) @?= 1388445486000000000
        it "handles empty threshold fields correctly" $ do
            let datum = (perfdataFromDefaultTemplate cpuTemplateData)
            datum `shouldSatisfy` good
            let metrics = perfdataMetrics . fromRight $ datum
            fst (metrics !! 1) @?= "CpuUser"
    describe "perfdataFromCheckResult" $
        it "extracts perfdata from Nagios check result"  $
            perfdataFromCheckResult defaultCheckResult `shouldSatisfy` good
  where
    good (Left _) = False
    good (Right _) = True

main :: IO ()
main = hspec suite

