-- Imports and types {{{
module Tests where
import MolSeq
import Profile
import Distance
import EvolTree
import Test.HUnit

type RawSeq = (String, String)
-- }}}
-- Help functions {{{
roundToDecimals :: Integer -> Double -> Double
roundToDecimals n = (/factor) . fromInteger . round . (*factor) where
    factor = 10^n

third :: (a, b, c) -> c
third (_, _, x) = x

convertSeqs :: [RawSeq] -> [MolSeq]
convertSeqs = map $ uncurry string2seq

seqListsToProfileDistance :: [RawSeq] -> [RawSeq] -> DistanceTriplet
seqListsToProfileDistance a b = distance (fromMolSeqs $ convertSeqs a) (fromMolSeqs $ convertSeqs b)

seqListPositionDistance :: [RawSeq] -> Int -> Int -> DistanceTriplet
seqListPositionDistance seqList posA posB = distance (convertSeqs seqList !! posA) (convertSeqs seqList !! posB)
-- }}}
-- Tests {{{
testDistancesBetweenFamLists :: Test
testDistancesBetweenFamLists = TestList
    [ "Distance between fam1 and fam2." ~: 171.1 ~=? roundToDecimals 1 (third $ seqListsToProfileDistance fam1 fam2)
    , "Distance between fam1 and fam3." ~: 176.8 ~=? roundToDecimals 1 (third $ seqListsToProfileDistance fam1 fam3)
    , "Distance between fam1 and fam4." ~: 167.2 ~=? roundToDecimals 1 (third $ seqListsToProfileDistance fam1 fam4)
    , "Distance between fam1 and fam5." ~: 179.3 ~=? roundToDecimals 1 (third $ seqListsToProfileDistance fam1 fam5)
    , "Distance between fam2 and fam3." ~: 151.7 ~=? roundToDecimals 1 (third $ seqListsToProfileDistance fam2 fam3)
    , "Distance between fam2 and fam4." ~: 161.3 ~=? roundToDecimals 1 (third $ seqListsToProfileDistance fam2 fam4)
    , "Distance between fam2 and fam5." ~: 151.2 ~=? roundToDecimals 1 (third $ seqListsToProfileDistance fam2 fam5)
    , "Distance between fam3 and fam4." ~: 160.0 ~=? roundToDecimals 1 (third $ seqListsToProfileDistance fam3 fam4)
    , "Distance between fam3 and fam5." ~: 154.1 ~=? roundToDecimals 1 (third $ seqListsToProfileDistance fam3 fam5)
    , "Distance between fam4 and fam5." ~: 167.0 ~=? roundToDecimals 1 (third $ seqListsToProfileDistance fam4 fam5)
    ]

testDistancesBetweenFOXP4proteins :: Test
testDistancesBetweenFOXP4proteins = TestList
    [ "Distance between human and cow."   ~: 0.090 ~=? roundToDecimals 3 (third $ seqListPositionDistance foxp4 0 1)
    , "Distance between human and dog."   ~: 0.055 ~=? roundToDecimals 3 (third $ seqListPositionDistance foxp4 0 2)
    , "Distance between human and rat."   ~: 0.051 ~=? roundToDecimals 3 (third $ seqListPositionDistance foxp4 0 3)
    , "Distance between human and mouse." ~: 0.055 ~=? roundToDecimals 3 (third $ seqListPositionDistance foxp4 0 4)
    , "Distance between human and frog."  ~: 0.245 ~=? roundToDecimals 3 (third $ seqListPositionDistance foxp4 0 5)
    , "Distance between cow and dog."     ~: 0.119 ~=? roundToDecimals 3 (third $ seqListPositionDistance foxp4 1 2)
    , "Distance between cow and rat."     ~: 0.126 ~=? roundToDecimals 3 (third $ seqListPositionDistance foxp4 1 3)
    , "Distance between cow and mouse."   ~: 0.124 ~=? roundToDecimals 3 (third $ seqListPositionDistance foxp4 1 4)
    , "Distance between cow and frog."    ~: 0.314 ~=? roundToDecimals 3 (third $ seqListPositionDistance foxp4 1 5)
    , "Distance between dog and rat."     ~: 0.090 ~=? roundToDecimals 3 (third $ seqListPositionDistance foxp4 2 3)
    , "Distance between dog and mouse."   ~: 0.090 ~=? roundToDecimals 3 (third $ seqListPositionDistance foxp4 2 4)
    , "Distance between dog and frog."    ~: 0.272 ~=? roundToDecimals 3 (third $ seqListPositionDistance foxp4 2 5)
    , "Distance between rat and mouse."   ~: 0.017 ~=? roundToDecimals 3 (third $ seqListPositionDistance foxp4 3 4)
    , "Distance between rat and frog."    ~: 0.256 ~=? roundToDecimals 3 (third $ seqListPositionDistance foxp4 3 5)
    , "Distance between mouse and frog."  ~: 0.259 ~=? roundToDecimals 3 (third $ seqListPositionDistance foxp4 4 5)
    ]
-- }}}

testNewickOutput :: Test
testNewickOutput = TestList
    [ "fam1 in Newick-format."  ~: fam1string  ~=? newick (makeDistanceMatrix $ convertSeqs fam1)
    , "fam2 in Newick-format."  ~: fam2string  ~=? newick (makeDistanceMatrix $ convertSeqs fam2)
    , "fam3 in Newick-format."  ~: fam3string  ~=? newick (makeDistanceMatrix $ convertSeqs fam3)
    , "fam4 in Newick-format."  ~: fam4string  ~=? newick (makeDistanceMatrix $ convertSeqs fam4)
    , "fam5 in Newick-format."  ~: fam5string  ~=? newick (makeDistanceMatrix $ convertSeqs fam5)
    , "foxp4 in Newick-format." ~: foxp4string ~=? newick (makeDistanceMatrix $ convertSeqs foxp4)
    ] where
    fam1string  = "((((PPRA_Human, PPRA_Mouse), (PPRG_Human, PPRG_Mouse)), PPRB_Zfish), PPRB_Human, PPRB_Mouse);"
    fam2string  = "(((HNFG_Human, HNFG_Mouse), HNF_Fly), (HNFA_Human, HNFA_Mouse), HNFA_Zfish);"
    fam3string  = "(((((ERB_Human, ERB_Mouse), ERB_Chicken), ERB_Zfish), ERR_Fly), ((ERA_Human, ERA_Mouse), ERA_Chicken), ERA_Zfish);"
    fam4string  = "(((4A1_Human, 4A1_Mouse), (4A3_Human, 4A3_Mouse)), 4A2_Human, 4A4_Fly);"
    fam5string  = "((((5A2_Human, 5A2_Mouse), 5A2_Zfish), 5A4_Zfish), (((5A3_Worm, 5B1_Fly), 5A3_Fly), 5A5_Zfish), 5A1_Human);"
    foxp4string = "((((FOXP4_MOUSE, FOXP4_RAT), FOXP4_FROG), FOXP4_HUMAN), FOXP4_COW, FOXP4_DOG);"
-- Sequence lists {{{
-- Simple DNA list.
shortDNAs :: [RawSeq]
shortDNAs =  [("T1", "ACATAA"), ("T2", "AAGTCA"), ("T3", "ACGTGC"), ("T4", "AAGTTC"), ("T5", "ACGTAA")]

-- Six FOXP4 proteins
foxp4 :: [RawSeq]
foxp4 = [("FOXP4_HUMAN" , "EMSPAELLHFQQQQALQVARQFLLQQASGLSSPGNNDSKQSAVQVPVSVAMMSPQMLTPQQMQQILSPPQLQALLQQQQALMLQQLQEYYKKQQEQLHLQLLTQQQAGKPQPKEALGNKQLAFQQQLLQMQQLQQQHLLNLQRQGLVSLQPNQASGPLQTLPQAVCPTDLPQLWKGEGAPAEDSVKQEGLDLTGTAATSFAAPKVSPPLSHHTLPNGQPTRRDSSSHEETSPLYGHGECKWPGCETLCEDLGQFIKHLNTEHALDDRSTAQCRVQMQVVQQLEIQLRLQAMMAHLHMRPSEPKPFSQPVTVSADSFPDGLVHPPTSAAAPVTPLRPPGLGSASLHGGGPARRRSSDKFCSPISSELAQNHEFYKNADVRPPFTYASLIRQAILETPDRQLTLNEIYNWFTRMFAYFRRNTATWKNAVRHNLSLHKCFVRVENVKGAVWTVDEREYQKRRPPKMTGSPTLVKNMISGLSYGALNASYQAALAESSFPLLNSPGMLNSASSLLPLSHDDVGAPVEPLPSNGSSPRLSPQYSHQVQVKEEPAEEDRQPGPLGAPNPSASGPPEDRDLEEELPGEEL"),
         ("FOXP4_COW"   , "EMSPAELLHFQQQQALQVARQFLLQQASGLSSPGNNDSKQSAVQVPVSVAMMSPQMLTPQQMQQILSPPQLQALLQQQQALMIQQLQEYYKKQQEQLHLQLLTQQQAGKQQPKEALGNKQLAFQQQLLQMQQLQQQHLLNLQRQGLVSLQPSQASGPLQTLPQAVCPTDLPQLWKGEGAPAEDSVKQEGLDLTGTATTSFAAPKVSPPLSHHTLPNGQPTRRDSWGLALTAAIVGAGGLLLPGHTKLCSACGEPVRHLNTEHALDDRSTAQCRVQMQVVQQLEIQVWPQAVGAGRGGAPARPKPFSQPVTVSADSFPDGLAHPPTSAAAPVTPLRPPGLGSASLHSGGPARRRSSDKFCSPISSELAQNHEFYKNADVRPPFTYASLIRQAILETPDRQLTLNEIYNWFTRMFAYFRRNTATWKNAVRHNLSLHKCFVRVENVKGAVWTVDEREYQKRRPPKMTGSPTLVKNMISGLSYGTLNASYQAALAESSFPLLNSPGMLNSASSLLPLGHDDAGAPVEPLPSNGSSPRLSPQYSHQVQVKEEPAEEDRRPGPMGPPNPSTAGPPEDRDLEEELPGEEL"),
         ("FOXP4_DOG"   , "EMSPAELLHFQQQQALQVARQLLLQQASGLSSPGNNDSKQSAVQVPVSVAMMSPQMLTPQQMQQILSPPQLQALLQQQQALMLQQLQEYYKKQQEQLHLQLLTQQQAGKQQPKEGVGRADCTFRDALLPTWSSPQQHRRNQQRQGLVSLQPSQASGPLQTLPQAVCPTDLPQLWKGEGAPAEDSVKQEGLDLTGSATTSFAAPKVSPPLSHHTLPNGQPTRRDSSSHEETSPLYGHGECKWPGCETLCEDLGQFIKHLNTEHALDDRSTAQCRVQMQVVQQLEIQLRLQAMMAHLHMRPSEPKPFSQPVTVSADSFPDGLVHPPTSAAAPVTPLRPPGLSSASLHSGGPARRRSSDKFCSPISSELAQNHEFYKNADVRPPFTYASLIRQAILETPDRQLTLNEIYNWFTRMFAYFRRNTATWKNAVRHNLSLHKCFVRVENVKGAVWTVDEREYQKRRPPKMTGSPTLVKNMISGLSYGALNASYQAALAESSFPLLNSPGMLNSASSLLPLSHDEVGAPVEPLPSNGSSPRLSPQYSHQVQVKEEPAEEDRRPGPLGPPNPSTAGPPEDRDLEEELPGEEL"),
         ("FOXP4_RAT"   , "EMSPAELLHFQQQQALQVARQFLLQQASSLNSPGNNDSKQSAVQVPVSVAMMSQQMLTPQQMQQILSPPQLQALLQQQQALMLQQLQEYYKKQQEQLHLQLLSQQQAGKQQPKEALGNKQLAFQQQLLQMQQLQQQHLLNLQRQGLVSLQPSQASGPLQALPQAVCPTDLPQLWKGEGAPAEDGGRQEGLDLASPAATSFASPKVSPPLSHHPLPNGQPTRRDSSSHEETSPLYGHGECKWPGCETLCEDLGQFIKHLNTEHALDDRSTAQCRVQMQVVQQLEIQLRLQAMMAHLHMRPSEPKPFSQPVTVSADPFPDGLVHPPTSAAAPVTPLRPPGLGSASLHGGGPARRRSNDKFCSPISSELAQNHEFYKNADVRPPFTYASLIRQAILETPDRQLTLNEIYNWFTRMFAYFRRNTATWKNAVRHNLSLHKCFVRVENVKGAVWTVDEREYQKRRPPKMTGSPTLVKNMISGLSYGALNASYQAALAESSFPLLSSPGMLNSASSLLPLSQDDMGAPGEPLPSNGSSPRLSPQYSHQIQVKEEPAEEDRRPGPLGAPNPSTVGPPEDRDLEEDLAGEDI"),
         ("FOXP4_MOUSE" , "EMSPAELLHFQQQQALQVARQFLLQQASSLNSPGNNDSKQSAVQVPVSVAMMSQQMLTPQQMQQILSPPQLQALLQQQQALMLQQLQEYYKKQQEQLHLQLLTQQQAGKQQPKEALGNKQLAFQQQLLQMQQLQQQHLLNLQRQGLVSLQPSQASGPLQALPQAVCPTDLPQLWKGEGAPAEDSGRQEGLDLASTAVTSFASPKVSPPLSHHPLPNGQPTRRDSSSHEETSPLYGHGECKWPGCETLCEDLGQFIKHLNTEHALDDRSTAQCRVQMQVVQQLEIQLRLQAMMAHLHMRPSEPKPFSQPVTVSADPFPDGLVHPPTSAAAPVTPLRPPGLGSASLHSGGPARRRSNDKFCSPISSELAQNHEFYKNADVRPPFTYASLIRQAILETPDRQLTLNEIYNWFTRMFAYFRRNTATWKNAVRHNLSLHKCFVRVENVKGAVWTVDEREYQKRRPPKMTGSPTLVKNMISGLSYGALNASYQAALAESSFPLLSNPGMLNSASSLLPLSQEDLGVPGEPLPSNGSSPRLSPQYSHQIQVKEEPAEEDRRPGPLGAPNPSTVGPPEDRDLEEDLGGEDI"),
         ("FOXP4_FROG"  , "ELSPAELLHFQQQQALQMARQLLLQQATGLSSPSSTDNKQPSVQVPVSVAMMSPGMITPQQMQQILSPTQLQAVLQQQQALMLQQLQEYYKKQQEQLHLQLLSQQQAGKQQPKESLGNKQLAFQQQLLQMQQLQQQHLLNLQRQNLVGLQSGQGPLPIQSLPQAVSPSDLQQLLKEMSSNQEESSKQDTVDLTTSITTSFPNSKVSLPTIHPSLPNGQNTRRDSMSHYESSPLYGHGECRWPGCEALCEDMGQFIKHLNTEHALDDRSTAQCRVQMQVVQQLEIQLRLQAMMTHLHMRPSEPKPFSQPNKMSPDTFPDGLPQPPTSATAPITPLRTSVISSSSLPSVGPVRRRIVDKFSTPISSELAQNHEFYKNAEVRPPFTYASLIRQAILDTPDRQLTLNEIYNWFTRMFAYFRRNTATWKNAVRHNLSLHKCFVRVENVKGAVWTVDELEYQKRRPPKMTGSPTLVKNMISGLGYSALNASYQAALAESSFPLLNSPPLHNSSGSVLHGGHDDVTSTGEPGNSNGSSPRLSPQYSQSIHVKEEPAEDDVRPASLSAPTNQTTVLPEDRDIEPETPMEDL")]

-- Below follows protein sequences taken from six lists of nuclear hormone receptors.
-- The data has been simplified, but this is essentially what a researcher would
-- use when studying the evolutionary history among this class of receptors.
-- The six lists represent different subfamilies of receptors.

fam1 :: [RawSeq]
fam1 = [("PPRA_Human" , "VETVTELTEFAKAIPAFANLDLNDQVTLLKYGVYEAIFAMLSSVMNKDVAYGNGFITRDIMEPKFDFAMALELDDSDISLFVAAIICCPGLLNVGIEKMEGILHLQSNHPEQE"),
        ("PPRA_Mouse" , "VETVTELTEFAKAIPGFANLDLNDQVTLLKYGVYEAIFTMLSSLMNKDAYGNGFITREDIMEPKFDFAMALELDDSDISLFVAAIICCPGLLNIGIEKLEGILHLQSNHPEQE"),
        ("PPRB_Zfish" , "VETVRELTEFAKNIPGFVDLFLNDQVTLLKYGVHEAIFAMLPSLMNKDVANGKGFVTREIMEPKFEFAVALELDDSDLALFVAAIILCPGLMNVKVEQIDGIQHLQVHHPEQE"),
        ("PPRB_Human" , "VETVRELTEFAKSIPSFSSLFLNDQVTLLKYGVHEAIFAMLASIVNKDVANGSGFVTRDIIEPKFEFAVALELDDSDLALFIAAIILCPGLMNVPVEAIDTIFHLQANHPEQE"),
        ("PPRB_Mouse" , "VETVRELTEFAKNIPNFSSLFLNDQVTLLKYGVHEAIFAMLASIVNKDLVANGSGFVTDIIEPKFEFAVALELDDSDLALFIAAIILCPGLMNVPVEAIDTIFHLQVNHPEQE"),
        ("PPRG_Human" , "VEAVQEITEYAKSIPGFVNLDLNDQVTLLKYGVHEIIYTMLASLMNKDSEGQGFMTREDFMEPKFEFAVALELDDSDLAIFIAVIILSPGLLNVKIEDIDNLLQLKLNHPEQE"),
        ("PPRG_Mouse" , "VEAVQEITEYAKNIPGFINLDLNDQVTLLKYGVHEIIYTMLASLMNKDSEGQGFMTREDFMEPKFEFAVALELDDSDLAIFIAVIILSPGLLNVKIEDIDNLLQLKLNHPEQE")]

fam2 :: [RawSeq]
fam2 = [("HNFA_Zfish" , "KQQLLVLVEWAKYIPAFCDLPLDDQVALLRAHAGEHLLLGAAKRSMMYLLGNDHIIPRVAVRILDELVLDLQIDDNEYACLKAIVFFDKGLSDPSIKRMYQVDYINDRQYQEF"),
        ("HNFA_Human" , "KEQLLVLVEWAKYIPAFCELPLDDQVALLRAHAGEHLLLGATKRSMVFLLGNDYIVPRVSIRILDELVLELQIDDNEYAYLKAIIFFDKGLSDPGIKRLSQVDYINDRQYQEF"),
        ("HNFA_Mouse" , "KEQLLVLVEWAKYIPAFCELLLDDQVALLRAHAGEHLLLGATKRSMVFLLGNDYIVPRVSIRILDELVLELQIDDNEYACLKAIIFFDKGLSDPGIKRLSQVDYINDRQYQEF"),
        ("HNFG_Human" , "KQQLLVLVEWAKYIPAFCELPLDDQVALLRAHAGEHLLLGATKRSMMYLLGNNYVIHRRVANRVLDELVEIQIDDNEYACLKAIVFFDKGLSDPVIKNMFQVDYINDRQYQEF"),
        ("HNFG_Mouse" , "KQQLLVLVEWAKYIPAFCELPLDDQVALLRAHAGEHLLLGATKRSMMYLLGNHYVIHRRVANRVLDELVEIQIDDNEYACLKAIVFFDKGLSDPVIKNMFQVDYINDRQYQEF"),
        ("HNF_Fly"    , "KQQLLTLVEWAKQIPAFNELQLDDQVALLRAHAGEHLLLGLSRRSMHLLLSNNCVITRIGARIIDELVTDVGIDDTEFACIKALVFFDKGLNEPHIKSLHQIDYISDRQYQEF")]

fam3 :: [RawSeq]
fam3 = [("ERB_Human"   , "DKELVHMISWAKKIPGFVELSLFDQVRLLESCWMEVLMMGLMWRSIDHIFAPDLVLDRGILEIFDMLLAELKLQHKEYLCVKAMILLNQDADSSRLAHLNAVWVIAKSGIKEN"),
        ("ERB_Mouse"   , "DKELVHMIGWAKKIPGFVELSLLDQVRLLESCWMEVLMVGLMWRSIDHIFAPDLVLDRGILEIFGMLLAELKLQHKEYLCVKAMILLNQEAESSRLTHLNAVWVISKSGIKEN"),
        ("ERB_Zfish"   , "DKELVLMISWAKKIPGFVELTLSDQVHLLECCWLDILMLGLMWRSVDHIFTPDLKLNRGIMEIFDMLLAELKLQREEYVCLKAMILLNEDVESRGVLNLDSVWIISRTGLKEN"),
        ("ERA_Human"   , "DRELVHMINWAKRVPGFVDLTLHDQVHLLECAWLEILMIGLVWRSMEHLFAPNLLLDRGMVEIFDMLLAMMNLQGEEFVCLKSIILLNKSLEEKDIHRVDKIHLMAKAGLKEN"),
        ("ERA_Mouse"   , "DRELVHMINWAKRVPGFGDLNLHDQVHLLECAWLEILMIGLVWRSMEHLFAPNLLLDRGMVEIFDMLLAMMNLQGEEFVCLKSIILLNKSLEEKDIHRVDKIHLMAKAGLKEN"),
        ("ERA_Zfish"   , "DKELVHMIAWAKKVPGFQDLSLHDQVQLLESSWLEVLMIGLIWRSIHSIFAQDLILDRGMAEIFDMLLASLKLKLEEFVCLKAIILINEPLMDNFVQCMDNIYCISKSGAKEN"),
        ("ERR_Fly"     , "DKELVSVIGWAKQIPGFIDLPLNDQMKLLQVSWAEILTLQLTFRSLPFCFATDVWMDEYTEFYYHCVQIRISPRREEYYLLKALLLANILLDDQSLRAFDTIDVVYLLRHIRD"),
        ("ERA_Chicken" , "DRELVHMINWAKRVPGFVDLTLHDQVHLLECAWLEILMIGLVWRSMEHLFAPNLLLDRGMVEIFDMLLAMMNLQGEEFVCLKSIILLNKSLEERDIHRVDKIHLMAKSGLKEN"),
        ("ERB_Chicken" , "DKELVHMIGWAKKIPGFIDLSLYDQVRLLESCWMEVLMIGLMWRSIDHIFAPDLVLDRGILEIFDMLLAELKLQHKEYLCVKAMILLNSPEEPESLHHLNVVWVIAKSGIKEN")]

fam4 :: [RawSeq]
fam4 = [("4A1_Human" , "SGSLEVIRKWAEKIPGFAELSPADQDLLLESAFLELFILRLAYRSKPGIFCSGLVLHRDWIDSILAFSRSLLVDVPAFACLSALVLITHGLQEPRVEELNRIEHVAAVAGQQD"),
        ("4A1_Mouse" , "SGSLDVIRKWAEKIPGFIELCPGDQDLLLESAFLELFILRLAYRSKPGIFCSGLVLHQDWIDNILAFSRSLGVDVPAFACLSALVLITHGLQDPRVEELNRIEHMATVAGQQD"),
        ("4A2_Human" , "TGSMEIIRGWAEKIPGFADLPKADQDLLFESAFLELFVLRLAYRSNPVIFCNGVVLHREWIDSIVEFSSNMNIDISAFSCIAALAMVTHGLKEPKVEELNKIDHVTFNNGQQD"),
        ("4A3_Human" , "TASIDVSRSWAERIPGFTDLPKEDQTLLIESAFLELFVLRLSIRSNTAVFCNGLVLHREWLDSIKDFSLSLNLDIQALACLSALSMITHGLKEPKVEELNKIDHQSKGQALQD"),
        ("4A3_Mouse" , "TASIDVSRSWAEKIPGFTDLPKEDQTLLIESAFLELFVLRLSIRSNTAVFCNGLVLHREWLDSIKDFSLSLNLDIQALACLSALSMITHGLKEPKVEELTKIDHQRKGQAQQD"),
        ("4A4_Fly"   , "TSSVDVIKQFAEKIPGYFDLLPEDQELLFQSASLELFVLRLAYRARIDIFCNGTVLHREWLNDIMEFSRNLEIDISAFACLCALTLITHGLREPKVEQLMKIDHVTYNAEQQD")]

fam5 :: [RawSeq]
fam5 = [("5A1_Human" , "DQTFISIVDWARRCMVFKELEVADQMTLLQNCWSELLVFDHIYRQVQHLLVTGQEVELSLVLRAQELVLALQLDRQEFVCLKFIILFSKFLNNHIVKDAEKADYTLCHYPQEG"),
        ("5A2_Zfish" , "DQTLFSIVEWARSSIFFRELKVDDQMKLLQNCWSELLILDHVFRQVMHLLVTGQQVDYNLLSHAQELVSSLQLDQREFVCLKFLVLFSKNLENFHVESVEQVDYVMCNYPQEN"),
        ("5A2_Human" , "DQTLFSIVEWARSSIFFRELKVDDQMKLLQNCWSELLILDHIYRQVVHFLVTGQQVDYNLMSHAQELVASLQFDQREFVCLKFLVLFSKNLENFQVEGVEQVDYTMCNYPQEN"),
        ("5A2_Mouse" , "DQTLFSIVEWARSSIFFRELKVDDQMKLLQNCWSELLILDHIYRQVAHFLVTGEHVDYNLLSLAQELVVSLQFDQREFVCLKFLVLFSKNLENLQVEGVEQVDYTVCNYPQDN"),
        ("5A3_Worm"  , "EENLKDIVIWAKNDQLFSKLSLDDQMILLQTSWTTVHIVDITNAMVHGKMSNGDEVPVTFVSSWNDVVINMGFTNFDYCAFRFLALFDPAVSTARLQSWEVRLEIFEQIRPST"),
        ("5A3_Fly"   , "DQNLFSQVDWARNTVFFKDLKVDDQMKLLQHSWSDMLVLDHLHHRIHNQLNNGQVFNLQPGDYFNELQNDLKFDMGDYVCMKFLILLNRGIVNRKVSEGDNVDYTLTCYPRDP"),
        ("5A4_Zfish" , "DQTLFSIVEWARSCVFFKELKVGDQMRLLHNCWSELLLLDHICRQVHHLLITGQEVELSMVERGQDLSRLLQVDSREMACLKFLILFNKLLENPQVESVEQVEYTLFSYPEDC"),
        ("5A4_Zfish" , "DQTLFSIVEWARSCIFFKELKVGDQMKLLHNCWSELLVLDYVARQLHHLLITGQEVELGMIQRGQELVQELQLDRRETACLKYLILFNKLLENQPVESVEQVEYTLCAYPQDS"),
        ("5A5_Zfish" , "DQTLFGLVEWARNCELFKELKVDDQMVLLQSCWSELLVLDHLCRQVAYCLITGQQIEASLVSRAQDLVTSLQLDREEFVCLKYLVLFNKSVQNRRVEQTERVDHTMQTHPQEG"),
        ("5B1_Fly"   , "DHRLYKIVKWCKSLPLFKNISIDDQICLLINSWCELLLFSCCFRSIDTKMSQGRKITLTCIERMLNLTDRLRVDRYEYVAMKVIVLLQTELQEAVVRECEKAAYTLAHYPLEG")]
-- }}}
-- Distance matrices {{{
-- This is the sample matrix found on the Wikipedia Neighbor Joining page.
wikiMatrix :: [[DistanceTriplet]]
wikiMatrix = [[("A","A", 0),("A","B",7),("A","C",11),("A","D",14)]
             ,[("B","A", 7),("B","B",0),("B","C", 6),("B","D", 9)]
             ,[("C","A",11),("C","B",6),("C","C", 0),("C","D", 7)]
             ,[("D","A",14),("D","B",9),("D","C", 7),("D","D", 0)]]
-- }}}
