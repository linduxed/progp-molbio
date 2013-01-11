module Molbio where
import MolSeq
import Profile
import Evol

seqdata = map $ uncurry string2seq

-- Two simple datasets
simple = [("S1", "ACGTACGT"), ("S2", "ACCTACCT"), ("S3", "AGGAAGGT"), ("S4", "ACGAAGGA"), ("S5", "AAGGAAGG")]
sample = [("S1", "ACGTACGT"), ("S2", "CCCTACCT"), ("S3", "AGGAAGGT"), ("S4", "CCGAAGGA"), ("S5", "AAGGAAGG")]
figur =  [("T1", "ACATAA"), ("T2", "AAGTCA"), ("T3", "ACGTGC"), ("T4", "AAGTTC"), ("T5", "ACGTAA")]

-- Snabbtest vid redovisningar
t1 = seqdata figur
snabbtest de xs = de s1 s2 where
    s1 = head xs
    s2 = xs !! 1

t2 = snabbtest seqDistance $ seqdata figur
t3 = snabbtest seqDistance $ seqdata foxp4
t4 = fromMolSeqs $ seqdata foxp4
t5 = profileDistance (fromMolSeqs $ seqdata fam1) (fromMolSeqs $ seqdata fam2) -- Should equal 171.1
t6 = profileDistance (fromMolSeqs $ seqdata fam1) (fromMolSeqs $ seqdata fam3) -- Should equal 176.8
t7 = snabbtest distance $ seqdata foxp4 -- Should equal 0.090
t8 = distanceMatrix $ seqdata foxp4

-- Six FOXP4 proteins
foxp4 = [("FOXP4_HUMAN" , "EMSPAELLHFQQQQALQVARQFLLQQASGLSSPGNNDSKQSAVQVPVSVAMMSPQMLTPQQMQQILSPPQLQALLQQQQALMLQQLQEYYKKQQEQLHLQLLTQQQAGKPQPKEALGNKQLAFQQQLLQMQQLQQQHLLNLQRQGLVSLQPNQASGPLQTLPQAVCPTDLPQLWKGEGAPAEDSVKQEGLDLTGTAATSFAAPKVSPPLSHHTLPNGQPTRRDSSSHEETSPLYGHGECKWPGCETLCEDLGQFIKHLNTEHALDDRSTAQCRVQMQVVQQLEIQLRLQAMMAHLHMRPSEPKPFSQPVTVSADSFPDGLVHPPTSAAAPVTPLRPPGLGSASLHGGGPARRRSSDKFCSPISSELAQNHEFYKNADVRPPFTYASLIRQAILETPDRQLTLNEIYNWFTRMFAYFRRNTATWKNAVRHNLSLHKCFVRVENVKGAVWTVDEREYQKRRPPKMTGSPTLVKNMISGLSYGALNASYQAALAESSFPLLNSPGMLNSASSLLPLSHDDVGAPVEPLPSNGSSPRLSPQYSHQVQVKEEPAEEDRQPGPLGAPNPSASGPPEDRDLEEELPGEEL"),
         ("FOXP4_COW"   , "EMSPAELLHFQQQQALQVARQFLLQQASGLSSPGNNDSKQSAVQVPVSVAMMSPQMLTPQQMQQILSPPQLQALLQQQQALMIQQLQEYYKKQQEQLHLQLLTQQQAGKQQPKEALGNKQLAFQQQLLQMQQLQQQHLLNLQRQGLVSLQPSQASGPLQTLPQAVCPTDLPQLWKGEGAPAEDSVKQEGLDLTGTATTSFAAPKVSPPLSHHTLPNGQPTRRDSWGLALTAAIVGAGGLLLPGHTKLCSACGEPVRHLNTEHALDDRSTAQCRVQMQVVQQLEIQVWPQAVGAGRGGAPARPKPFSQPVTVSADSFPDGLAHPPTSAAAPVTPLRPPGLGSASLHSGGPARRRSSDKFCSPISSELAQNHEFYKNADVRPPFTYASLIRQAILETPDRQLTLNEIYNWFTRMFAYFRRNTATWKNAVRHNLSLHKCFVRVENVKGAVWTVDEREYQKRRPPKMTGSPTLVKNMISGLSYGTLNASYQAALAESSFPLLNSPGMLNSASSLLPLGHDDAGAPVEPLPSNGSSPRLSPQYSHQVQVKEEPAEEDRRPGPMGPPNPSTAGPPEDRDLEEELPGEEL"),
         ("FOXP4_DOG"   , "EMSPAELLHFQQQQALQVARQLLLQQASGLSSPGNNDSKQSAVQVPVSVAMMSPQMLTPQQMQQILSPPQLQALLQQQQALMLQQLQEYYKKQQEQLHLQLLTQQQAGKQQPKEGVGRADCTFRDALLPTWSSPQQHRRNQQRQGLVSLQPSQASGPLQTLPQAVCPTDLPQLWKGEGAPAEDSVKQEGLDLTGSATTSFAAPKVSPPLSHHTLPNGQPTRRDSSSHEETSPLYGHGECKWPGCETLCEDLGQFIKHLNTEHALDDRSTAQCRVQMQVVQQLEIQLRLQAMMAHLHMRPSEPKPFSQPVTVSADSFPDGLVHPPTSAAAPVTPLRPPGLSSASLHSGGPARRRSSDKFCSPISSELAQNHEFYKNADVRPPFTYASLIRQAILETPDRQLTLNEIYNWFTRMFAYFRRNTATWKNAVRHNLSLHKCFVRVENVKGAVWTVDEREYQKRRPPKMTGSPTLVKNMISGLSYGALNASYQAALAESSFPLLNSPGMLNSASSLLPLSHDEVGAPVEPLPSNGSSPRLSPQYSHQVQVKEEPAEEDRRPGPLGPPNPSTAGPPEDRDLEEELPGEEL"),
         ("FOXP4_RAT"   , "EMSPAELLHFQQQQALQVARQFLLQQASSLNSPGNNDSKQSAVQVPVSVAMMSQQMLTPQQMQQILSPPQLQALLQQQQALMLQQLQEYYKKQQEQLHLQLLSQQQAGKQQPKEALGNKQLAFQQQLLQMQQLQQQHLLNLQRQGLVSLQPSQASGPLQALPQAVCPTDLPQLWKGEGAPAEDGGRQEGLDLASPAATSFASPKVSPPLSHHPLPNGQPTRRDSSSHEETSPLYGHGECKWPGCETLCEDLGQFIKHLNTEHALDDRSTAQCRVQMQVVQQLEIQLRLQAMMAHLHMRPSEPKPFSQPVTVSADPFPDGLVHPPTSAAAPVTPLRPPGLGSASLHGGGPARRRSNDKFCSPISSELAQNHEFYKNADVRPPFTYASLIRQAILETPDRQLTLNEIYNWFTRMFAYFRRNTATWKNAVRHNLSLHKCFVRVENVKGAVWTVDEREYQKRRPPKMTGSPTLVKNMISGLSYGALNASYQAALAESSFPLLSSPGMLNSASSLLPLSQDDMGAPGEPLPSNGSSPRLSPQYSHQIQVKEEPAEEDRRPGPLGAPNPSTVGPPEDRDLEEDLAGEDI"),
         ("FOXP4_MOUSE" , "EMSPAELLHFQQQQALQVARQFLLQQASSLNSPGNNDSKQSAVQVPVSVAMMSQQMLTPQQMQQILSPPQLQALLQQQQALMLQQLQEYYKKQQEQLHLQLLTQQQAGKQQPKEALGNKQLAFQQQLLQMQQLQQQHLLNLQRQGLVSLQPSQASGPLQALPQAVCPTDLPQLWKGEGAPAEDSGRQEGLDLASTAVTSFASPKVSPPLSHHPLPNGQPTRRDSSSHEETSPLYGHGECKWPGCETLCEDLGQFIKHLNTEHALDDRSTAQCRVQMQVVQQLEIQLRLQAMMAHLHMRPSEPKPFSQPVTVSADPFPDGLVHPPTSAAAPVTPLRPPGLGSASLHSGGPARRRSNDKFCSPISSELAQNHEFYKNADVRPPFTYASLIRQAILETPDRQLTLNEIYNWFTRMFAYFRRNTATWKNAVRHNLSLHKCFVRVENVKGAVWTVDEREYQKRRPPKMTGSPTLVKNMISGLSYGALNASYQAALAESSFPLLSNPGMLNSASSLLPLSQEDLGVPGEPLPSNGSSPRLSPQYSHQIQVKEEPAEEDRRPGPLGAPNPSTVGPPEDRDLEEDLGGEDI"),
         ("FOXP4_FROG"  , "ELSPAELLHFQQQQALQMARQLLLQQATGLSSPSSTDNKQPSVQVPVSVAMMSPGMITPQQMQQILSPTQLQAVLQQQQALMLQQLQEYYKKQQEQLHLQLLSQQQAGKQQPKESLGNKQLAFQQQLLQMQQLQQQHLLNLQRQNLVGLQSGQGPLPIQSLPQAVSPSDLQQLLKEMSSNQEESSKQDTVDLTTSITTSFPNSKVSLPTIHPSLPNGQNTRRDSMSHYESSPLYGHGECRWPGCEALCEDMGQFIKHLNTEHALDDRSTAQCRVQMQVVQQLEIQLRLQAMMTHLHMRPSEPKPFSQPNKMSPDTFPDGLPQPPTSATAPITPLRTSVISSSSLPSVGPVRRRIVDKFSTPISSELAQNHEFYKNAEVRPPFTYASLIRQAILDTPDRQLTLNEIYNWFTRMFAYFRRNTATWKNAVRHNLSLHKCFVRVENVKGAVWTVDELEYQKRRPPKMTGSPTLVKNMISGLGYSALNASYQAALAESSFPLLNSPPLHNSSGSVLHGGHDDVTSTGEPGNSNGSSPRLSPQYSQSIHVKEEPAEDDVRPASLSAPTNQTTVLPEDRDIEPETPMEDL")]


-- Below follows protein sequences taken from six sets of nuclear hormone receptors.
-- The data has been simplified, but this is essentially what a researcher would
-- use when studying the evolutionary history among this class of receptors.
-- The six sets represents different subfamilies of receptors.

fam1 = [("PPRA_Human" , "VETVTELTEFAKAIPAFANLDLNDQVTLLKYGVYEAIFAMLSSVMNKDVAYGNGFITRDIMEPKFDFAMALELDDSDISLFVAAIICCPGLLNVGIEKMEGILHLQSNHPEQE"),
        ("PPRA_Mouse" , "VETVTELTEFAKAIPGFANLDLNDQVTLLKYGVYEAIFTMLSSLMNKDAYGNGFITREDIMEPKFDFAMALELDDSDISLFVAAIICCPGLLNIGIEKLEGILHLQSNHPEQE"),
        ("PPRB_Zfish" , "VETVRELTEFAKNIPGFVDLFLNDQVTLLKYGVHEAIFAMLPSLMNKDVANGKGFVTREIMEPKFEFAVALELDDSDLALFVAAIILCPGLMNVKVEQIDGIQHLQVHHPEQE"),
        ("PPRB_Human" , "VETVRELTEFAKSIPSFSSLFLNDQVTLLKYGVHEAIFAMLASIVNKDVANGSGFVTRDIIEPKFEFAVALELDDSDLALFIAAIILCPGLMNVPVEAIDTIFHLQANHPEQE"),
        ("PPRB_Mouse" , "VETVRELTEFAKNIPNFSSLFLNDQVTLLKYGVHEAIFAMLASIVNKDLVANGSGFVTDIIEPKFEFAVALELDDSDLALFIAAIILCPGLMNVPVEAIDTIFHLQVNHPEQE"),
        ("PPRG_Human" , "VEAVQEITEYAKSIPGFVNLDLNDQVTLLKYGVHEIIYTMLASLMNKDSEGQGFMTREDFMEPKFEFAVALELDDSDLAIFIAVIILSPGLLNVKIEDIDNLLQLKLNHPEQE"),
        ("PPRG_Mouse" , "VEAVQEITEYAKNIPGFINLDLNDQVTLLKYGVHEIIYTMLASLMNKDSEGQGFMTREDFMEPKFEFAVALELDDSDLAIFIAVIILSPGLLNVKIEDIDNLLQLKLNHPEQE")]

fam2 = [("HNFA_Zfish" , "KQQLLVLVEWAKYIPAFCDLPLDDQVALLRAHAGEHLLLGAAKRSMMYLLGNDHIIPRVAVRILDELVLDLQIDDNEYACLKAIVFFDKGLSDPSIKRMYQVDYINDRQYQEF"),
        ("HNFA_Human" , "KEQLLVLVEWAKYIPAFCELPLDDQVALLRAHAGEHLLLGATKRSMVFLLGNDYIVPRVSIRILDELVLELQIDDNEYAYLKAIIFFDKGLSDPGIKRLSQVDYINDRQYQEF"),
        ("HNFA_Mouse" , "KEQLLVLVEWAKYIPAFCELLLDDQVALLRAHAGEHLLLGATKRSMVFLLGNDYIVPRVSIRILDELVLELQIDDNEYACLKAIIFFDKGLSDPGIKRLSQVDYINDRQYQEF"),
        ("HNFG_Human" , "KQQLLVLVEWAKYIPAFCELPLDDQVALLRAHAGEHLLLGATKRSMMYLLGNNYVIHRRVANRVLDELVEIQIDDNEYACLKAIVFFDKGLSDPVIKNMFQVDYINDRQYQEF"),
        ("HNFG_Mouse" , "KQQLLVLVEWAKYIPAFCELPLDDQVALLRAHAGEHLLLGATKRSMMYLLGNHYVIHRRVANRVLDELVEIQIDDNEYACLKAIVFFDKGLSDPVIKNMFQVDYINDRQYQEF"),
        ("HNF_Fly"    , "KQQLLTLVEWAKQIPAFNELQLDDQVALLRAHAGEHLLLGLSRRSMHLLLSNNCVITRIGARIIDELVTDVGIDDTEFACIKALVFFDKGLNEPHIKSLHQIDYISDRQYQEF")]

fam3 = [("ERB_Human"   , "DKELVHMISWAKKIPGFVELSLFDQVRLLESCWMEVLMMGLMWRSIDHIFAPDLVLDRGILEIFDMLLAELKLQHKEYLCVKAMILLNQDADSSRLAHLNAVWVIAKSGIKEN"),
        ("ERB_Mouse"   , "DKELVHMIGWAKKIPGFVELSLLDQVRLLESCWMEVLMVGLMWRSIDHIFAPDLVLDRGILEIFGMLLAELKLQHKEYLCVKAMILLNQEAESSRLTHLNAVWVISKSGIKEN"),
        ("ERB_Zfish"   , "DKELVLMISWAKKIPGFVELTLSDQVHLLECCWLDILMLGLMWRSVDHIFTPDLKLNRGIMEIFDMLLAELKLQREEYVCLKAMILLNEDVESRGVLNLDSVWIISRTGLKEN"),
        ("ERA_Human"   , "DRELVHMINWAKRVPGFVDLTLHDQVHLLECAWLEILMIGLVWRSMEHLFAPNLLLDRGMVEIFDMLLAMMNLQGEEFVCLKSIILLNKSLEEKDIHRVDKIHLMAKAGLKEN"),
        ("ERA_Mouse"   , "DRELVHMINWAKRVPGFGDLNLHDQVHLLECAWLEILMIGLVWRSMEHLFAPNLLLDRGMVEIFDMLLAMMNLQGEEFVCLKSIILLNKSLEEKDIHRVDKIHLMAKAGLKEN"),
        ("ERA_Zfish"   , "DKELVHMIAWAKKVPGFQDLSLHDQVQLLESSWLEVLMIGLIWRSIHSIFAQDLILDRGMAEIFDMLLASLKLKLEEFVCLKAIILINEPLMDNFVQCMDNIYCISKSGAKEN"),
        ("ERR_Fly"     , "DKELVSVIGWAKQIPGFIDLPLNDQMKLLQVSWAEILTLQLTFRSLPFCFATDVWMDEYTEFYYHCVQIRISPRREEYYLLKALLLANILLDDQSLRAFDTIDVVYLLRHIRD"),
        ("ERA_Chicken" , "DRELVHMINWAKRVPGFVDLTLHDQVHLLECAWLEILMIGLVWRSMEHLFAPNLLLDRGMVEIFDMLLAMMNLQGEEFVCLKSIILLNKSLEERDIHRVDKIHLMAKSGLKEN"),
        ("ERB_Chicken" , "DKELVHMIGWAKKIPGFIDLSLYDQVRLLESCWMEVLMIGLMWRSIDHIFAPDLVLDRGILEIFDMLLAELKLQHKEYLCVKAMILLNSPEEPESLHHLNVVWVIAKSGIKEN")]

fam4 = [("4A1_Human" , "SGSLEVIRKWAEKIPGFAELSPADQDLLLESAFLELFILRLAYRSKPGIFCSGLVLHRDWIDSILAFSRSLLVDVPAFACLSALVLITHGLQEPRVEELNRIEHVAAVAGQQD"),
        ("4A1_Mouse" , "SGSLDVIRKWAEKIPGFIELCPGDQDLLLESAFLELFILRLAYRSKPGIFCSGLVLHQDWIDNILAFSRSLGVDVPAFACLSALVLITHGLQDPRVEELNRIEHMATVAGQQD"),
        ("4A2_Human" , "TGSMEIIRGWAEKIPGFADLPKADQDLLFESAFLELFVLRLAYRSNPVIFCNGVVLHREWIDSIVEFSSNMNIDISAFSCIAALAMVTHGLKEPKVEELNKIDHVTFNNGQQD"),
        ("4A3_Human" , "TASIDVSRSWAERIPGFTDLPKEDQTLLIESAFLELFVLRLSIRSNTAVFCNGLVLHREWLDSIKDFSLSLNLDIQALACLSALSMITHGLKEPKVEELNKIDHQSKGQALQD"),
        ("4A3_Mouse" , "TASIDVSRSWAEKIPGFTDLPKEDQTLLIESAFLELFVLRLSIRSNTAVFCNGLVLHREWLDSIKDFSLSLNLDIQALACLSALSMITHGLKEPKVEELTKIDHQRKGQAQQD"),
        ("4A4_Fly"   , "TSSVDVIKQFAEKIPGYFDLLPEDQELLFQSASLELFVLRLAYRARIDIFCNGTVLHREWLNDIMEFSRNLEIDISAFACLCALTLITHGLREPKVEQLMKIDHVTYNAEQQD")]

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
