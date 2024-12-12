﻿module AdventOfCode.D12.TestCases

let testShort = "RRRRIICCFF
RRRRIICCCF
VVRRRCCFFF
VVRCCCJFFF
VVVVCJJCFE
VVIVCCJJEE
VVIIICJJEE
MIIIIIJJEE
MIIISIJEEE
MMMISSJEEE"

let testLong = "XXXXXUUUNNNNNNNNNNNNNNNNQQQQQQQQQJJJJJJJJJJJJJJJJCCCAAAAAAAAAAAAOVVVVVVVVVVVVVVVVVVLLLLLLLLKKKKGGGGGGGGGGGGGGGGGGGKKKKKKKKKKKKKKKKKKKKKKKKKK
XXXUXXUUUUNNNNNNNNNDNNDGQQQQQQQQQJJJJJJJJJJJJJJJJJJCCCAAAAAAAAAYVVZVVVVVVVVVVVVVVVVVLLLLLLLLKKKGGGGGGGGGGGGGGGGGGGKKKKKKKKKKKKJKKKKKKKKKKKKK
XUUUUUUUUUENNNNNNNNNNNDCCCCCCCCQQJJEJJJJJJJJJJJJJJCCAAAAAAAAAAAZZZZVVZVVVVVVVVVVVVVVLLLLLLLLKGGGGGGGGGGGGGGGGGGGGKKKKKKKKKKKKAKKKKKKKKKKKKKK
UUUUUUUUUUUYNNNNNNNNNNNCCCCCCCCQJJJJJJJJJJJJJJJJJJCAAAAAAAAAAAAZZZVVVVVVVVVVVVVVVVVVLLLLLLLLLLZGGGGGGGGGGLLGGGGGGKKKKKKKKKKAAAAKKKKKKKKKKKKG
UUUUUUUUUKKNNNNNNNNNNZZCCCCCCCCJJJJJJJJJJJDDJJJJJJCAAAAAAAAAAAAHZZVVVVVVVVVVVVVFVVMVVLLLLLLLLLMAAAGGGGLLGLLGGGGGGGGGKKKKKKKAAAAKKKKKKKKKKKKG
UUUUUUUUUKUANNNNNNNNNZZCCCCCCCCQJJJJJJJJJDDDDJJJJJCJJAAAAAAAAAAHZZVVVVVVVVVVVVVVVVMMLLLLLLLGLLMAAAAGGGLLGLLLLLLLGGGGKKKKKKKAAAAKKKKKKKKKKKKG
UUUUUUUUUUUUUNNNNNNNNZZCCCCCCCCQQQQJJDDDDDDDDDJJJJJJAAAAAAAAAAAZZZVVVVVVVVVVVVVVVVMMLLLLLLGGGMMAALGGGLLLLLLLLLLLGGGKRKKKKKKAPPPPPKKKKKKKKKKG
UUUUUUUUUUUUNNNNNNNNNZZCCCCCCCCCCCCCCCCCDDDDDDJJJJJJAAAAAAAAZZZZZZZVVVVVVVVVVVVVVVMMMLLLLLGGGMAAALLLLLLLLLLLLLLLKKKKKKKKKKKPPPPPKKKKKKKKKGGG
UUUUUUUUUUUZNYNNSNNNNZZCCCCCCCCCCCCCCCCCDDDDDDEJJJJJAAAAAAAAAZZZZZZVVVVVVVVVVVVVVVMMMLLLLLGMMMMMALJLLLLLLLLLLLLLKKKKKKKOKPKPPPPKKKKKKGGKGGGG
UUUUUUUUUUUZNSNZZZZZZZZCCCCCCCCCCCCCCCCCDDDDDDJJJJJAAAAAAAQAAAZZZZZZZMFMMMMMMMMMMMMMMMLLLGGMMMMMMMLLLLLLLLLLLLLLKKKKKKKKPPPPPPPPKKPKKGGKGGGG
UUUUUUUUUUMSSSZZZZZZZZZCCCCCCCCCCCCCCCCCDDDDDJJJJJJAAAAAAAQZZZZZZZZZZFFMFMMMMMMMMFFMMLLLLGAMMMMMMMMMLLLLLLLLLLLLLKKKKKKPPPPPPPPPKKPKKGGGGGGG
UUUUUUUUUMMSSSZZZZZZZZZCCCCCCCCCCCCCCCCCDDDJJJJJJJJJAAJAAQQZZZZZZZZZZFFFFMMMMMMMMFIAMMAAAAAMMMMMMMMLLLLLLLLLLLLLLKFKKKKEPPPPPPPPPKPKGGGGGGGG
OOUUUUUUSSSSSSZZZZZZZZZCCCCCCCCCCCCCCCCCDDDJJJJJJJJJJJJAAQQNNNZZZZZZZFFFFFFFFFFFFFFAAAAAAAAMMMMMMMMXLLLLLLLLLLLLKKUUUUUPPPPPPPPPPPPPPPGGGGCC
OUUUUOUUUPSSSSZZZZZZZZZZZZZZZQQCCCCCCCCDDSDDCJJJJJJJJAAAAAQNZZZZZZZZZFFFFFFFHFFFFFFFAAAAAAAAMMMMMMCCCLCLLLLLLLLLLTUUUUUPPPPPPPPPPPPPPPPGGGCC
OOUGGGGUUSSSSZZZZZZZZZZXXXXJJJJJJCCCCCCDDSSCCJJJJJJJJAAAAANNNNZZZZZBPPFFFFFFFFFFFFFFAAAMAAAAAMMMMMMCCCCCLLLLLLLLLMMUUUPPPPPPPPPPPPPPPPPGGGCC
OGGGGGGLLLLSSZZZZZZZZZZXXXJJJJJJJCCCCCCSSSSSJJJJJJJJJJJAAANNNNNZZZNPPPPFFGPPFFFFFFFAAAAMAAMMAMMMMMMCCCCCCOOOLLLLLLMMUUUPPPPPPPPPPZPPPPCCCCCC
OGZZGGGLLLLLSZZZSSSXXXXXXXXXJJJJJCCCCCCSSSSWJJJJJJJJJJPJANNNNNZZZNNPPPPPFPPPPPFFFVHHAAMMMMMMMMMCMCICCCCCCCCOOLTOOLMMMUUPPPPPPPZPPZPCCICCCCCP
OGGGGGGGGLLSSZZZSSSXXXXXXXXXJJJJJCCCCCCSSSSSJJJJJJJJJJJJNNNNNZZZZZNNPPPPPPPPPPPVVVVVMMMMMMMMMMCCCCCCCCCCCOOOOOOOOOMMUUUUPPPPPZZZZZZCCCCCCCCP
SGGGGGGGGLSSSZZZSXXXXXXXXXXJJJJJJCCCCCCJSSSRJJJJJJJNJJJNNNNNNNZZZNNPPPPPPPPPPPPVVVVVMVMMMMMMMCCWCCCCCCCCCCCOOOOOOOOOUUUUPPPPPNNZZZCCCCCCCCCP
SGGGGGGGSSSSSZZZSUXXXXXXXJJJJIJJJCCCCCCJWWWWJJJJJJJJLJJNNNNNNNNNNNNNPPPPPPPPPPPVVVVVVVMMMMMCCCCCCCCCCCCCCCCCCOOOOOOOUUUPPPPPPNNNNCCCCCCCCCPP
SSSGGGGGGGVSSSSSUUXXXXXXXJJJIIJJJCCCCCCWWWWWJJJJJJJJJJNNFNNNNNNNNPPPPPPPPPPPPPVVVVVVVMMMMMMMRRRRCCCCCCCCCCCCCOOOOUOOUUUPUTNNNNNNNCCCCCCCPCPP
SSSSZGGGGGRYSSSRXXXXXXXXXXJJJJJJJJJJJJWWWWWWWJJAJJJJJNNFFNNNNNNNNPPPPPPPPPPPPPVVVVVVVMMMMMMMMRRRCCCCCCCCCCCCCOOOOUUUUUUUUTNNNNNNNNCCCCCCPPPP
SSSZZGHGGGRRJSRRGXXXXXXXXXXJJSSJJJJJJJWWWWWWWJJAJJJJJJFFNNNNNNNNNNPPPPPPPPPPPPPVVVVVVMMMMMMMRRRRRCCCCCCCCCCCCOOOUUUUUUUUUTTTTTNNNNNCCCCPPPPP
SSQQZZGGGGRRRRRRRXXXXXXXXRXRRJJJJJJJJWWWWWWWWWJJJJJJJJJFFFFFFNNANNNPPPPPPPPPVVVVVVVVVMMMMMRRRRRRRCLLCLLLLLCCCCOOOUUUUUUUTTTTTTNNPPNCCPPPPPPP
QQQQQZZGGGRRRRRRRXXXXXRRRRRRRRJJJJJJWWWWWWWWWWWWWJJJJFFFFFFFFAAAAAPPPPPPPPPVVVVVVVVVVVVDMMMRRRRRRRLLLLLLLLCLCLOOUUUUUUUUUUUUUUUPPPPPPPPPPPPP
QQQQZZQQZRRRRRRRRRRRRRRRRRRRRRRJJJJJSWWWWWWWWWWWWWJJJFFFFFFFFAAAAAPPPPPPPPVVVVVVVVVVVVDDMDRRRRRRRRXXXXLLLLLLLLUOUUUUUUUUUUUUUUUPXPPPPPPPPPPP
QQQQQQQQQRRRRRRRRRRRRRRRRRRRRRRRJVJSSWWWWWWWWWWWWWFFFFFFFSFAAAAAAAPAPPPPPPVVVVVVVVVVDVDDDDRRRRRRRRLXLLLLLLLLLLUUUUUUUUUUUUUUUUUUXXPPPPPPPPPP
QQQQQQQQQRRRRRRPRRRRRRRRRRRRRRRRVVWWWZWWWWWWWWWWWWJFFFFFFFAAAAAAAAAAVPPPPPVVVVVVVVVVDDDDDDHRRRRRRELLLLLLLLLLLSSUUUUUUKUUUUUUUUKUXPPPPPPPPPPP
QQQQQDQDGRSPRPPPPRRRRRRRRRRRRRRRRVVVWWWWWWWWFFJJJJJJJJJFFAAAAAAAAAAAAAPPVVVVVVVVVVVDDDDDDRRRRRRRRRRRLLLLLLLLLLSUUUUUKKKUUKKKUUKUUPPPPPPPPPPP
QDDQDDQDGGPPPPPPPRRRRRRRRRRRRRRRRVVWWWWWWWWWDFFJJFFFFJJFFAAAAAAAAAAAAAAPVVVDDVVVVVDDDDDDDRRRRRRRRRRRLLLLLLLLLSSSUUSUUKKKKKKKKKKUYYPPPPPPPPPP
QDDDDDDDGPPPPPPPPRRRRRRRRRRRRRRRRVVVVWWWWWQQFFFJFFFFFFFFFAAAAAAAAAAAAABBHDDDDVVDVDDDDDDDDRFRRRRRRRRLLLLLLLLLLLSSSSSSKKKKKKKXXDYYYYYPPPPPPPPP
DDDDDDDGGGPWPPPPPPPPRRRRRRRRRRRRRRVVVWWQQWQQQFFJFFFFFFFFAAAAAAAAAAAAAUBBHBBBDDDDDDDDDDDDDDRRRRRRRRRLLLLLLLLLLSSSSSSSKKKKKKKKXDYYYYYPYPPPPPPP
DDDDDDDDDGPWPPPPPPPPPRRRRRRRRRRRRRVRWWWWQQQQQFFFFFFFFFZFAAAAAAAAAAAAABBBBBBBBBBDDDDDDDDDDDDDYRRRRRRRLLLLLLLJLLSSSSSSKKKKKKKKXDDDYYYYYYYPPPKP
DDDDDDDDDDDDPPPPPPPPPRRRRRRRRRRRRRRRRSSQQQFFFFFFFFKKFFFHHHAAAAAAAAAAABBBBBBBBBBBDDDDDDDDDDDDRRRRRRLLLLLLLLSSSSSSSSSUKKKKKKKKDDDDYYYYYYYPPPPY
DDDDDDDDDDAAPPPPPPPPPRRRRRRRRRRRRRRRSSQQQEEGFFFFHHFFFFHHHHAAAAAAAAABBBBBBBBBBBBBDDDDDDDDDDDDRRRRRRRLLLHHLLLSSSSSSSSSKKKKKKGGDDDDDDYYYYYPPPYY
DDDDDDDDDDAAAPPPPPPPPPRRRNNRRRRRRRRSSQQQQEEEFFFHHHHHFHHHHHHHAAAABBBBBBBBBBBBBBBBBDDDDDDDDDRRRRRRRRRLLLHHHLSSSSSSSSSSKKKKKKDDDDDDDDYYYYYYYYYY
DDDDDDDDDDDAAPPPPPPPPRRRNNNRRRRRRRQQSQQQQEEEFFFHHHHHHHHHHAAAABBBBBBSSBBBBBBBBBBBBDDDDDJJJJDDRRRRRRRHLLHLLLLSSSSSSSSKKKKKKKUDDCDDDDYYYYYYYYYY
DDDDDDDDDDAAAPPPPPIIIRRRRNRRRRRRRRQQQQQQQEEEFFFHHHHHHHHHHHHAABBBBBBBBYBTBBBBBKBRBDDDDJJMMJJDRRSSRRRHLHHHHHLQQQSSSSSSKKKKKKDDDDDDDYYYYYYYYYYY
DDDDDDDDDDDAAPPPPPPQIIIRRRRRRRRRRQQQQQQNNQEHHFFFHHHHHHHHHHHHHBBBBBBBYYYBBBBBBBBRRRRDDJJJJJJRRRRRRRHHHHHHJJLQQQSQQSSSSSSKKKDDDDDDDYYYYYYYYYYY
DDDDDDDDNNDQAPPPPPQQQIIRQRRRRRRRRRQQQQQQNNSHHFHHHHHHHHHHHHHHHBDBBBYYYYYBBBBBBRRRRTDDJJJJJJJTRRRRRRHHHHHHHQQQQQQQQSSSSSSSDDDDDDDDDYYYYYYYYYYY
DDDDDDDDNNNQAPPNPPPQQIIQQHRRRRRRRRQQQQQQQNSHHHHHHHHHHHHHHHHEHBDDGBBGWWWBBBBBBBRRRTTTTJJJTTTTRHHHHHHHHHHHHHQQQQQQQSSSSSSSSSDDDDDDYYYYYMYYMYYY
DDDDDDDNNNNQQQPPQQQQQQQQQHHHHHXXXXXQQQQQQNNNHHXXHHHHHHHRRHRRRRDDGBBGWWWWBBBWWBRRRTTTTTTTTTTTTHHHHHHHHHHHHQQQQQQQQSJJSSSSSSSDDDDDYYMYMMMMMMYM
DDDIIDDDDNNQQQQPQQQQQQQQQQHHHXXXXXQQQQQQQNNEEEHHHHHHHHHRRRRRRRGGGGGGWEWWWBBWWBWWWWRTTTTTTTTTHHHHHHHHFHHLQQQQQQQQQQJJJSSSSDSDIIDDYYMMMMMMMMYM
DDDDDDDHNNNNQQQQQQQQQQQQHHHHXFXXXXXXXNNNNNNNNHHHHHHHHHHRRRRRRUUGGGGGGGGWWWWWWWWWWWRRTTTTTTTHHHHHHHHFFHGQQQQQQQQQJJJJJJSSSDDDIIYYYYMMMMMMMMMM
QDDDDNNNNNNNNQQQQQQQZZQQQHHXXXXXXXXXXNUNNNNNNNNPNYYHHHRRRRRRRRUGGGGGGGGWWWWWWWWWWWWWTTTTCTTTHHHHHHHHHGGGQQQQQQQJJJJJVJWSWKKKYIYYYYYMMMMMMMMM
QDDQNNNNNNNNNNNQQQQQZZLQHHXXXXXXXXXNNNNNNNNNNNNNNNYHHHRRRRRRRRRGGGGGGGWWWWWWWCWWWWCWTTTTCCTCHHHHHHGGHGGQQQQQQQJJJJJJVWWSWKKKYYYYYYYYMMMMMMMM
QQQQNNNNNNNNNQQQQQQQZZZZZXXXXXXXXXTNNNNNNNNNNNNNNNNNHHRRRRRRRGGGGGGGGGGWWWWCCCWWWCCQQCCCCCCCHVHHHHHGGGGQQQQQQQJJJJJJJWWWWWKKKQYYYYMMMMMMMMMM
QQQNNNNNNNNNNNQQQQQQQZZZZLXUWXXXXXNNNNNNNNNNNNNNNNNKKHRRRRRRRGGGGGGGGGGWWWCCCCCCCCCQQQCCCCCCVVVVVDDGGGQQQQQQQQJJJJJJJWWWWWLKLQQYQQNMMMMMMMMM
QQQQNNNNNNNNNNQQQZZZZZZZZZOWWXWXXWNNWNNNNNNNNNNNKKKKKKARRRRRRGGGGGGGGGGGWWWCCNCCCCCCQCCCCCCCCVVVVDGGGGKOQOKKQJJJJJJJWWWWWWLLLQQQQMMMMMMMMMMM
QQQNNNNNNNNNNNNQZZZZZZZZZZZZWWWWWWNWWWNNNNNNNNNNKKKKKKARRRRKKKKGGGGGGGGGGWWWCCCCCCCCCCCCCCCCCCVDDDDDGGKOOOKKQQJJJJJWWWWWWLLLLMQQQMMMMMMMMMMU
LQQQNNNNNNNNNNQQZZZZZZZZZTTTTWTWWWWWWWNNNNNNNNNNNKKKKKARRRRKKKKGGGGGGGGGWWWUCRCCCCCCCCCCCCCCJDDDDDDGGKKKKKKKKKJJJJJWWWWWWMLLLMQQTMMMMMMMMMMM
LNNNNNNNNNNNNQQQZZZZZZZZTTTTTWTWWWWWWWNNNNNNNNNNKKKKKKARRRRDKKKKKKGGGGGGWGUUCCCCCCCCCCCCCCCUCCDDDDDGDKKKKKKKKKJJJJTTTWWWMMMMMMMMMMMMMMMMMMMM
LLNNNNNNNNNNNNQQZZZZZZZZTTTTTTTTWWWWWWWNNNNNNNNNNKKKKKKGGRRDDKKKKKKKKGGGGGUUUUCCCCCCCCCCCCCCCCDDDDDDDNKKKKKKKKKKJYYTTTTMMTTMMMMMMMMMMMMMMMMM
LLLNTNNNNNTNQNQQCCZZZZZTTTTTTTTTWWWWWWWNNNNNNNNNNKNNKKKGGRRRDDKKKKKKKUGGGUUUUUCCCCCCCCCCCCCCCCDDDDDNNNNNKKKKKKKKBYYTTTTMTTTTTTMMMMMMMMMMMMMM
LLLLTTNNNNTTQQQQQQZZZZTTNTTTTTTWWWWWWNNNNNNNNNNNNGGGGKGGGGGCDKKKKKKKKUGGUUUUUCCCCCCCCCCCCCCCCCDDDDDDNNNNKKKKKKKKKYYTTTTTTTTTMMMMMMMMMMMMMMMM
LLLLLTNNNNTTQTQQTTTTTTTTTTTTTTTWWWWWWWNNNNNNNNNNNNGGGGGGGCCCCCKKKKKKUUUUUUUVUCCCCCCCCCCCLCCCCCDDDDDDDDDDKKKKKKKKYYYTTTTTTTTTNNNMMMMMMMKKMMMM
LLLLLTNNNNTTTTQTTTTTTTTTGTTTTTKKKWKKKWNNNNNNNNNNNNNGGGGGGGCCCCKKKKKKUUUUUUUVVCCCCCCCCCCLLCEECCDDDDDDDDDKKKKKKKYYYYYTTTTTTTTNNNNNKKKKKKKKKKKK
LLLLLTTTNNTTTHBTHTTTTTTTGTTTTKKKKKKKKWNNNNNNNNNNNNNGGGGGGGXCCCCKKKKKUUUUUUZVVCCCCCLCCLLLLLLEDDDDDDDDDDDKKKKKKKYYYYYTTTTTTTTTENNNKKKKKKKKKKKK
LLLLLLLTTTTHHHHHHHTTTTTGGTGKKKKKKKKKKWWNNNNNNNNNNNGGGGGGGGCCCCKKKBBKUVVVVZZVCCCCFFLLLLLALZBDDDDDDDDDDDDKKKKKKKYYYYYTTTTTTTNNNNNNNKKKKKKKKKKK
TTTTTTLTTTHHHHHHHHHTTTTTGGGKKKKKKKKKKWWGNNGNNNNNNNNNGGGGGGCCCCCKKKCCUVVVVVVVVVCFFFFLLLLAZZZZZDDDDDDDKKKKKKKKYYYYYYYYTTTTTTNNNNNNNKKPKKKKKKKK
TTTITTTTTTHHHHHHHHTTTTTTGGKKKKKKKKKKKGGGGGGNNNNNNNNNNGGGGCCCCCCCCCCCUUUVVVVVVVVVVFFLLLLAAZZZZZDVDDDKKKKKKYYYYYYMYYTTTYTTTTTNNNNNNNKKKKKCKKKN
TTIIIIIIIIIIIIIHHHTTTTTTTACKPKKKKKKPKGGGGGGGNNNNNNNNNGGGGCCCCCCCCCCCCUUVVVVVVVVVVVRHHLLLZZZZZZZZDKKKKKKKKYYYYYYMMYYYYYYTTTNNNNNNNNNKKKKKKKNN
TTTIIIIIIIIIIHHHHHHTTTTTAAKKKKKKKKPPPGGGGGGGGGNGGNNNNGGGXCCCCCCCCCCCCCCVVVVVVVVVVVRRRRRLLZZZZZZZDZZZKKMMKMMYMMMMMMYYYYYYYYNNNNNNNNNNNNKKNNNN
TTIIIIIIIIIIIIPHAATTTTTAAAUKKKKKTPPPGLLGGGGGGGGGNNNKKKGXXXKCCCCCCCCCCVVVVVVVVVVRRRRRREEZZZZZZZZZZZZZKKMMMMMMMMMMMMYYYYYYYYNNNNNNNNNNNNNNNNNN
TTIIIIIIIIIIIIIIAAATTTAAAUUUKKTTTTPPGGGGGGGGGGGSKKKKKKKXXXCCCCCCCCCCVVVVVVVVVVVVRRRRRRRZZZZZZZZZZZZZKKMMMMMMMMMMMYYYYYYYYYYNNNNNNNNNNNNNNNNN
IIIIIIIIEEEIIIIIAAAAAAAAUOUUKKJTTTPPGGGGGGGGGGGSKKKKKKXXXXCCUCCCCCCCVVVVVVVVVVVVRRRRRRRZZZZZZZZZZZZZKMMMMMMMMMMMMMYYYYYYYYYYNANNNNNNNNNNNNNN
EERIIEEEEEBEIIIAAAAAAAUUUUUUUUJJTTTJJJGGGGGGGGGGGKKKKKXXXXXXCCCCCCCCVVVVVVVVVVRRRRRRFRRZEZZZZZZZZZZZKKKMMMMMMMMMMMYYYYYYYZZZZZZZNNNNNNNNNNXX
EEEIIEEEEEEEEEEEEAAAAUUUUWWNUUJJTTTTJGGGGGGGGGGGGKKKKXXQXXXQCCCCCQCCVVVVVVVVVRJJRRRRRRRZEEEZZZZZZZZKKKMMMMMMMMMMMMYYYYYTTZZZZZZZNNNNANTNNXXX
EEECEEEEEYEEEEEAEAAAAUULLWWWWLTTTTTTTGGGGGGGGGGGGKKKKKKQXQQQCCQCQQCCVVVVVVVVRRRRRRRRRRRRERRZZZZUZKKKKKHMMMMMMMMMPMMTTTTTTZZZZZZZAANNATTXXXXR
EEECEEEEEEEEEEJAAAAAAAUOOOOWWLLTTTTVGGGGGGGGGGGKKKKKKKKQQQQQQQQQQQCCCVVVVVKKRRRRRRRRRRRRRRIPBZZUZKKKKHHMMMMMMMMMMTTTTTTTTZZZZZZZAANNAARRRRRR
EEEEEEEEEEEEEEAAAAAAAAUOOOOWWLLLQUTVGGGGGGGGGGKKKKKKKKKQQQQQQQQQQQQCVVVVVVKKKRRRRRRRRRRRRIIPPPPPPKKKKHHHHMMMMMMMMMTTTTTTTZZZZZZZAAANARRRRRRR
EEEEEEEEEEEEEEEVAAAAAAPOOOOLLLLQQQLQGGGKGKKKKGTTKKKKKKKQQQQQQQQQKQQCCVVVVVKKKRRRRRRRRRRRRRPPPPPLPKKHHHHHHMMMMMMMMMTTTTTTTTOAZZZZAAAAAARRRRRR
EEIEEEEEEEEEEEEEAAAAAAOOOLLLLQQQQQQQGGQKKKKKKEETTKKKKKQQQQQQQKKKKQQQVVVVVKKKKRRRRRRRRRRRRPPPPPPPPHHHHHHHHMMMMMMOMOTTTTTTTTHAZZZZAHAAARRRRRRR
CEIEEEEEEEEEEEEEAAAAAAALLLLLLQQQQQQQGGQKKKKKKEETEKKWWKQQQQQQKKKKKKQKKVVKKKKKKKKKKKKKRRRRRPPPPPPPPPHHHHHHHHMMMMMOOOOTTTTTTCCCCCCCCCAAARRRRRRR
BBBBEEEEEEEEEEAAAAAAAAAALLLQQQQQQQQQQQQQQQQKKEETEEWWWQQQQQQKKKKKKMKKKVVKKKKKKKKKKKKKRBRRHVHHPPPPPPHHHHHHHMMMMMMMMMTTTTTTTCCCCCCCCCAAAARRRRRR
BBBEEEWEEEEEEEEEAAAAAAAALLLQQQQQQQQQQQQQQKKKKEEEEEWWWWWQQQQKKKKKKKKKKKKKKKKKKKKKKKKKRBKRHHHHPPPPPPRHHHHHRQMIMMMMMQTTTTTTTCCCCCCCCCHRRRRRRRRR
BBBBEEEEEEEEAAAEAAAAAAAALLQQQQQQQQQQQQQQKKEEEEEEEEWWWWWWWKKKKKKKKKKKKKNKKKKKKKKKKKKKKKKRHHHHPPPPPPPHHHHHQQVMMMMPPTTTTTTTTCCCCCCCCCHRRRRRRRRR
BBBBEEHEEEEEAAAAAAAAAAALLLSZQQQQQQQQQQQQQQEEEEEENEEWWWWWKKKKKKKKKKKKKNNKNNCKKKKCKKKKKKKHHHHHPPPPPPPHHHHHQQQLMMMMPPTTTTTTTCCCCCCCCCOORRRRRRRR
BBHHHHHHEAAEAAAAAAAAAAAALLSZQQQQQQQQQQQQQQEEEEEEEEEEHWWWKKKKKKKKKKKKKKNKNNCCCCCCCCKKKKKKHHHHHHPPPPDHHSSHQQLLLLMPPPTTTTTTTCCCCCCCCCOORRRRRRRR
BBHHHHAHEAAAAAAAAAAAAAAAASSZQQQQQQQQQQQQQUEUEEEEEEEHHWWWWEKKKKKKKKKKKKNNNNCCCCCCCCKKKKKKHHHNNHPPPPPHHSSQQQLLMMMPPPTTTTTTTCCCCCCCCCOOOOORRRRF
BBHHHHAAAAAAASSAAAAAAAAAYSSSSQQQQQQQQQQQUUUUUEEEBEHHEEEEEEEKKKKKKKKKKKKKNNCCCCCCCCKKKKHHHHHNNNXPPPPHHSSQQLLMMMMPMPPTTTTTTOOOOHHOOOOOOOOORRRR
BBHHHHHHAAAASSSAAAAAAAYYYYSSSSSQQQQQQQQQQUUBBBBBBBHHHHEEEEEKKKKKKKKKKKKKNCCCCCCCCCCKKHHHHHNNNXXRRPXHSSSQQLMMMMMMMPPTTTTSZZOOOOOOOOOOOOOOORDD
BBHHHHHJJAAAASSSSSSSYAAYYYYSSSQQQQQQQRQQQUUUUUBBBBBHHHEEEEEEEEEKKKKOOZCZNCCCCCCCCCCKKKHHHHNNNXXXXXXXSSSMSSMMMMMMMPPTTTTTZZOOOOOOCOOOOOOOOOOD
HJHHHHJJJJAAASSSSSSSYYYYYYYYYYQQQRQQQRRRUUUUBBBBBBBBHHEEEEEEEXXXXXXXZZZZZCCCCCCCCCCCCHHHNNNNXXXXXXXSSSSSSMMMMMMMMWPTTTTZZZZOOOOOCOOOOOOOOOCO
HHHHHJJJJJSSSSSSSSSSYYYYYYYYYRRQRRRRRRRRUUUUBBBBBBBBBHHHEEEEEXXXXXXXZZZZZCCCCCCCCCCCCCHHHNNNRXXXXXSSSSSSSSMMMMMMWWPTTTTZMZZZOOCCCCOOOOOOOOOO
HHHHJJJJJJJSSSSSSSSSSYYYYYYYYRRRRRRRRRRRUUQUUBBBHHBBHHEEEEEEWXXXXXXXZZZZZCCCCCCCCCCCCCCCHPPNNXXXSSSSSSSSSSMMMMMMWWWTTTTTZZZZCCCCCCCCOOOOOUOU
HJJHJJJJJJJMSSSSSSSSSSYYYYYYYYRRRRRRRRRRRUQUUHHBHHBHHHHEEEWWWXXXXXXXZZZZZCCCCCCCCCCCCCYYCPPPPPSSSSSSSSSSSSMMMMMMWWMTTTTTZCCCCCCCCCCCOOUUUUUU
JJJJJJJJJJJJJSSSSSSSSSSYYYYYYYRRRRRRRRRRQQQQHHHHHHHHHHHEEEWWXXXXXXXXZZZZZCCCCCCCCCCCCCCCCPPPPPSSSSSSSSSSSMMMMMMMMMMMTTTTZCCCCCCCCCCCQCUUUUUU
JJJJJJJJJJJJJSSSSSSSIISYYYYYYYRRRRRRRRRRRPQQQHHHHHHHHHHEEHHWXXXXXXXXZZZZZKKNCCCCCCCCCCPPPPPPPPSSSSSSSSSSMMMMMMMMMMMMMTTTTCCCCCCCCCCCCCUUUUUU
JJJJJJJJJJJJJSSSSIIIIIIYYYYRRRRRRRRRRRRRRPPQPPHHHHHHHHHHEEHWXXXXXXXXZZZZKKKKKKCCCCCCCOPPPPPPPPPPSSSSSSSSMMMMMMMMMMMMMTTTTCCCCCCCCCCCCUUUUUUU
JJJJJJJJJJJPPPIIIIIIIIYYIIIRRRRRRRRRRRRCCPPPPHHHHHHHHHHHHHHHXXXXXXXXZZKKKKWKKKKCCCCCPPPPPPPPPPPASSSSSSSSSKMMMMMMMMMMMTTTTTCMCCCCCCCCCUUUUUUU
JJJJJJJJJJJQIIIAAAAAAAAIIEEEREBRRRRRRRRCCPCPCYHHHHHHHHHHHHHHXXXZQZZZZZKCCKKKKKKKKKCQQQQQQQQPPPPSSSSSSSSSMMMMMMMMUMMHHTTTTTTMMCHCCCFCUUUUUUUU
JJJJJJAAAAAAAAAAAAAAAAAAAAAARRBRRRRRRCCCCCCCCYHHHHHZHHHHHHHSXXXZZZZZZZKCCKKKKKKKKKWQQQQQQQQPPPPPSSSSSSSSSSMMMMMMMHHHHTTTTTTMMCHCCCUUUUUUUUUY
QJJJJJAAAAAAAAAAAAAAAAAAAAAABBBBBBBRRRRCCCCCBHHHZZHZZHHHHHMSXXXSZZZZZCCCKKKKKSKKWKWQQQQQQQQPCCPSSSSSSSSSSSMMHHHMHHHHHTTHTTTMHHHCFCUUUUUUUUUY
QJJJJJAAAAAAAAAAAAAAAAAAAAAABBBBBBBBRRRRCCCEBEHHZZZZHHHHHHMSXXXSSZZZCCCCCKKKKKKWWWWQQQQQQQQGCCCCSSSSSSSSSSHHHHHHHHHSSSTHTVVMHHUUFFFUUUUUYUUY
AAAAAAAAAAAAAAAAAAAAAAAACCBBBBBBBBBBRRRCCCCEEEEZZZZZHHHHHHMSXXXSSSZSSCCCCKKKKKKWWWWQQQQQQQQCCCCCCCESSSSSSSSHHHHHHHHSSSHHHHVVUUUUUFUUUUUYYUYY
AAAAAAAAAAAAAAAAAAAAAAAACCBBBBBBBBBBBRRRDCSBEBEEEZZZZZHHHHMSXXXSSSSSSSSCCKOKWKWWWWWQQQQQQQQQQQQCCEEEESVSSEHHHHHHHHHSSHHHHHVVUUUUUUUUUUUUYYYY
AAAAAAAAAAAAAAAAAAAAAAAACBBBBBBBBBBBBBRRDDBBBBBEEEXXZZHMMMMMSPPPSSSSWWWKKKKKWWWWWWQQQQQQQQQQQQQCEEEEEEEEEEHHHPHHHHHHHHHHHVVUUUUUUUUUUUUUYYYY
AAAAAAAAGGGGGGAAAAAAAAAABBBBBBBBBBTBTBDDDDBBBBBBBEHHHHHMPPPPPPPPSSSWWWWKWKKWWWWWWQQQQQQQQQQQQQQEEEEEEEEEEEHHHHHHHHHHHHHHHVVVVUUUUUUUUUUUYYYY
AAAAAAAAGGGGGGAAAAAAAAAABBBBBBBTTBTTTMDDDDBBBBBBBBHHHHMMPPPPPPPPPPPPPPWWWWWWWWWWWQQQQQQQQQQQQQQEEEEEEEEEEEEHHHHHHHHHHHHHVVVVVUUUUUUUUUHHYYYY
GGGGGGGGGGGGGGAAAAAAAAAAZBBBBBBTTBTTTTTDDDBRBBBBBBHHHMMMPPPPPPPPPPPPPPWWWWWWWWWCWQQQQQQQQQQQQQQPPPPEPPPEEEEXHHHHHHHUHVVHVVVVVVUUUUUUHHHHHHHY
GGGGGGGGGGGGGGAAAZZZZZZZZBBBTTTTTBTTTTTDDDDBBBBBBBHHHMMJPPPPPPPPPPPPPPKWWKKWWWCCCQQQQQQQQQQQQQQPPPPPPXXXXXEXXIXXXHUUVVVVVVVVVVUUUUUUUHHHHHHH
GGGGGGGGGGGGAAAAAZZZZZZZZBBBTTTTTTTTTTTTTDDBBBBBBBHHHHMJPPPPPPPPKKKKKKKWKKKKWWCCCQQQQQQQQQQQQQQPPPPPPXXXXXXXXXXXUUUUVVVVJJVVVVVVVUVUUHHHHHHH
GGGGGGGGGGGGAAAAAAAZZZZZZZRRRTTTTTTTTTTTLBBBBBBBBBBBMMMJPPPPPPPPKKKKKKKKKKCCCCCCCQQQQQQQQQQQQQQQQQQPPXXXXXXXXXXXXUUUUUVJJJJJJVJVVVVVVHHHHHHH
YYGGGGGGGGGGAAAAAAAZZZZZZZZRRTTTTTTTTTQTLBBBBBBBBBBMMMMJPPPPPPPPPKKKKKKKKKCCCCCCCQQQQQQQQBBQQQQQQQQPPXXXXXXXXXYXXUUUUJVJJJJJJJJVVVVVHHHHHHHH
YYGGHGGGGGGGAAAAAAZZZZZZZRRRTTTTTTTTTTQQBBBBBBBBBBBBBBMJPPPPPPPPPKKKKKKKKKCCCCCCCQQQQQQQQBBQQQQQQQQPPPXXXXXXXXXUUUUUUJPJJJJJJJVVVVVVVHHHHHHH
YYPGHGGGGGGHADDAAAWZZZZZZZZRRTTTTTTTTTQQBBBBBBBBBBBBEEMJPPPPPPPPPKKKKKKKKKCCCCCCCQQQQQQQQBBGGGPPPPPPPPXXXXXXXXXUUUUUUJJJJJJJJJVVVVVHHHHHHHHH
PPPPHHHHGGGHHHMAAAAZZZZZHHHTTTTTTTTTTTQQBBBBBBBBBBOBOEMJJJJJJJKKKKKKKKKKIICCCCCCCCCCCCCBBBBGGGGHPPPPPXXXXXXXXXXUUUUUJJJJJJJJJCJJVVVVHHHHHHHH
PPHHHHHHHGGHMMMMMMMZZZHHHHHTTTTTTTTTTTTTTTBBBBBBBOOOOOWJJJJJJJJJJJJJJKKKIICCCCCCCCCCCCCBBBBGGGGGWLLLLXXXXXXXXXXUUUJJJJJJJJJJTJJJJVUUUHHHHHHH
HHHHHHHHHHHHMMMMMMMMMZHHHHHHTTTTTTTTTTTTTEBBBBBBBBBOOOWJJJJJJJJJJJJCCCCCCCCCCCCCHCCCCCCBBBBGGGSLLLLLLXXXXXXXXXXXUUUUUJHHJJJJJJJJJJUUUUHHHHHH
HHHHHHHHHHHHSMMMMMMMMZHHHHHHHHTTTTTTTTEEEEEEBBBBBBBBOOWJJJJJJJJJJJJCCCCCCCCCCCCCCCCICCVBBBBLLLLKKKKKKKKKKKKKKKUUUUULLLHHJJJJJJJJUUUUUUHHHHHH
HHHHHHHHXHDHSMMMMMMMMMMHHHHUHTTTTTTTTTEEEEEEHHDBBBBDOOLWLLLLJJJJJJJCCCCCCCCCCCCCICCIMHHBBBBKLLLKKKKKKKKKKKKKKKXXULUULLJJJJJJJJJUUUUUUUUHHHHH
HXXXHHHHHHHHMZMMMMMMMMMHUUUUUWTTTTTTTTEEEHEEHDDBBDDDOLLLLLLLJJJJJJJCCCCCCCCCCCCIIIIIIKKBBBBKLLLKKKKKKKKKKKKKKKXILLLLLLLJJJJJJJUUUUUUUUUUUHHH
HHXQXXXXHHQQMMMMMMMMMMUHUUUUUWBTTTTTNBBEHHHHHDDDDDDDLLLLLLLLJJJJJJJCCCCCCCCCCCIIIIIIIKKKKLLLLLLKKKKKKKKKKKKKKKXIILIILLIJJJJJJUUUUUWWWUUUHHHH
HXXXXXNNHHQQQMMMMMMMMUUUUUUUUBBTTBBBBBBEEHHHHDDDDDSDLLLLLLLLJJJJJJJCCCCCCCCCCCIIIIIIIIKKLLLLLLLKKKKKKKKKKKKKKKXIIIIIIIIIJJJJJUUUUUWWWWWWWWWH
XXXXXXXNNNXQCCCCCMMMDUUBUUUUUUBBBBBBBBBEEHHHHHHHSDSDDDDLDLLCJJJJJJJCCCCCCCCCCCJIIIIIIIIKKKKLLLLKKKKKKKKKSUXIIIIIIIIIIIIIJJJJJJUUUWWWWWWWWWWH
XXXXXXXXXXXXCCCCCCCDDUUUUUUUUUBBBBBBBBBBEHHHHHHHSSSSDSSSDDNNJJJJJJJCCCCCCCCCCCJIIIIIIIIIKKKLLLLKKKKKKKKKRSIIIIIIIIIIIIIIIIJJJJUUUWWWWWWWWWHH
XXXXXXXXXXXXCCCCCCCIDUUUUUUUUUUBBBBBBBBBBDDHHHHSSSSSSSSSSDDDNNNNNNNCCCCCCCCCCCJIIIIIIIHIIKLLLLLLSSSSSSSSSSIIIIIIIIIIIIIIIDDJJDDUUUWWWWWWWWHH
XXXXXXXXXXXXOCCCCCCIDUUUUUUUUUUBBBBTTBBBBDDSSHSSSSSSSSSSSSSNNNNNNNNNCCCCCCCCCCJJJJIIIHHHHLLLLLLSSSSSSSSSSSIIIIIIIIIIIIIDIIDJJDDDUUWWWWWWWWWW
XXXXXXXXXXXXOOHHHHHIIIIUUUUUUUUUBBBNTBNBDDSSSSSSSSSSSSSSSPPPNNNNNNNNCCCCCCCCCCJJJHHHHHHHHLLLLLLSSSSSSSSSSSSIIIIIIIIIIIIDDIDDDDDDDDDWWWWWWWWW
XXXXXXXDXXXXXOHAHHHHIIUUUUUUUUUUBBBNNNNNSSSSSSSSSSSSSSSPPPPPNNNNNNNNCCCCCCCCCCJJJHHHHHHHHLLLLLLSKSSSSSSSSSSIIIIIIIIIIIIIDDDDDDDDDDWWWWWWWWWW
XXKXXXXDDXXOOOHHHHQHWIIUUUUUUUUUUGGGNNNNNNNSSSSSSSSSSQHQPPQQQNNNNNNNCCCCCCCCCCJJJJHHHHHHHHHLLLLLSSSSSSSSSIIIIININIIIIIIDDDDDDDDDDDDWWWWWWWWW
NXXTXXTTTXXOOOHHHHHHHIIUUUUUUUUUUGEGNNNNNNNSSSSSSSSSSQQQQPQQQQQQNNNNCCCCCCCJJJJJCJHHHHHHHLHLLLLLVVSSSSSSSIIIINNNNIIIIIIDDDDDDDDDDDDWWWWWWWWW
XXTTTTTTTTOOOOHHHHHHHIIIUUUWMUUUUJGGNNNNNNPPSSPSSSSSQQQQQQQQQQQQNNNNNNNJJJJJJJJJJJHHHHLLLLLLLLLLVVSSSSSYYYINNNNNNICIIIIDDDDDDDDDDDWWWWWWWWWW
TXTTTTTTTTTOFFFHHHHIIIIIUWWWWWWUWGGGNNNNNNPPSPPPSSSSQQQQQQQQQQQQQNNNNNNJJJJJJJJJJJHHHHLLLHLLLLLLSSSSSSSSSYNNNNNNNCCCCICDDQQQQPDDDDYWWWWWWWWW
TXTTTTTTTTTOIJFFHHHIIIIIWWWWWWWWWWWGWNNNPPPPPPPPSSSQQQQQQQQQQQQQNNNNNNNNJJJJJJJJJJJJHHHHLHHHLLHYYYSSSSSYYYNNNNNCCCCCCCCQQQQQQQDDDDDWWWWWWWWS
TTTTTTTTTTTTIFFFIHIIIIWWWWWWWWWWWWWWWNUPPPPPPPPPSSSQQQQQQQQQQQQNNNRNNNNNJJJJJJJJJJJHHHHHHHHHHLHYYYSSYSSYYYYYNNNCCCCCCCCCQQQQQQDDDIIWWWWWWWWS
TTTTTTTTTTTTIIIIIIIIIWWWWWWWWWWWWWWHHHUPPPPZPPPPPSSQQQQQQQQQQQQNNRRNRRRRFFFFJFFJJHHHHHHHHHHHHHHHYYYYYYYYYYYYYNNCCCCCCCQQQQQQQDDIIIIWJWWWXWWW
TTTTTTTTTTTTIIIIIIIIIWWWWWWWWWWWWWHHHHUPPPPPPPPPPPPPLLQQQQQQQQQQRRRRRRRRFFFFFFFFHHHHHHHHHHHHHHHHHYYYYYYYYYYYAANDCCCCCCCQQQQIIIIIIIIWJWWWXWWW
TTTTTTTTTTTTIIIIIIMMIUWWWWWWWWWWWWHHHHUPPPPPPPPPPPPPQQQQQQQQQQAARRRRRRRRRFFFFFHHHHHHHZZHHHHHHYYYYYYYYYYYYYYYAAAAACCCCCCCCHYYIYIITIIWWWIWWWRQ
TTTTTTTTTTTTTIIIIIMYWWWWWWWWWWWHHHHHHHHPPPPPPPPPPPPPQQQQQQQQQQAQRRRRRRRRRRFFHHHHHHHHZZZZHHHHHYEEYEYYYYYYYYAAAAAACCCCCCCCCCCYIYIITTIIIIIWIIRQ
TTTTTTTTTTTTBIIIIIMMMMWWWWWWWWWWHHHHHHHHZPPPPPPPPPOOQQQQQQQQQQQQTRRRRRRRRRRRHHHHHHRZZZZZZPHHHYEEEEEYYYYYYYYYAAAACCZOOCCCCCCYYYIITIIIIIIIIRRQ
TTTTTXXXBTVVJIIIIIMMMWWWWWWWWWWHHHHHHHHHPPPPPPPPPPPOQQQQQQQQQQQQTTRRRRRRRRHHHHHHHZZZZZZZPPEEEEEEEEYYYYYYYYYAAAAZZZZOOOOCCCCCTTTTTTTTTIIIIIRQ
TTTTXXXXXXJVJJIIIMMMWWWWWWWWWWHHHHHHHNHHPPPPPPPPPPPOOOOOQOQQQQQOOTORRRRRRRRRHHHHZZZZZZZZEEEEEEEEEYYYYYYYYYYYYAAZEZZZOOOOCCCCCTTTTTTTTTTTTTRR
TTTXXXXXJJJVJJIIIIMVVVWWWWWWWHHHHHHHHHHHOPPPPPPPPOPOOOOOOOOQQOOOOOORRRRRRRHHHHHHHZZZZZZZYEEEEEEEYYYYYYYYYYYYYKAEEZZZOOOOOOIITTTTTTTTTTTTTTRR
TXXXXXXYYJJJJIIIIIIVVVWWWWWWWHHHHHHHHHHHOPPOPPOPOOOOOOOOOOQQQOOOOOORRRRRHHHHHHHHHZZZZZZYYEEEEEEEEEIYYYYYYYYYYKKTEEEEOOOOOOITTTTTTTTTTTTRRRRR
TXXXXXXXYJJJJJIJIIIVVVVWWWWWWHHHHHHHHHHOOOOOPOOOOOOOOOOOOOOQOOOOOOORRRRRHRHHHHHHHHHZZZZYYYYEEEEEEEYYYYYYYYYOOKKKEEOOOOOOOOITTTTTTTTTTTTRRRRR
TXTXXXXXYYJJJJJJVVVVVVWWWWWWWWHHHHHHHHHHOOOOOOOOOOOOOOOOOOQQOOOOOOORRRRRRRHHHHHHHHHHHHZYYYEEEEEEEYYYYYYYYYYOOOOOOOOOOOOOOOIITTTTTTTTTTRRRRRR
TTTXXXXXXXJJJJJVVVVVVVVVWWWWWWHFFFFFFFHHHOOOOOOOOOOOOOOOOOOQOOOOOQORRRRRRRHHHHHHHHHHHHHHYYEEEMEEMMYYYYYYYYYYOOOOOOOOOOOOOOIIITTTTTTTTRRRRRRR
TTTXXXXXXZZZJJJVVVVVVVVVWWWWWWFFFFFFFHHLOOOOOOOOOOOOOOOOOOOOOOOOOOORRRRRRRHHHHHHHHHHHHHHYYEEMMMMMYYYYYYYYYYOOOOOOOOOOOQOIIIIITTFFTTRRRRRRRRR"