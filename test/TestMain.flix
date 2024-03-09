@Test
def test01(): Bool = 1 + 1 == 2

def test02(): Bool = true

def seedParseTest(): Bool \ IO =
    let seed = Random.nextInt32(Random.new());
    seed==parseSeed(toBinaryString(seed))
