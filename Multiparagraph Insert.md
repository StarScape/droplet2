Inserting a list of paragraphs into the middle of another paragraph:

We'll call the paragraph being inserted into P, and the paragraphs in the list as p1, p2, p3, and so on. The runs inside of each paragraph are referred to as p1_r1, P_r1, p2_r1, p2_r2, etc. The offset into P that the insertion is performed at will be denoted 'x.'

Partition P at x. Call these partitions P1 and P2.

Insert p1 at the end of P1. One run at a time will work okay, though you could also probably safely insert just p1_r1 to prevent creating any unnecessary runs in the case of overlapping formats, then append p1_r2...p1_rn onto P1.runs. In a pure-functional way, of course, or at least a way such that no previous references or DSs are modified.

Insert pn at the beginning of P2. Again, a more efficient way would be to insert the last run in pn (pn_rn) at position 0 in P2, and then prepend the remaining list ( [p1_r1...p(n-1)\_(n1)] ) onto the beginning of the new P2.runs. This will prevent any unnecessary runs being created.

Return P1 and P2.

TODO: what is P1 === P2?
