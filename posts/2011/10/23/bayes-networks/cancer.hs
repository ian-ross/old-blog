import BayesNet

data Event = C | T1 | T2 deriving (Eq, Ord, Show)

bn = fromList [(C, [], [0.01]),
               (T1, [C], [0.9,0.2]),
               (T2, [C], [0.9,0.2])]
               
p_3_20 = prob bn C [(T1,True),(T2,True)]
p_3_21 = prob bn C [(T1,True),(T2,False)]
