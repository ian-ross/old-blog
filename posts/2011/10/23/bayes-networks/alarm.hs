import BayesNet

data Event = B | E | A | J | M deriving (Eq, Ord, Show)

bn = fromList [(B, [], [0.001]),
               (E, [], [0.002]),
               (A, [B, E], [0.95, 0.94, 0.29, 0.001]),
               (J, [A], [0.9, 0.05]),
               (M, [A], [0.7, 0.01])]
