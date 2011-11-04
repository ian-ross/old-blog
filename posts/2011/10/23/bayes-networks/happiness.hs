import BayesNet
import Data.Ratio

data Event = S | R | H deriving (Eq, Ord, Show)

bn = fromList [(S, [], [7 % 10]),
               (R, [], [1 % 100]),
               (H, [S, R], [1, 7 % 10, 9 % 10, 1 % 10])]
