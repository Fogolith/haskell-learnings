dupli :: [a] -> [a]
dupli [] = []
dupli [x] = [x, x]
dupli (x:ys) = x:x:dupli ys
