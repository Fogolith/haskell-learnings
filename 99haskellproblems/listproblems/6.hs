myPalindrome :: [a] -> Bool
myPalindrome [] = True
myPalindrome [_] = True
myPalindrome xs = xs == reverse xs
