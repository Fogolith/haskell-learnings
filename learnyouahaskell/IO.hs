main = do                                -- main do block
    line <- getLine                       -- get impure input
    if null line                         -- check if line is null
        then return()                    -- if so create empty IO action
        else do                          -- else do another IO action (use do to glue together)
          putStrLn $ reverseWords line   -- print fuction
          main                           -- call main again

reverseWords :: String -> String
reverseWords = unwords . map reverse . words




