{-
    Japanese study aid: known kanji and readable words stats.

    Checks a list of known kanji against the list of jouyou kanji and
    provides informations regarding known and unknown ones.
    Also provides info about which words among the most frequently
    used ones contain known kanji (and so are readable).
-}

import Data.List
import Data.List.Split
import System.Console.ANSI

main :: IO ()
main = do

    -- Starts to run crappily when set to tens of thousands
    let word_limit = 1000
    
    -- Load the jouyou kanji list
    jouyou <- readFile "jouyou.txt"
    let jouyou_list = lines jouyou
    
    -- Load the known kanji list
    known <- readFile "known.txt"
    let known_kanji_list = [[head x] | x <- lines known, length x > 1]

    -- Load the known kana (and other typography symbols) list
    known_kana <- readFile "known_kana.txt"
    let known_kana_list = [c | line <- lines known_kana, c <- words line]

    -- Read the list of the most frequent words
    frequent <- readFile "most frequent words.txt"
    let frequent_list_lines = lines frequent
    let frequent_list_length = length frequent_list_lines

    -- Limit the most frequent words list according to the word_limit setting
    let max_words = if word_limit < frequent_list_length then word_limit else frequent_list_length
    let frequent_list = take max_words $ frequent_list_lines

    let known_list                    = known_kanji_list ++ known_kana_list
    let known_jouyou_list             = [k | k <- known_kanji_list, elem k jouyou_list]
    let kanji_seen_duplicates_list    = [kanji | word <- frequent_list, kanji <- word, not $ elem [kanji] known_kana_list]
    let kanji_seen_list               = nub kanji_seen_duplicates_list
    let kanji_seen_list_length        = length kanji_seen_list
    let unknown_kanji_duplicates_list = [kanji | kanji <- kanji_seen_duplicates_list, not $ elem [kanji] known_list]

    -- check how often each kanji is present in words
    let unknown_kanji_frequency_list  = reverse $ sort $ map (\l -> (length l, head l)) $ group $ sort unknown_kanji_duplicates_list

    let unknown_kanji_list            = nub unknown_kanji_duplicates_list
    let unreadable_word_list          = [w | w <- frequent_list, k <- w, not $ elem [k] known_list]
    let total_readable_words          = max_words - (length unreadable_word_list)
    let total_kanji_recognized        = kanji_seen_list_length - length unknown_kanji_list
    let recognized_percentage         = (100 * total_kanji_recognized) `div` kanji_seen_list_length
    let jouyou_known_percentage       = (100 * length known_jouyou_list) `div` length jouyou_list
    let readable_words_percentage     = (100 * total_readable_words) `div` max_words

    -- Output data (unknown, non-jouyou kanji marked in red)
    setSGR [SetColor Foreground Vivid White]

    putStrLn "Unknown kanji:"
    mapM_ (\k ->
        if elem k jouyou_list
        then do
            setSGR [SetColor Foreground Vivid White]
            putStr k
        else do 
            setSGR [SetColor Foreground Vivid Red]
            putStr k) [[k] | k <- unknown_kanji_list]

    setSGR [SetColor Foreground Vivid White]

    putStrLn   ""
    putStrLn $ "Kanji known:           " ++ show (length known_kanji_list) ++ " (" ++ show jouyou_known_percentage ++ "% of jouyou kanji)"
    putStrLn $ "Kanji recognized:      " ++ show total_kanji_recognized ++ "/" ++ show kanji_seen_list_length ++ " (" ++ show recognized_percentage ++ "%)"
    putStrLn $ "Readable words:        " ++ show total_readable_words ++ "/" ++ show max_words ++ " (" ++ show readable_words_percentage ++ "%)"

    putStrLn   "Best kanji to learn next:"
    putStr   $ unlines $ map (\t ->
                                unwords $ map (\n ->
                                    [snd $ unknown_kanji_frequency_list !! n] ++ " seen in " ++ show (fst $ unknown_kanji_frequency_list !! n) ++ " words    ") [t,t+10..t+20]) [0..10]

    putStr   $ "Most common unreadable words:\n" ++ unlines (take 10 unreadable_word_list)
