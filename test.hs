{-
    Japanese study aid: known kanji and readable words stats.

    Checks a list of known kanji against the list of jouyou kanji and
    provides informations regarding known and unknown ones.
    Also provides info about which words among the most frequently
    used ones contain known kanji (and so are readable).
-}

import Data.List
import Data.List.Split
import Data.Maybe
--import qualified Data.HashMap.Lazy as HashMap
import qualified Data.Set as Set
import System.Console.ANSI

main :: IO ()
main = do

    let word_limit = 100000
    
    -- Load the jouyou kanji list
    jouyou <- readFile "jouyou.txt"
    let jouyou_set = Set.fromList $ lines jouyou
    
    -- Load the known kanji list
    known <- readFile "known.txt"
    let known_kanji_set = Set.fromList [[head x] | x <- lines known, length x > 1]

    -- Load the known kana (and other typography symbols) list
    known_kana <- readFile "known_kana.txt"
    let known_kana_set = Set.fromList [c | line <- lines known_kana, c <- words line]

    -- Read the list of the most frequent words
    frequent <- readFile "most frequent words.txt"
    let frequent_list_lines = lines frequent
    let frequent_list_length = length frequent_list_lines

    -- Limit the most frequent words list according to the word_limit setting
    let max_words = if word_limit < frequent_list_length then word_limit else frequent_list_length
    let frequent_list = take max_words $ frequent_list_lines

    let known_set                     = Set.union known_kanji_set known_kana_set
    let known_jouyou_set              = Set.filter (\k -> Set.member k jouyou_set) known_kanji_set
    let kanji_seen_duplicates_list    = [kanji | word <- frequent_list, kanji <- word, not $ Set.member [kanji] known_kana_set]
    let kanji_seen_set                = Set.fromList $ kanji_seen_duplicates_list
    let total_kanji_seen              = Set.size kanji_seen_set
    let unknown_kanji_duplicates_list = [kanji | kanji <- kanji_seen_duplicates_list, not $ Set.member [kanji] known_set]

    -- check how often each kanji is present in words (might be better suited for a HashMap)
    let unknown_kanji_frequency_list  = reverse $ sort $ map (\l -> (length l, head l)) $ group $ sort unknown_kanji_duplicates_list

    let unknown_kanji_set             = Set.fromList unknown_kanji_duplicates_list
    let unreadable_word_list          = [w | w <- frequent_list, k <- w, not $ Set.member [k] known_set]
    let total_readable_words          = max_words - (length unreadable_word_list)
    let total_kanji_recognized        = total_kanji_seen - Set.size unknown_kanji_set
    let recognized_percentage         = (100 * total_kanji_recognized) `div` total_kanji_seen
    let jouyou_known_percentage       = (100 * Set.size known_jouyou_set) `div` Set.size jouyou_set
    let readable_words_percentage     = (100 * total_readable_words) `div` max_words

    -- Output data (unknown, non-jouyou kanji marked in red)
    setSGR [SetColor Foreground Vivid White]

    putStrLn "Unknown kanji:"
    mapM_ (\k ->
        if Set.member [k] jouyou_set
        then do
            setSGR [SetColor Foreground Vivid White]
            putStr [k]
        else do 
            setSGR [SetColor Foreground Vivid Red]
            putStr [k]) $ Set.toList unknown_kanji_set

    setSGR [SetColor Foreground Vivid White]

    putStrLn   ""
    putStrLn $ "Kanji known:           " ++ show (Set.size known_kanji_set) ++ " (" ++ show jouyou_known_percentage ++ "% of jouyou kanji)"
    putStrLn $ "Kanji recognized:      " ++ show total_kanji_recognized ++ "/" ++ show total_kanji_seen ++ " (" ++ show recognized_percentage ++ "%)"
    putStrLn $ "Readable words:        " ++ show total_readable_words ++ "/" ++ show max_words ++ " (" ++ show readable_words_percentage ++ "%)"

    putStrLn   "Best kanji to learn next:"
    putStr   $ unlines $ map (\t ->
                                unwords $ map (\n ->
                                    [snd $ unknown_kanji_frequency_list !! n] ++ " seen in " ++ show (fst $ unknown_kanji_frequency_list !! n) ++ " words    ") [t,t+10..t+20]) [0..10]

    putStr   $ "Most common unreadable words:\n" ++ unlines (take 10 unreadable_word_list)

    --let m = HashMap.fromList [("foo", "bar")]

    --putStrLn $ show $ fromJust $ HashMap.lookup "foo" m
