bin_search :: (Ord a) => [a] -> a -> Maybe Int
bin_search [] _ = Nothing
bin_search [s] n
    | s == n = Just 0
    | otherwise = Nothing
bin_search stack needle 
    | middle_element == needle = Just middle_index
    | middle_element > needle = bin_search top_half needle
    | otherwise = fmap (+middle_index) $ bin_search bottom_half needle
    where middle_element = stack !! middle_index 
          middle_index = size `div` 2
          size = length stack
          top_half = take middle_index stack
          bottom_half = drop middle_index stack
