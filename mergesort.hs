comPair :: (Int,Int) -> (Int,Int) -> (Int,Int)
comPair left right
    | (fst left) < (fst right) = left
    | (fst right) < (fst left) = right
    | (snd left) < (snd right) = left
    | (snd right) < (snd left) = right

mergeTwo :: [(Int,Int)] -> [(Int,Int)] -> [(Int,Int)]
mergeTwo [] ys = ys
mergeTwo xs [] = xs
mergeTwo (x:xs) (y:ys) = 
    if comPair x y == x
        then x:mergeTwo xs (y:ys)
        else y: mergeTwo (x:xs) ys

mergeSort ::[(Int,Int)] -> [(Int,Int)]
mergeSort [] = []
mergeSort [x] = [x]
mergeSort xs = mergeTwo (mergeSort left) (mergeSort right)
    where left = take half xs
          right = drop half xs
          half = (length xs) `div` 2
