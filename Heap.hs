data Heap a= MinHeap [a] | MaxHeap [a] deriving (Show)

swap :: (Ord a) => [a] -> Int -> Int -> [a]
swap list i1 i2 = if (i1 > i2)
					then (swap list i2 i1)
					else ((take i1 list) ++ [(list!!i2)] ++ (take (i2 - i1 - 1) (drop (i1 + 1) list)) ++ [(list!!i1)] ++ (drop (i2+1) list))

heapTop :: Heap a -> Maybe a
heapTop (MinHeap l) = if null l 
						then Nothing 
						else Just (head l)
heapTop (MaxHeap l) = if null l 
						then Nothing 
						else Just (head l)

heapifyUp :: Ord a => Heap a -> Int -> Heap a
heapifyUp (MinHeap l) index = if index == 0 
								then MinHeap l
								else if (l !! index >= (l !! quot (index - 1) 2) )
										then MinHeap l
										else heapifyUp (MinHeap $ (swap l index (quot (index - 1) 2 ) ) )  (quot (index - 1) 2)
heapifyUp (MaxHeap l) index = if index == 0 
								then MaxHeap l
								else if (l !! index <= (l !! quot (index - 1) 2) )
										then MaxHeap l
										else heapifyUp (MaxHeap $ (swap l index (quot (index - 1) 2 ) ) )  (quot (index - 1) 2)

getLeftChild :: Num a => a -> a
getLeftChild index = index * 2 + 1

getRightChild :: Num a => a -> a
getRightChild index = index * 2 + 2

heapifyDown :: Ord a => Heap a -> Int -> Heap a
heapifyDown (MinHeap l) index
	|index >= length l = (MinHeap l)
	|left >= length l = (MinHeap l)
	|right >= length l && (l !! index > l !! left) = (MinHeap (swap l index left))
	|right >= length l = (MinHeap l)
	|(l !! left < l !! right) && (l !! index > l !! left) = (heapifyDown (MinHeap (swap l index left)) left)
	|(l !! left >= l !! right) && (l !! index > l !! right) = (heapifyDown (MinHeap (swap l index right)) right)
	|otherwise = (MinHeap l)
	where (left,right) = ( (getLeftChild index) , (getRightChild index) )

heapifyDown (MaxHeap l) index
	|index >= length l = (MaxHeap l)
	|left >= length l = (MaxHeap l)
	|right >= length l && (l !! index < l !! left) = (MaxHeap (swap l index left))
	|right >= length l = (MaxHeap l)
	|(l !! left > l !! right) && (l !! index < l !! left) = (heapifyDown (MaxHeap (swap l index left)) left)
	|(l !! left <= l !! right) && (l !! index < l !! right) = (heapifyDown (MaxHeap (swap l index right)) right)
	|otherwise = (MaxHeap l)
	where (left,right) = ( (getLeftChild index) , (getRightChild index) )

heapAdd :: Ord a => Heap a -> a -> Heap a
heapAdd (MinHeap l) el = heapifyUp (MinHeap (l++[el])) (fromIntegral (length l))
heapAdd (MaxHeap l) el = heapifyUp (MaxHeap (l++[el])) (fromIntegral (length l))

heapDelete (MinHeap l) 
	|null l = (MinHeap [])
	|otherwise = heapifyDown (MinHeap $ init (swap l 0 (length l - 1))) 0
heapDelete (MaxHeap l) 
	|null l = (MaxHeap [])
	|otherwise = heapifyDown (MaxHeap $ init (swap l 0 (length l - 1))) 0

gen i n heap
	|i == n = heap
	|otherwise = (gen (1 + i) n (heapAdd heap i))

delete i n h
	|i == n = h
	|otherwise = (delete (i+1) n (heapDelete h))