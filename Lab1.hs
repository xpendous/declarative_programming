module Lab1 (elementPosition, everyNth, elementBefore) where

elementPosition :: Eq t => t -> [t] -> Int
elementPosition elt [] = error "ERROR!"
elementPosition elt lst 
			| elt == head lst = 0
			| elt/=head lst = 1 + (elementPosition elt (tail lst))

everyNth :: Int -> [t] -> [t]
everyNth 0 lst = error "ERROR!"
everyNth n lst
	|null (drop (n-1) lst) = []
	|otherwise = x:(everyNth n xs)  
	 where x:xs = drop (n-1) lst

elementBefore :: Eq a => a -> [a] -> Maybe a
elementBefore elt [] = Nothing			  
elementBefore elt lst
			| null (tail lst) = Nothing 
			| head lst == elt = Nothing
			| head (tail lst) == elt = Just(head lst)
			| otherwise =  elementBefore elt (tail lst) 