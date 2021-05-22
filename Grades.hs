grade :: Int -> Int -> Int
grade a b 
	| (a<0 || a>100) || (b<0 || b>20) = -1
	| (c>47 && a<=47) = 47
	| (c<50 && c>47 && a>47) = 50
	| otherwise = c
	where c = (div (8*a) 10)+b
