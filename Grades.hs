grade :: Int -> Int -> Int
grade a b 
	| (a<0 || a>100) || (b<0 || b>20) = -1
	| (c>47 && a<=47) = 47
	| (c<50 && c>47 && a>47) = 50
	| otherwise = c
	where c = (div (8*a) 10)+b

countDigits :: Int -> Int -> Int
countDigits 0 0 = 0
countDigits a b = countDigits (div a 10) (div b 10) + if (mod a 10)==(mod b 10) then 1 else 0

digits :: Int -> Int -> Int
digits  a b 
	|n == 8 = 1000000
	|n == 7 = 100000
	|n == 6 = 8000
	|n == 5 = 300
	|n == 4 = 20
	|n == 3 = 5
	|n == 2 = 1
	|otherwise = 0
		where n = countDigits a b

