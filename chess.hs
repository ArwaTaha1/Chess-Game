import Data.List 
type Location = (Char, Int)
data Player = White | Black deriving (Show, Eq)
data Piece = P Location | N Location | K Location | Q Location | R Location | B Location deriving (Show, Eq)
type Board = (Player, [Piece], [Piece])
type List1 = [Location] 


setBoard :: Board 
setBoard = (White,[R ('h',1),N ('g',1),B ('f',1),K ('e',1),Q ('d',1),B ('c',1),N ('b',1),R ('a',1),P ('h',2),P ('g',2),P ('f',2),P ('e',2),P ('d',2),P ('c',2),P ('b',2),P ('a',2)] ,[R ('h',8),N ('g',8),B ('f',8),K ('e',8),Q ('d',8),B ('c',8),N ('b',8),R ('a',8),P ('h',7),P ('g',7),P ('f',7),P ('e',7),P ('d',7),P ('c',7),P ('b',7),P ('a',7)])
changeplayer (White,x,y) = (Black,x,y)

finds ( _ , [], [])  _ _ = " "++" |"
finds (z, [], ( p: t)) x y =if ((helper2 p)==x && (helper35 p)==y) then (helper4 Black (helper3 p)) 
                        else finds (z, [], t) x y 
finds (z, ( p: t), l) x y = if ((helper2 p)==x && (helper35 p)==y) then (helper4 White (helper3 p)) 
                        else finds (z, t, l) x y
					
puts1 _ _ 18 = ["  "]
puts1 p i (-2) = (helper9 i (-2)): puts1 p i (0)
puts1 p i j = (finds p  i j): (puts1 p i (j+2))

helper4 a b  = if a == White then   b ++ "W" ++ "|" 
              else  b ++ "B" ++ "|"
			  
convertToString = unlines . map (concat . addSeparators)
                  where addSeparators (x:xs) = x : "" : xs

pts White = "White"
pts Black = "Black"

ichooser (w,x,y) 9  = [["      Turn:"++ (pts w) ]]
ichooser b 0 = grouping  : ichooser b 1
ichooser b s =  puts1 b s (-2) : ichooser b (s+1) 	

grouping  = ["  ", "  ", "a", "  ", "b", "  ", "c", "  ","d", "  ","e" , "  ","f", "  ","g", "  ","h", "  "] 



visualizeBoard:: Board->String
visualizeBoard b = convertToString (ichooser  b 0)

helper :: [[String]] 
helper = [[" ", "", "a", "", "b", "", "c", "","d", "","e" , "","f", "","g", "","h", ""],
   ["8", "|", "", "|", "", "|", "", "|", "", "|", "", "|", "", "|", "", "|", "", "|"],
   ["7", "|", "", "|", "", "|", "", "|", "", "|", "", "|", "", "|", "", "|", "", "|"],
   ["6", "|", "", "|", "", "|", "", "|", "", "|", "", "|", "", "|", "", "|", "", "|"],
   ["5", "|", "", "|", "", "|", "", "|", "", "|", "", "|", "", "|", "", "|", "", "|"],
   ["4", "|", "", "|", "", "|", "", "|", "", "|", "", "|", "", "|", "", "|", "", "|"],
   ["3", "|", "", "|", "", "|", "", "|", "", "|", "", "|", "", "|", "", "|", "", "|"],
   ["2", "|", "", "|", "", "|", "", "|", "", "|", "", "|", "", "|", "", "|", "", "|"],
   ["1", "|", "", "|", "", "|", "", "|", "", "|", "", "|", "", "|", "", "|", "", "|"]]
 
 
helper9 :: Int -> Int -> String
helper9 x j  |(x==1 && j==(-2)) = "8"
             |(x==2 && j==(-2)) = "7"
             |(x==3 && j==(-2)) = "6"
             |(x==4 && j==(-2)) = "5"
             |(x==5 && j==(-2)) = "4"
             |(x==6 && j==(-2)) = "3"
             |(x==7 && j==(-2)) = "2"
             |(x==8 && j==(-2)) = "1"
             |(x==0 && j==(-2)) = ""
             |otherwise = ""
 
helper2 :: Piece -> Int
helper2 (P (_, n)) = 9-n
helper2 (N (_, n)) = 9-n
helper2 (B (_, n)) = 9-n
helper2 (R (_, n)) = 9-n
helper2 (Q (_, n)) = 9-n
helper2 (K (_, n)) = 9-n
			 
helperRev  x  |x==16 = 'h'
             |x==14 = 'g'
			 |x==12 = 'f'
			 |x==10 = 'e'
			 |x==8 = 'd'
			 |x==6 = 'c'
			 |x==4 = 'b'
			 |x==2 = 'a'
             |otherwise = ' '
			 
helperRev2 x  |x==1 = 8
             |x==2 = 7
			 |x==3 = 6
			 |x==4 = 5
			 |x==5 = 4
			 |x==6 = 3
			 |x==7 = 2
			 |x==8 = 1
             |otherwise = 92	


helper25 :: Char -> String
helper25 x   |x== 'h' = "h"
             |x== 'g' = "g"
			 |x== 'f' = "f"
			 |x== 'e' = "e"
			 |x== 'd' = "d"
			 |x== 'c' = "c"
			 |x== 'b' = "b"
			 |x== 'a' = "a"
			 |otherwise = "zzzz"
			 

			 
helper3 :: Piece -> String
helper3 (P (_, n)) = "P"
helper3 (N (_, n)) = "N"
helper3 (B (_, n)) = "B"
helper3 (R (_, n)) = "R"
helper3 (Q (_, n)) = "Q"
helper3 (K (_, n)) = "K"

helper8 :: Char -> Int
helper8 x    |x=='h' = 16
             |x=='g' = 14
			 |x=='f' = 12
			 |x=='e' = 10
			 |x=='d' = 8
			 |x=='c' = 6
			 |x=='b' = 4
			 |x=='a' = 2
             |otherwise = 92

helper35 :: Piece -> Int
helper35 (P (n, _)) = helper8 n
helper35 (N (n, _)) = helper8 n
helper35 (B (n, _)) = helper8 n
helper35 (R (n, _)) = helper8 n
helper35 (Q (n, _)) = helper8 n
helper35 (K (n, _)) = helper8 n




finderW _ [] = False
finderW p (h:t) = if p==h then True else finderW p t

comp i j x y = if i==x && j==y then True else False 

finderW2 _ [] = False
finderW2 (i,j) ((P(x,y)):t) = if comp i j x y then True else finderW2 (i,j) t
finderW2 (i,j) ((K(x,y)):t) = if comp i j x y then True else finderW2 (i,j) t
finderW2 (i,j) ((Q(x,y)):t) = if comp i j x y then True else finderW2 (i,j) t
finderW2 (i,j) ((N(x,y)):t) = if comp i j x y then True else finderW2 (i,j) t
finderW2 (i,j) ((B(x,y)):t) = if comp i j x y then True else finderW2 (i,j) t
finderW2 (i,j) ((R(x,y)):t) = if comp i j x y then True else finderW2 (i,j) t 

--checkerbecker (i,j) x l l2 = if finderW2 (helperRev (i+2), helperRev2 j) l2 then (helperRev (i+2), helperRev2 j): loop1( (i+2),j) 0 l l2 else ('h',92): loop1( (i+2),j) 0 l l2

loop1 (_,_) 0 _ _ =  []
loop1 (i,j) x l l2 = if finderW2 (helperRev (i+2), helperRev2 j) l || finderW2 (helperRev (i+2), helperRev2 j) l2 then if finderW2 (helperRev (i+2), helperRev2 j) l2 then (helperRev (i+2), helperRev2 j): loop1( (i+2),j) 0 l l2 else ('h',92): loop1( (i+2),j) 0 l l2
 else(helperRev (i+2), helperRev2 j): loop1( (i+2),j) (x-1) l l2

loop2 (_,_) 0 _ _ =  []
loop2 (i,j) x l l2 = if finderW2 (helperRev (i), helperRev2 (j+1)) l || finderW2 (helperRev (i), helperRev2 (j+1)) l2 then if finderW2 (helperRev (i), helperRev2 (j+1)) l2 then (helperRev (i), helperRev2 (j+1)): loop2( (i),(j+1)) 0 l l2 else ('h',92): loop2( (i),(j+1)) 0 l l2
 else(helperRev (i), helperRev2 (j+1)): loop2( (i),(j+1)) (x-1) l l2

loop3 (_,_) 0 _ _ =  []
loop3 (i,j) x l l2 = if finderW2 (helperRev (i-2), helperRev2 (j)) l || finderW2 (helperRev (i-2), helperRev2 (j)) l2 then if finderW2 (helperRev (i-2), helperRev2 (j)) l2 then (helperRev (i-2), helperRev2 (j)): loop3( (i-2),(j)) 0 l l2 else ('h',92): loop3( (i-2),(j)) 0 l l2
 else(helperRev (i-2), helperRev2 (j)): loop3( (i-2),(j)) (x-1) l l2


loop4 (_,_) 0 _ _=  []
loop4 (i,j) x l l2= if finderW2 (helperRev (i), helperRev2 (j-1)) l || finderW2 (helperRev (i), helperRev2 (j-1)) l2 then if finderW2 (helperRev (i), helperRev2 (j-1)) l2 then (helperRev (i), helperRev2 (j-1)): loop4( (i),(j-1)) 0 l l2 else ('h',92): loop4( (i),(j-1)) 0 l l2
 else(helperRev (i), helperRev2 (j-1)): loop4( (i),(j-1)) (x-1) l l2

loop5 (_,_) 0 _ _=  []
loop5 (i,j) x l l2 = if finderW2 (helperRev (i+2), helperRev2 (j+1)) l || finderW2 (helperRev (i+2), helperRev2 (j+1)) l2 then if finderW2 (helperRev (i+2), helperRev2 (j+1)) l2 then (helperRev (i+2), helperRev2 (j+1)): loop5( (i+2),(j+1)) 0 l l2 else ('h',92): loop5( (i+2),(j+1)) 0 l l2
 else(helperRev (i+2), helperRev2 (j+1)): loop5( (i+2),(j+1)) (x-1) l l2

loop6 (_,_) 0 _ _=  []
loop6 (i,j) x l l2 = if finderW2 (helperRev (i-2), helperRev2 (j+1)) l || finderW2 (helperRev (i-2), helperRev2 (j+1)) l2 then if finderW2 (helperRev (i-2), helperRev2 (j+1)) l2 then (helperRev (i-2), helperRev2 (j+1)): loop6( (i-2),(j+1)) 0 l l2 else ('h',92): loop6( (i-2),(j+1)) 0 l l2
 else(helperRev (i-2), helperRev2 (j+1)): loop6( (i-2),(j+1)) (x-1) l l2
 
loop7 (_,_) 0 _ _=  []
loop7 (i,j) x l l2= if finderW2 (helperRev (i+2), helperRev2 (j-1)) l || finderW2 (helperRev (i+2), helperRev2 (j-1)) l2 then if finderW2 (helperRev (i+2), helperRev2 (j-1)) l2 then (helperRev (i+2), helperRev2 (j-1)): loop7( (i+2),(j-1)) 0 l l2 else ('h',92): loop7( (i+2),(j-1)) 0 l l2
 else(helperRev (i+2), helperRev2 (j-1)): loop7( (i+2),(j-1)) (x-1) l l2

loop8 (_,_) 0 _ _=  []
loop8 (i,j) x l l2 = if finderW2 (helperRev (i-2), helperRev2 (j-1)) l || finderW2 (helperRev (i-2), helperRev2 (j-1)) l2 then if finderW2 (helperRev (i-2), helperRev2 (j-1)) l2 then (helperRev (i-2), helperRev2 (j-1)): loop5( (i-2),(j-1)) 0 l l2 else ('h',92): loop8( (i-2),(j-1)) 0 l l2
 else(helperRev (i-2), helperRev2 (j-1)): loop8( (i-2),(j-1)) (x-1) l l2

--merge :: [Location] -> [Location] -> [Location]
merge xs     []     = xs
merge []     ys     = ys
merge (x:xs) (y:ys) = x : y : merge xs ys

combine :: [a] -> [a] -> [[a]]
combine xs ys = [xs,ys]

helperQ (i,j) l l2 = merge (helperB (i,j) l l2)  (helperR (i,j) l l2)

helperB (i,j) l l2 = merge (merge (loop5 (i,j) 8 l l2) (loop6 (i,j) 8 l l2)) (merge (loop7 (i,j) 8 l l2) (loop8 (i,j) 8 l l2))

--helperQ (i,j) =   [loop2 (i,j) 8 , loop1 (i,j) 8 , loop3 (i,j) 8 , loop4 (i,j) 8 , loop5 (i,j) 8 , loop6 (i,j) 8 , loop7 (i,j) 8 , loop8 (i,j) 8]
helperR (i,j) l l2 =  merge (merge (loop2 (i,j) 8 l l2) (loop1 (i,j) 8 l l2)) (merge (loop3 (i,j) 8 l l2) (loop4 (i,j) 8 l l2))

helperP p (i,j) l l2= if j==2 && p==Black then
                      if finderW2 (helperRev (i), helperRev2 (j+1)) l || finderW2 (helperRev (i), helperRev2 (j+1)) l2 
					  then []
					  else if finderW2 (helperRev (i), helperRev2 (j+2)) l || finderW2 (helperRev (i), helperRev2 (j+2)) l2
					  then [(helperRev (i), helperRev2 (j+1)) ,(helperRev (i+2), helperRev2 (j+1))] 
					  else [(helperRev (i), helperRev2 (j+1)) ,(helperRev (i+2), helperRev2 (j+1)),(helperRev (i), helperRev2 (j+2)) ,(helperRev (i+2), helperRev2 (j+2))]
					  else
                      if j==7 && p==White then
                      if finderW2 (helperRev (i), helperRev2 (j-1)) l || finderW2 (helperRev (i), helperRev2 (j-1)) l2 
					  then []
					  else if finderW2 (helperRev (i), helperRev2 (j-2)) l || finderW2 (helperRev (i), helperRev2 (j-2)) l2
					  then [(helperRev (i), helperRev2 (j-1))]
					  else[(helperRev (i), helperRev2 (j-1)),(helperRev (i), helperRev2 (j-2))]					  
				      else
				      if p==White then 
				      if finderW2 (helperRev (i), helperRev2 (j-1)) l || finderW2 (helperRev (i), helperRev2 (j-1)) l2
                      then []
                      else [(helperRev (i), helperRev2 (j-1))]
                      else
                      if p==Black then 
                      if finderW2 (helperRev (i), helperRev2 (j+1)) l || finderW2 (helperRev (i), helperRev2 (j+1)) l2
                      then []
                      else [(helperRev (i), helperRev2 (j+1))]
                      else []
					  
					 

					  
diagP Black (i,j) = [(helperRev (i-2), helperRev2 (j+1)),(helperRev (i+2), helperRev2 (j+1))]
diagP White (i,j) = [(helperRev (i-2), helperRev2 (j-1)),(helperRev (i+2), helperRev2 (j-1))]
 

diag (i,j) l =  if finderW2 (i,j) l then True else False
filter5 _ [] _ _ = []			  
filter5 c ((i,j):t) l l2 = if c==White then
                           if diag (i,j) l2 then (i,j):filter5 c t l l2 else filter5 c t l l2
						   else 
						   if diag (i,j) l then (i,j):filter5 c t l l2 else filter5 c t l l2
					  
					  

helperN (i,j) l l2= [(helperRev (i+2), helperRev2 (j+2)),(helperRev (i-2), helperRev2 (j+2)),(helperRev (i+2), helperRev2 (j-2)),(helperRev (i-2), helperRev2 (j-2)),(helperRev (i-4), helperRev2 (j+1)),(helperRev (i-4), helperRev2 (j-1)),(helperRev (i+4), helperRev2 (j+1)),(helperRev (i+4), helperRev2 (j-1))]

helperK (i,j) l l2= [(helperRev (i+2), helperRev2 j),(helperRev (i+2), helperRev2 (j+1)),(helperRev (i), helperRev2 (j+1)) , (helperRev (i-2), helperRev2 j),(helperRev (i), helperRev2 (j-1)),(helperRev (i-2), helperRev2 (j-1)),(helperRev (i+2), helperRev2 (j-1)),(helperRev (i-2), helperRev2 (j+1))]

filterNk [] _ = []
filterNk ((i,j):t) l = if finderW2 (i,j) l then  filterNk t l else (i,j):filterNk t l



getlist x (P (i, n)) l l2= merge (helperP x (helper35 (P (i, n)),helper2 (P (i, n))) l l2) ( filter5 x (diagP x (helper35 (P (i, n)),helper2 (P (i, n)))) l l2)
getlist x (B (i, n)) l l2= helperB (helper35 (B (i, n)),helper2 (B (i, n))) l l2
getlist x (R (i, n)) l l2= helperR (helper35 (R (i, n)),helper2 (R (i, n))) l l2
getlist x (Q (i, n)) l l2= helperQ (helper35 (Q (i, n)),helper2 (Q (i, n))) l l2
getlist x (N (i, n)) l l2= filterNk (helperN (helper35 (N (i, n)),helper2 (N (i, n))) l l2) l
getlist x (K (i, n)) l l2= filterNk (helperK (helper35 (K (i, n)),helper2 (K (i, n))) l l2) l


filter3 []  = []				
filter3 ((i,j):t) = if i==' ' || j==92 then filter3 t else (i,j):filter3 t

checkscolor  p h = if finderW p h then White else Black 
checkscolor2 p h = if finderW p h then Black else White


suggestMove:: Piece -> Board -> [Location]
suggestMove p (pl,x,y)  = if (checkscolor p x)==White then (filter3 (getlist (checkscolor p x) p x y) )
                          else (filter3 (getlist (checkscolor p x) p y  x)) 

findingLegal _ [] = False
findingLegal (i,j) ((x,y):t) = if i==x && j==y then True else findingLegal (i,j) t

isLegal:: Piece -> Board -> Location -> Bool
isLegal p b l = findingLegal l (suggestMove p b)

finderW3 _ _ []  = []
finderW3 (r,u) (i,j) ((P(x,y)):t) = if comp i j x y || comp r u x y then (P(r,u)):finderW3 (r,u) (i,j) t  else (P(x,y)):finderW3 (r,u) (i,j) t
finderW3 (r,u) (i,j) ((K(x,y)):t) = if comp i j x y || comp r u x y then (K(r,u)):finderW3 (r,u) (i,j) t  else (K(x,y)):finderW3 (r,u) (i,j) t
finderW3 (r,u) (i,j) ((Q(x,y)):t) = if comp i j x y || comp r u x y then (Q(r,u)):finderW3 (r,u) (i,j) t  else (Q(x,y)):finderW3 (r,u) (i,j) t
finderW3 (r,u) (i,j) ((N(x,y)):t) = if comp i j x y || comp r u x y then (N(r,u)):finderW3 (r,u) (i,j) t  else (N(x,y)):finderW3 (r,u) (i,j) t
finderW3 (r,u) (i,j) ((B(x,y)):t) = if comp i j x y || comp r u x y then (B(r,u)):finderW3 (r,u) (i,j) t  else (B(x,y)):finderW3 (r,u) (i,j) t
finderW3 (r,u) (i,j) ((R(x,y)):t) = if comp i j x y || comp r u x y then (R(r,u)):finderW3 (r,u) (i,j) t  else (R(x,y)):finderW3 (r,u) (i,j) t

insertfirst p (r,u) g = (p(r,u):g)

checkybecky p (White,w,b) l = if (checkscolor p w)==White then putter p l w else error "This is White player's turn, Black can't move."
checkybecky p (Black,w,b) l = if (checkscolor2 p b)==Black then putter p l b else error "This is Black player's turn, White can't move."

putter  (P(r,u)) (i,j) l =  (finderW3 (i,j) (r,u) l)
putter  (Q(r,u)) (i,j) l =  (finderW3 (i,j) (r,u) l)
putter  (K(r,u)) (i,j) l =  (finderW3 (i,j) (r,u) l)
putter  (N(r,u)) (i,j) l =  (finderW3 (i,j) (r,u) l)
putter  (R(r,u)) (i,j) l =  (finderW3 (i,j) (r,u) l)
putter  (B(r,u)) (i,j) l =  (finderW3 (i,j) (r,u) l)						  
	
groupie (P(i,j)) (White,w,b) (r,u) = if (checkscolor (P(i,j)) w)==White then(Black ,(checkybecky (P(i,j)) (White,w,b) (r,u)) , (finderW3 (i,j) (r,u) b)) else error "This is White player's turn, Black can't move."
groupie (K(i,j)) (White,w,b) (r,u) = if (checkscolor (K(i,j)) w)==White then(Black ,(checkybecky (K(i,j)) (White,w,b) (r,u)) , (finderW3 (i,j) (r,u) b)) else error "This is White player's turn, Black can't move."
groupie (Q(i,j)) (White,w,b) (r,u) = if (checkscolor (Q(i,j)) w)==White then(Black ,(checkybecky (Q(i,j)) (White,w,b) (r,u)) , (finderW3 (i,j) (r,u) b)) else error "This is White player's turn, Black can't move."
groupie (N(i,j)) (White,w,b) (r,u) = if (checkscolor (N(i,j)) w)==White then(Black ,(checkybecky (N(i,j)) (White,w,b) (r,u)) , (finderW3 (i,j) (r,u) b)) else error "This is White player's turn, Black can't move."
groupie (B(i,j)) (White,w,b) (r,u) = if (checkscolor (B(i,j)) w)==White then(Black ,(checkybecky (B(i,j)) (White,w,b) (r,u)) , (finderW3 (i,j) (r,u) b)) else error "This is White player's turn, Black can't move."
groupie (R(i,j)) (White,w,b) (r,u) = if (checkscolor (R(i,j)) w)==White then(Black ,(checkybecky (R(i,j)) (White,w,b) (r,u)) , (finderW3 (i,j) (r,u) b)) else error "This is White player's turn, Black can't move."

groupie (P(i,j)) (Black,w,b) (r,u) = if (checkscolor2 (P(i,j)) b)==Black then (White , (finderW3 (i,j) (r,u) w),(checkybecky (P(i,j)) (Black,w,b) (r,u)) ) else error "This is Black player's turn, White can't move."
groupie (K(i,j)) (Black,w,b) (r,u) = if (checkscolor2 (K(i,j)) b)==Black then (White , (finderW3 (i,j) (r,u) w),(checkybecky (K(i,j)) (Black,w,b) (r,u)) ) else error "This is Black player's turn, White can't move."
groupie (Q(i,j)) (Black,w,b) (r,u) = if (checkscolor2 (Q(i,j)) b)==Black then (White , (finderW3 (i,j) (r,u) w),(checkybecky (Q(i,j)) (Black,w,b) (r,u)) ) else error "This is Black player's turn, White can't move."
groupie (N(i,j)) (Black,w,b) (r,u) = if (checkscolor2 (N(i,j)) b)==Black then (White , (finderW3 (i,j) (r,u) w),(checkybecky (N(i,j)) (Black,w,b) (r,u)) ) else error "This is Black player's turn, White can't move."
groupie (B(i,j)) (Black,w,b) (r,u) = if (checkscolor2 (B(i,j)) b)==Black then (White , (finderW3 (i,j) (r,u) w),(checkybecky (B(i,j)) (Black,w,b) (r,u)) ) else error "This is Black player's turn, White can't move."
groupie (R(i,j)) (Black,w,b) (r,u) = if (checkscolor2 (R(i,j)) b)==Black then (White , (finderW3 (i,j) (r,u) w),(checkybecky (R(i,j)) (Black,w,b) (r,u)) ) else error "This is Black player's turn, White can't move."


move:: Piece -> Location -> Board -> Board
move p l b = if isLegal p b l then groupie p b l else error ("illegal move for piece  "++ show p)					  
						  
