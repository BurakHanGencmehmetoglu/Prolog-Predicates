module HW1 (
    form,
    constGrid,
    flatten,
    access,
    slice,
    vcat,
    hcat,
    without,
    matches2d
) where







--Create grids with given shape 
--Example runs :
--form [1,2,3] (1, 3) -> [[1,2,3]] 
--form [1,2,3] (3, 1) -> [[1],[2],[3]] 
--form [1..10] (2, 5) -> [[1,2,3,4,5],[6,7,8,9,10]]

form :: [a] -> (Int, Int) -> [[a]] 
form [] (b,c) = []
form a  (b,c) = [take c a] ++ (form (drop c a) (b,c))   


--Create grids with given shape and fill them with constant values
--Example runs : 
--constGrid False (1, 3) ->  [[False,False,False]] 
--constGrid 0 (5, 5) -> [[0,0,0,0,0],[0,0,0,0,0],[0,0,0,0,0],[0,0,0,0,0],[0,0,0,0,0]]

constGrid :: a -> (Int, Int) -> [[a]]
constGrid a (0,_) = []
constGrid a (b,c) = [take c (repeat a)] ++ (constGrid a (b-1,c))


--Reduce grids into one dimensional form
flatten :: [[a]] -> [a]
flatten a = [x | xs <- a,x <- xs] 


--Return given position of the grid
access :: [[a]] -> (Int, Int) -> a
access a (b,c) = a!!b!!c



--Return (i1,i2) rows and (j1,j2) columns of the grid (i1 and j1 inclusive,i2 and j2 exclusive) 
--Example run : 
--grid = [[25,26,27,28,29],[30,31,32,33,34],[35,36,37,38,39],[40,41,42,43,44],[45,46,47,48,49]]
--slice grid (0, 2) (0, 1) -> [[25],[30]] 
--slice grid (1, 3) (1, 3) -> [[31,32],[36,37]]


slice :: [[a]] -> (Int, Int) -> (Int, Int) -> [[a]]
slice a (row1,row2) (column1,column2) = if (row2)==row1 then [] 
                                        else [take (column2-column1) (drop column1 (a!!row1))] ++ slice a (row1+1,row2) (column1,column2)    




--Return concatenated grid

vcat :: [[a]] -> [[a]] -> [[a]]
vcat a b = a++b


--Return horizontally concatenated grid
hcat :: [[a]] -> [[a]] -> [[a]]
hcat a b = zipWith (++) a b




--Similar to slice but this time removes given rows and columns from the grid


without :: [[a]] -> (Int, Int) -> (Int, Int) -> [[a]]
without a (row1,row2) (column1,column2) = finallist where
                                          func (column1,column2) x = (take column1 x) ++ (drop column2 x)
                                          initiallist = if row1==row2 then a else [a!!x | x <- [0..(length (a) -1)], not (elem  x [row1..(row2-1)])]
                                          finallist = if column1==column2 then initiallist else map (func (column1,column2)) initiallist




--Takes grid and pattern and return every position where the pattern was found 
--Example run : 
--grid = [[0,0,0,0],[0,1,1,0],[0,1,1,0],[0,0,0,0]]
--matches2d grid [[1, 1], [1, 1]] -> [(1,1)]
--matches2d grid [[1, 1], [0, 0]] -> [(2,1)] 
--matches2d grid [[1, 0], [0, 0]] -> [(2,2)]


matches2d :: Eq a => [[a]] -> [[a]] -> [(Int,Int)]
matches2d grid pattern = result where
                             func grid1 pattern1 = result1 where 
                                             x = length pattern1
                                             y = length grid1
                                             x1 = length (pattern1!!0)
                                             y1 = length (grid1!!0)
                                             z1 = [(xs-x,xs) | xs <-[x..y]]
                                             z2 =  [(xs-x1,xs) | xs <- [x1..y1]]
                                             result1 = [(fst row,fst column) | column <- z2, row <-z1 , (slice grid1 row column) == pattern1]
                             grid2 = [x | xs <- grid,x <- xs]
                             pattern2 = [x | xs <- pattern,x <- xs]
                             result = if (length (pattern!!0) > length (grid!!0)) || (length pattern2 > length grid2)  then [] else func grid pattern 
                    

