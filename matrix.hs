import Data.Matrix

m1 = matrix 3 4 $ \(r, c) -> 4 * (r - 1) + c
m2 = fromList 3 4 [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12]
m3 = fromLists [[1, 2, 3, 4], [5, 6, 7, 8], [9, 10, 11, 12]]

main = do
    print m1
    print m2
    print m3

    print $ zero 3 4
    print $ identity 3
    print $ permMatrix 3 1 2

    print $ nrows m1
    print $ ncols m1

    print $ getElem 2 3 m1
    print $ m1 ! (2, 3)
    print $ getRow 2 m1
    print $ getCol 3 m1
