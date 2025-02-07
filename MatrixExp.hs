import Numeric.LinearAlgebra.HMatrix
import Prelude hiding ((<>))
import Data.Time.Clock


--this method split rows of matrix by ch
splitBy :: Char -> String -> [String]
splitBy delimiter = words . map (\c -> if c == delimiter then ' ' else c)

-- this method read matrix from file
readMatFromFile :: FilePath -> IO (Matrix Double)
readMatFromFile filePath = do
    content <- readFile filePath
    let rows = lines content -- divide the content for rows
        matrixList = [map read (splitBy ',' row) | row <- rows] :: [[Double]] -- convert each row for list
        numRows = length matrixList
        numCols = length (head matrixList)
    return $ (numRows><numCols) (concat matrixList) -- build a matrix from list of lists


--this method calculate the norm of matrix (maximum norm that convert each element to plus instead of minus (abs) then sum each row in matrix then return the maximum row
norm_Mat::Matrix Double -> Double
norm_Mat m = maximum $ map sum $ toLists $ cmap abs m

--this method return the n from calculate the Rn of maclorin/lagrang (שארית מקלורן כדי לדעת כמה איטרציות לבצע בטור טיילור בחישוב האקספוננט)
calculate_Rn::Matrix Double -> Double -> Int --get a Matrix + epsilon and return a n (n is the num of iterations that i will give to sum_tor_taylor)
calculate_Rn m epsilon = 
 let norm_M = norm_Mat m
     e_norm_M = exp norm_M
     term = norm_M*e_norm_M
  in check_greater_than_epsilon 1 term epsilon norm_M

--this method check if term big than epsilon if yes return this n if not i increse n and check again (check again by recursive call to same function(this function instead of while loop
check_greater_than_epsilon::Int -> Double -> Double -> Double -> Int
check_greater_than_epsilon n term epsilon norm_M | term < epsilon = n-1
                                                 | otherwise = let
                                                                calcNewTerm = term * ((norm_M)*1/(fromIntegral (n + 1)))
                                                               in check_greater_than_epsilon (n+1) calcNewTerm epsilon norm_M

--this method calculate the tor of taylor from 0 to num_iterations 
sum_tor_taylor:: Matrix Double -> Int -> Matrix Double
sum_tor_taylor m num_iterations = 
 let dim = size $ m --get demonosial of matrix 
     rowSize = rows $ m
     identMat = ident rowSize --build ident matrix
     resultMat = identMat + m
     result = m
  in sum_matrix_for_tor_taylor m num_iterations 2 result resultMat
 

--this method make the sum of matrixes in recursive way,after it finished return the value to sum_tor_taylor
sum_matrix_for_tor_taylor:: Matrix Double -> Int -> Int -> Matrix Double -> Matrix Double -> Matrix Double
sum_matrix_for_tor_taylor m 0 fracVal result resultMat = resultMat
sum_matrix_for_tor_taylor m num_iterations fracVal result resultMat = 
 let myresult = multiMatrix m result
     mulFracResult = myresult*(1/(fromIntegral(fracVal)))
     newresultMat = resultMat + mulFracResult --like numpy the hmatrix can add two matrixes by + operator
  in sum_matrix_for_tor_taylor m (num_iterations-1) (fracVal+1) mulFracResult newresultMat


--this method multiply two matrix (like numpy.dot in python)
multiMatrix::Matrix Double -> Matrix Double -> Matrix Double
multiMatrix m resultMat = resultMat <> m --this like NUMPY.DOT in python 

--this method write the matrix to a file
writeResultToFileSimple :: FilePath -> Matrix Double -> IO ()
writeResultToFileSimple filename result = writeFile filename (show result)

main :: IO ()
main = do
     let input_file = "inv_matrix(1000x1000).txt"
     m <- readMatFromFile input_file
     let epsilon = 1.5e-12
     print("please wait the calcluate in my computer take 1 min to finish!!!")
     tt_start <- getCurrentTime
     let num_iterations = calculate_Rn m epsilon
     let res = sum_tor_taylor m num_iterations
     let firstElementInMyMat = res ! 0 ! 0 --get first element from matrix because haskell is lazy language and if we dont need to print then it will not do the calculate
     print $ firstElementInMyMat
     tt_end <- getCurrentTime
     print ("myWay Time:")
     print (diffUTCTime tt_end tt_start)
     print("---------------")
     tt_start <- getCurrentTime
     let hmatRes = expm m
     let firstElementInHMATRIX = hmatRes ! 0 ! 0 --get first element from matrix because haskell is lazy language and if we dont need to print then it will not do the calculate
     print $ firstElementInHMATRIX
     tt_end <- getCurrentTime
     print ("HMatrixWay Time:")
     print (diffUTCTime tt_end tt_start)
     print("please wait to print the results to TXT files!!!")
     writeResultToFileSimple "MyWayResults_Haskell.txt" res   --write the results for txt file
     writeResultToFileSimple "HmatrixResults_Haskell.txt" hmatRes
     print("the print finished Successfully")



