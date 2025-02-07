import numpy as np
from scipy.linalg import expm #used for compare my results with scipy results
from time import time

#calculate the norm of matrix and its do that :norm of infinite is abs all elements then sum all the rows of matrix and get max sum of row 
def norm_Mat(A):
    return np.max(np.sum(np.abs(A), axis=1)) #using norm of maximum 

#this method calculate the שארית of malorin/lagrang (the n is the num of iteration should tor taylor do)
def calculate_Rn(A,epsilon):
    if np.isscalar(A): #check if number is scalar and not matrix or vector
        return 0
    norm_M = norm_Mat(A)
    e_norm_M = np.exp(norm_M)  
    n=0
    term = e_norm_M * norm_M
    while(term>epsilon):  #while the Rn small than epsilon continue to add to n 
        n=n+1
        term = term*(norm_M*(1/(n+1)))
    return n 

#implement method for calculate multiply between matrixes 
def multiMatrix(A,oldResult):
    result = np.dot(A,oldResult)  #multiply between two matrixes (old Result is A^n-1 * A (instead of calculate every time from start)))
    return result

#return the result multiply of two matrix
def cal_exponent_mat(A,result):
    multiplyMat = multiMatrix(A,result).astype(np.float64)
    return multiplyMat.astype(np.float64)

#sum the tor of taylor
############ the code work for real numbers not for complex. it work for complex just for easily change np.float64 to np.complex128
def sum_tor(A,num_iterations):
    if np.isscalar(A): #i check if the matrix is number then i make a normal exponent
        return np.exp(A)
    dim = np.shape(A)[0] #get dim of matrix 
    if(np.shape(A)[0]!=np.shape(A)[1]): #check if the dim of matrix not equal in rows and cols 
        print("to calculate exponential of matrix you need matrix nXn")
        return
    I=np.eye(dim,dim,dtype=np.float64)  #create identy matrix (מטריצת יחידה)
    resultMat = I
    resultMat+=A #add I + A (numpy know to sum them)
    result = A
    for iter in range(2,num_iterations+1): #loop for sum until n (n I get from calculate_Rn func)
        result = cal_exponent_mat(A,result)
        result = result*(1/iter)
        resultMat+= result
    return resultMat    

#this function read a file txt
def read_Mat_TXT():
    with open('inv_matrix(1000x1000).txt', 'r') as file:
        content = file.read()
    rows = content.split('\n')
    matrix = [list(map(float, row.split(','))) for row in rows if row]
    matrix_np = np.array(matrix)
    return matrix_np.astype(np.float64)

#this function write results to file 
def write_result_to_file(filename, result):
    with open(filename, 'w') as file:
        file.write(str(result))


matrix = read_Mat_TXT()

epsilon = 1.5e-12
t = time()
scipyResult = expm(matrix)
t = time() - t
print("scipyTime",t)
print("---------------------")
print("please wait the calcluate in my computer take 1 min to finish!!!")
t = time()
num_iterations = calculate_Rn(matrix,epsilon)
myWayResult = sum_tor(matrix,num_iterations)
t = time() - t
print("myWay Time",t)

#I save the results for files 
write_result_to_file("ScipyResults.txt",scipyResult)
write_result_to_file("MyWayResults.txt",myWayResult)
print("the print of results to txt files finished Successfully")
