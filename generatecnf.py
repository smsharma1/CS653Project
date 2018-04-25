import random
import sys
import os

def gen_clause(N, K):
    Clause = []
    Literals = random.sample(range(1, N+1), K)
    for literal in Literals:
        sign  = random.randint(0,1)
        if sign:
            Clause.append(-literal)
        else:
            Clause.append(literal)
    print(Clause)
    return Clause

def main():
    N = int(sys.argv[1])
    K = int(sys.argv[2])
    for i in range(5,6):
        for j in range(1,6):
            f = open("data/" + str(N) + "_" + str(K) + "_" + str(int((i*N)/5)) + "_" + str(j) + ".cnf","w+")
            f.write("p " + "cnf "+  str(N) + " " + str(int((i*N)/5)) + "\n")
            for m in range(0,int((i*N)/5)):
                clause = gen_clause(N,K)
                f.write("%d %d %d 0\n" % (clause[0], clause[1], clause[2]))
            f.close()

if __name__== "__main__":
    main()

