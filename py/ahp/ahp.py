from collections import namedtuple
import numpy as np
from numpy import array
import sys

class AHP(object):
    '''
    Calculates the normalized principal eigenvector for a square matrix.
    using numpy and nth root of product approx method.
    
    Also provides a consistency check
    '''

    # random consistency index (RCI)
    RCI = { 
            1: 0, 
            2: 0, 
            3: 0.58, 
            4: 0.9, 
            5: 1.12, 
            6: 1.24, 
            7: 1.32,
            8: 1.41,
            9: 1.45,
           10: 1.49,
        }

    def __init__(self, matrix=None, criteria=None):
        self.m = self.matrix = matrix
        self.criteria = sorted(criteria) if criteria else None
        

    def run(self):
        if sys.argv > 2:
            self.criteria = sys.argv[1:]
        if self.criteria:
            self.make_comparison()
            self.approximate()
            self.analyze()
        elif self.has_matrix:
            self.approximate()
            self.analyze()
        elif isinstance(self.matrix, list):
            self.matrix = np.array(self.matrix)
            self.approximate()
            self.analyze()            
        else:
            raise Exception("neither criteria nor matrix specified")
            return
        self.report()
    
    @property
    def has_matrix(self):
        return isinstance(self.matrix, np.ndarray)

    @property
    def n(self):
        if self.criteria: return len(self.criteria)
        elif self.has_matrix: return self.matrix.shape[0]
        else: return
    
    @property
    def n_comparisons(self):
        return self.n * (self.n - 1) / 2

    def approximate(self):
        self.nth_root_prod = array([pow(row.prod(), 1./row.size) 
                                    for row in self.matrix])
        self.approx_npev = (self.nth_root_prod / 
                               sum(self.nth_root_prod))

    def analyze(self):
        self.eigenvalues, self.eigenvectors = np.linalg.eig(self.matrix)    
        
        self.principal_eigen_value = max(self.eigenvalues)
            
        self.principal_eigen_vector = self.eigenvectors.diagonal()
        
        self.npev = self.normalized_principal_eigen_vector = (
            abs(self.principal_eigen_vector) / 
            sum(abs(self.principal_eigen_vector))
        )
     
        # consistency check
        self.consistency_index = (
            (self.principal_eigen_value - self.n) / 
            (self.n - 1)
        )
        self.consistency_ratio = abs (
            self.consistency_index / 
            self.RCI[self.n]
        )
        
        if self.consistency_ratio <= 0.1:
            self.is_consistent = True
        else:
            self.is_consistent = False

    def report(self):
        if self.criteria:
            f = open("-".join(self.criteria)+".txt", "w")
            print >> f, "criteria: " + " ".join(self.criteria)
        else:
            f = sys.stdout
        def show(title, obj):
            print >> f, "{0}:".format(title).ljust(15), obj
        print >> f
        print >> f, 'matrix:'
        if self.criteria:
            self.display(f)
        else:
            print >> f, self.matrix
        print >> f, 'is consistent:', self.is_consistent
        CR = round(abs(self.consistency_ratio), 4)
        if self.is_consistent:
            print >> f, 'CR: {0}% <= 10%'.format(CR * 100)
        else:
            print >> f, 'CR: {0}% > 10%'.format(CR * 100)
        print >> f, 'normalized principal eigen vector:'
        show('a) numpy', abs(self.npev))
        show('b) approx.', abs(self.approx_npev))
        errors = self.approx_npev - self.npev
        show('errors (b-a)', errors)
    
    def display(self, f):
        print >> f, '(rounding to 2 decs)'
        row_labels = [w[:3].upper() for w in self.criteria]
        print >> f, "      " + "   ".join(row_labels)
        for row_label, row in zip(row_labels, self.matrix):
            print >> f, '%s [%s]' % (row_label, '  '.join('%04s' % round(i,2) for i in row))

    def make_comparison(self):
        cl = self.criteria[:]
        lst = []
        flag = 1
        for j in range(len(self.criteria)):
            q = "({0}/{1}) {2} 9  8  7  6  5  4  3  2  1 -2 -3 -4 -5 -6 -7 -8 -9 {3}"
            fst = cl.pop(0)
            for i in cl:
                print q.format(flag, self.n_comparisons, fst, i)
                flag += 1
                response = int(raw_input("> "))
                lst.append((fst, i, response))
        
        res = []
        # add the reciprocals
        for i,j,v in lst:
            if v < 0:
                res.append((j, i, -v))
                res.append((i, j, 1.0 / -v))
            else:
                res.append((i,j,v))
                res.append((j,i,1./v))
        
        # add the equal pairs e.g. (a, a, 1)
        for i in self.criteria:
            res.append((i, i, 1.0))
        
        combos = sorted(res)
        a = np.array([v for (i,j,v) in combos])
        self.matrix = a.reshape((self.n, self.n))


def test_matrices():
    m0 = array([
            [1., 1/3., 1/9., 1/5.],
            [3., 1., 1., 1.],
            [9., 1., 1., 3.],
            [5., 1., 1/3., 1.]
         ])
         
    m1 = array([
            [1., 1/3., 5.],
            [3., 1.,   7.],
            [1/5., 1/7., 1.]
         ])


    m2 = array([
            [1., 3., 7., 9.],
            [0.33, 1., 5., 7.],
            [0.14, 0.2, 1., 3.],
            [0.11, 0.14, 0.33, 1.],
         ])
         
    m3 = array([
            [1., 1/4., 4.],
            [4., 1., 9.],
            [1/4., 1/9., 1.]
         ])
         
    m4 = array([
            [1.  , 2.  , 3.],
            [1/2., 1.  , 2.],
            [1/3., 1/2., 1.],
         ])
    
    m5 = array([
            [1.  , 2.  , 3.  , 4.  , 5.],
            [1/2., 1.  , 2.  , 3.  , 4.],
            [1/3., 1/2., 1.  , 2.  , 3.],
            [1/4., 1/3., 1/2., 1.  , 2.],
            [1/5., 1/4., 1/3., 1/2., 1.]
         ])

    matrices = [m0, m1, m2, m3, m4, m5]
    for matrix in matrices:
        m = AHP(matrix)
        m.run()


def test_evaluate():
#    criteria = ["experience", "education", "charisma", "age"]
    #criteria = ["experience", "education", "speed"]
    app = AHP()
    app.run()
    return app
    



if __name__ == '__main__':
    #app = test_evaluate()
    test_matrices()
    


