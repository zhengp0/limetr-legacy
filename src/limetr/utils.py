# utility classes and functions
import numpy as np
from scipy.optimize import bisect
from spmat import BDLMat


class VarMat:
    """
    Covariates matrix class with form
        Diag(v_i) + Z_i Diag(gamma) Z_i^T
    with i is refered as the id of the block.

    provide functions
    - varMat
    - invVarMat
    - dot
    - invDot
    - diag
    - invDiag
    - logDet
    """

    def __init__(self, v, z, gamma, n):
        """
        initialize the object
        """
        # dimensions
        self.m = len(n)
        self.N = sum(n)
        self.k = gamma.size
        self.bdlmat = BDLMat.create_bdlmat(v, z*np.sqrt(gamma), n)

    def varMat(self):
        """
        naive implementation of the varmat
        """
        return self.bdlmat.mat

    def invVarMat(self):
        """
        naive implementation of the inverse varmat
        """
        return self.bdlmat.invmat

    def dot(self, x):
        """
        dot product with the covariate matrix
        """
        return self.bdlmat.dot(x)

    def invDot(self, x):
        """
        inverse dot product with the covariate matrix
        """
        return self.bdlmat.invdot(x)

    def diag(self):
        """
        return the diagonal of the matrix
        """
        return np.diag(self.varMat())

    def invDiag(self):
        """
        return the diagonal of the inverse covariate matrix
        """
        return np.diag(self.invVarMat())

    def logDet(self):
        """
        returns the log determinant of the covariate matrix
        """
        return self.bdlmat.logdet()

    @classmethod
    def testProblem(cls):
        n = [3, 4, 5]
        N = sum(n)
        k = 4

        v = np.random.rand(N) + 1e-2
        z = np.random.randn(N, k)
        gamma = np.random.rand(k)

        return cls(v, z, gamma, n)

    # internal functions
    # -------------------------------------------------------------------------
    @staticmethod
    def _blockVarMat(v, z, gamma):
        return np.diag(v) + (z*gamma).dot(z.T)

    @staticmethod
    def _blockInvVarMat(v, z, gamma):
        return np.linalg.inv(np.diag(v) + (z*gamma).dot(z.T))


def projCappedSimplex(w, w_sum, active_id=None):
    N = w.size
    if active_id is None:
        active_id = np.arange(N)

    w_all = np.ones(N)
    w = w[active_id]
    w_sum = w_sum - (N - active_id.size)

    a = np.min(w) - 1.0
    b = np.max(w) - 0.0

    def f(x):
        return np.sum(np.maximum(np.minimum(w - x, 1.0), 0.0)) - w_sum

    x = bisect(f, a, b)

    w = np.maximum(np.minimum(w - x, 1.0), 0.0)
    w_all[active_id] = w
    return w_all
