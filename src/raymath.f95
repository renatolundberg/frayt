MODULE ray_math
  TYPE vector
     REAL :: x, y, z
  END TYPE vector
  TYPE matrix
     REAL, DIMENSION(4,4) :: mat
  END TYPE matrix
CONTAINS
PURE FUNCTION mult_m_v(m, v)
  TYPE(vector), INTENT (IN)  :: v
  TYPE(matrix), INTENT (IN)  :: m
  TYPE(vector) :: r
  TYPE(vector) mult_m_v
  r%x = m%mat(1,1) * v%x + m%mat(1,2) * v%y + m%mat(1,3) * v%z + m%mat(1,4)
  r%y = m%mat(2,1) * v%x + m%mat(2,2) * v%y + m%mat(2,3) * v%z + m%mat(2,4)
  r%z = m%mat(3,1) * v%x + m%mat(3,2) * v%y + m%mat(3,3) * v%z + m%mat(3,4)
  mult_m_v = r
  RETURN
END FUNCTION mult_m_v

PURE FUNCTION mult(arg1, arg2)
  REAL, INTENT (IN) :: arg1, arg2
  mult = arg1 * arg2
  return
END FUNCTION mult

END MODULE ray_math
