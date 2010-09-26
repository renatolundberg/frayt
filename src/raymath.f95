MODULE ray_math
  IMPLICIT NONE

  TYPE vector
     REAL :: x, y, z
  END TYPE vector
  TYPE matrix
     REAL, DIMENSION(4,4) :: mat
  END TYPE matrix
CONTAINS

! transformacao de um vetor de posicao
PURE FUNCTION pos_vector_transf(m, v)
  TYPE(vector), INTENT (IN)  :: v
  TYPE(matrix), INTENT (IN)  :: m
  TYPE(vector) :: r
  TYPE(vector) pos_vector_transf
  pos_vector_transf%x = m%mat(1,1) * v%x + m%mat(1,2) * v%y + m%mat(1,3) * v%z + m%mat(1,4)
  pos_vector_transf%y = m%mat(2,1) * v%x + m%mat(2,2) * v%y + m%mat(2,3) * v%z + m%mat(2,4)
  pos_vector_transf%z = m%mat(3,1) * v%x + m%mat(3,2) * v%y + m%mat(3,3) * v%z + m%mat(3,4)
  RETURN
END FUNCTION pos_vector_transf

! transformacao de um vetor de direcao
PURE FUNCTION dir_vector_transf(m, v)
  TYPE(vector), INTENT (IN)  :: v
  TYPE(matrix), INTENT (IN)  :: m
  TYPE(vector) :: r
  TYPE(vector) dir_vector_transf
  dir_vector_transf%x = m%mat(1,1) * v%x + m%mat(1,2) * v%y + m%mat(1,3) * v%z
  dir_vector_transf%y = m%mat(2,1) * v%x + m%mat(2,2) * v%y + m%mat(2,3) * v%z
  dir_vector_transf%z = m%mat(3,1) * v%x + m%mat(3,2) * v%y + m%mat(3,3) * v%z
  RETURN
END FUNCTION dir_vector_transf

END MODULE ray_math
