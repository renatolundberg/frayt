MODULE raymath
  IMPLICIT NONE

  ! um vetor
  TYPE vector
     REAL :: x, y, z
  END TYPE vector

  ! uma matriz
  TYPE matrix
     REAL, DIMENSION(4,4) :: mat
  END TYPE matrix

  ! o zero
  TYPE(vector), PARAMETER :: ZERO_VECTOR = vector(0,0,0) 

  ! definicoes de operadores
  INTERFACE OPERATOR(-)
    MODULE PROCEDURE vector_subtract
  END INTERFACE

  INTERFACE OPERATOR(+)
    MODULE PROCEDURE vector_sum
  END INTERFACE

  INTERFACE OPERATOR(*)
    MODULE PROCEDURE vector_cross_product
  END INTERFACE

  INTERFACE OPERATOR(//)
    MODULE PROCEDURE vector_scalar_product
  END INTERFACE

CONTAINS

! transformacao de um vetor de posicao
PURE FUNCTION pos_vector_transf(m, v)
  TYPE(vector), INTENT (IN)  :: v
  TYPE(matrix), INTENT (IN)  :: m
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
  TYPE(vector) dir_vector_transf
  dir_vector_transf%x = m%mat(1,1) * v%x + m%mat(1,2) * v%y + m%mat(1,3) * v%z
  dir_vector_transf%y = m%mat(2,1) * v%x + m%mat(2,2) * v%y + m%mat(2,3) * v%z
  dir_vector_transf%z = m%mat(3,1) * v%x + m%mat(3,2) * v%y + m%mat(3,3) * v%z
  RETURN
END FUNCTION dir_vector_transf

! subtracao de dois vetores
PURE FUNCTION vector_subtract(v1, v2)
  TYPE(vector), INTENT (IN)  :: v1, v2
  TYPE(vector) vector_subtract
  vector_subtract%x = v1%x - v2%x
  vector_subtract%y = v1%y - v2%y
  vector_subtract%z = v1%z - v2%z
  RETURN
END FUNCTION vector_subtract

! subtracao de dois vetores
PURE FUNCTION vector_sum(v1, v2)
  TYPE(vector), INTENT (IN)  :: v1, v2
  TYPE(vector) vector_sum
  vector_sum%x = v1%x + v2%x
  vector_sum%y = v1%y + v2%y
  vector_sum%z = v1%z + v2%z
  RETURN
END FUNCTION vector_sum

! produto interno de dois vetores
PURE FUNCTION vector_scalar_product(v1, v2)
  TYPE(vector), INTENT (IN)  :: v1, v2
  REAL vector_scalar_product
  vector_scalar_product = v1%x * v2%x + v1%y * v2%y + v1%z * v2%z
  RETURN
END FUNCTION vector_scalar_product

! produto de um vetor por um escalar
PURE FUNCTION vector_real_product(v, x)
  TYPE(vector), INTENT (IN)  :: v
  REAL        , INTENT (IN)  :: x
  TYPE(vector) vector_real_product
  vector_real_product%x = v%x * x
  vector_real_product%y = v%y * x
  vector_real_product%z = v%z * x
  RETURN
END FUNCTION vector_real_product

! produto de dois vetores
PURE FUNCTION vector_cross_product(v1, v2)
  TYPE(vector), INTENT (IN)  :: v1, v2
  TYPE(vector) vector_cross_product
  vector_cross_product%x = v1%y * v2%z - v1%z * v2%y
  vector_cross_product%y = v1%z * v2%x - v1%x * v2%z
  vector_cross_product%z = v1%x * v2%y - v1%y * v2%x
  RETURN
END FUNCTION vector_cross_product

END MODULE raymath
