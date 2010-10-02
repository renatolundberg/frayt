MODULE raymath
  IMPLICIT NONE

  ! um vetor
  TYPE vector
     REAL, DIMENSION (3) :: v
  END TYPE vector

  ! uma matriz
  TYPE matrix
     REAL, DIMENSION(4,4) :: mat
  END TYPE matrix

  ! o zero
  TYPE(vector), PARAMETER :: ZERO_VECTOR = vector( (/0,0,0/) ) 

  ! definicoes de operadores
  INTERFACE OPERATOR(-)
    MODULE PROCEDURE vector_subtract
  END INTERFACE

  INTERFACE OPERATOR(+)
    MODULE PROCEDURE vector_sum
  END INTERFACE

  INTERFACE OPERATOR(.DOT.)
    MODULE PROCEDURE vector_dot_product
  END INTERFACE

  INTERFACE OPERATOR(*)
    MODULE PROCEDURE vector_real_product
  END INTERFACE

  INTERFACE OPERATOR(.CROSS.)
    MODULE PROCEDURE vector_cross_product
  END INTERFACE

CONTAINS

! transformacao de um vetor de posicao
PURE FUNCTION pos_vector_transf(m, v)
  TYPE(vector), INTENT (IN)  :: v
  TYPE(matrix), INTENT (IN)  :: m
  TYPE(vector) pos_vector_transf
  pos_vector_transf%v(1) = m%mat(1,1) * v%v(1) + m%mat(1,2) * v%v(2) + m%mat(1,3) * v%v(3) + m%mat(1,4)
  pos_vector_transf%v(2) = m%mat(2,1) * v%v(1) + m%mat(2,2) * v%v(2) + m%mat(2,3) * v%v(3) + m%mat(2,4)
  pos_vector_transf%v(3) = m%mat(3,1) * v%v(1) + m%mat(3,2) * v%v(2) + m%mat(3,3) * v%v(3) + m%mat(3,4)
  RETURN
END FUNCTION pos_vector_transf

! transformacao de um vetor de direcao
PURE FUNCTION dir_vector_transf(m, v)
  TYPE(vector), INTENT (IN)  :: v
  TYPE(matrix), INTENT (IN)  :: m
  TYPE(vector) dir_vector_transf
  dir_vector_transf%v(1) = m%mat(1,1) * v%v(1) + m%mat(1,2) * v%v(2) + m%mat(1,3) * v%v(3)
  dir_vector_transf%v(2) = m%mat(2,1) * v%v(1) + m%mat(2,2) * v%v(2) + m%mat(2,3) * v%v(3)
  dir_vector_transf%v(3) = m%mat(3,1) * v%v(1) + m%mat(3,2) * v%v(2) + m%mat(3,3) * v%v(3)
  RETURN
END FUNCTION dir_vector_transf

! subtracao de dois vetores
PURE FUNCTION vector_subtract(v1, v2)
  TYPE(vector), INTENT (IN)  :: v1, v2
  TYPE(vector) vector_subtract
  !vector_subtract(1) = v1%v(1) - v2%v(1)
  !vector_subtract(2) = v1%v(2) - v2%v(2)
  !vector_subtract(3) = v1%v(3) - v2%v(3)
  vector_subtract%v = v1%v - v2%v
  RETURN
END FUNCTION vector_subtract

! subtracao de dois vetores
PURE FUNCTION vector_sum(v1, v2)
  TYPE(vector), INTENT (IN)  :: v1, v2
  TYPE(vector) vector_sum
  !vector_sum%x = v1%v(1) + v2%v(1)
  !vector_sum%y = v1%v(2) + v2%v(2)
  !vector_sum%z = v1%v(3) + v2%v(3)
  vector_sum%v = v1%v + v2%v
  RETURN
END FUNCTION vector_sum

! produto interno de dois vetores
PURE FUNCTION vector_dot_product(v1, v2)
  TYPE(vector), INTENT (IN)  :: v1, v2
  REAL vector_dot_product
  !vector_dot_product = v1%v(1) * v2%v(1) + v1%v(2) * v2%v(2) + v1%v(3) * v2%v(3)
  vector_dot_product = DOT_PRODUCT(v1%v, v2%v)
  RETURN
END FUNCTION vector_dot_product

! produto de um vetor por um escalar
PURE FUNCTION vector_real_product(v, x)
  TYPE(vector), INTENT (IN)  :: v
  REAL        , INTENT (IN)  :: x
  TYPE(vector) vector_real_product
  !vector_real_product%x = v%v(1) * x
  !vector_real_product%y = v%v(2) * x
  !vector_real_product%z = v%v(3) * x
  vector_real_product%v = v%v * x
  RETURN
END FUNCTION vector_real_product

! produto de dois vetores
PURE FUNCTION vector_cross_product(v1, v2)
  TYPE(vector), INTENT (IN)  :: v1, v2
  TYPE(vector) vector_cross_product
  vector_cross_product%v(1) = v1%v(2) * v2%v(3) - v1%v(3) * v2%v(2)
  vector_cross_product%v(2) = v1%v(3) * v2%v(1) - v1%v(1) * v2%v(3)
  vector_cross_product%v(3) = v1%v(1) * v2%v(2) - v1%v(2) * v2%v(1)
  RETURN
END FUNCTION vector_cross_product

END MODULE raymath
