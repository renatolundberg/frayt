MODULE raymath
  IMPLICIT NONE

  ! um vetor
  TYPE vector
     REAL, DIMENSION (4) :: v
  END TYPE vector

  ! uma matriz
  TYPE matrix
     REAL, DIMENSION(4,4) :: mat
  END TYPE matrix

  ! tipo para retorno de solucoes de equacoes quadraticas
  TYPE retval
    INTEGER :: num_val
    REAL, DIMENSION(2) :: value
  END TYPE retval

  ! o zero
  TYPE(vector), PARAMETER :: ZERO_VECTOR = vector( (/0,0,0,0/) )
  ! o um
  TYPE(vector), PARAMETER :: ONE_VECTOR = vector( (/1,1,1,1/) )

  ! definicoes de operadores
  INTERFACE OPERATOR(-)
    MODULE PROCEDURE vector_subtract
  END INTERFACE

  INTERFACE OPERATOR(+)
    MODULE PROCEDURE vector_sum
  END INTERFACE

  INTERFACE OPERATOR(*)
    MODULE PROCEDURE vector_multiply
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
  pos_vector_transf%v = MATMUL(m%mat, v%v) + m%mat(1:4,4)
  RETURN
END FUNCTION pos_vector_transf

! transformacao de um vetor de direcao
PURE FUNCTION dir_vector_transf(m, v)
  TYPE(vector), INTENT (IN)  :: v
  TYPE(matrix), INTENT (IN)  :: m
  TYPE(vector) dir_vector_transf
  dir_vector_transf%v = MATMUL(m%mat, v%v)
  RETURN
END FUNCTION dir_vector_transf

! subtracao de dois vetores
PURE FUNCTION vector_subtract(v1, v2)
  TYPE(vector), INTENT (IN) :: v1, v2
  TYPE(vector) vector_subtract
  vector_subtract%v = v1%v - v2%v
END FUNCTION vector_subtract

! adicao de dois vetores
PURE FUNCTION vector_sum(v1, v2)
  TYPE(vector), INTENT (IN)  :: v1, v2
  TYPE(vector) vector_sum
  vector_sum%v = v1%v + v2%v
  RETURN
END FUNCTION vector_sum

! adicao de dois vetores
PURE FUNCTION vector_multiply(v1, v2)
  TYPE(vector), INTENT (IN)  :: v1, v2
  TYPE(vector) vector_multiply
  vector_multiply%v = v1%v * v2%v
  RETURN
END FUNCTION vector_multiply

! produto interno de dois vetores
PURE FUNCTION vector_dot_product(v1, v2)
  TYPE(vector), INTENT (IN)  :: v1, v2
  REAL vector_dot_product
  vector_dot_product = DOT_PRODUCT(v1%v, v2%v)
  RETURN
END FUNCTION vector_dot_product

! produto de um vetor por um escalar
PURE FUNCTION vector_real_product(v, x)
  TYPE(vector), INTENT (IN)  :: v
  REAL        , INTENT (IN)  :: x
  TYPE(vector) vector_real_product
  vector_real_product%v = v%v * x
  RETURN
END FUNCTION vector_real_product

! produto cruzado (?) de dois vetores
PURE FUNCTION vector_cross_product(v1, v2)
  TYPE(vector), INTENT (IN)  :: v1, v2
  TYPE(vector) :: vector_cross_product
  vector_cross_product%v(1) = v1%v(2) * v2%v(3) - v1%v(3) * v2%v(2)
  vector_cross_product%v(2) = v1%v(3) * v2%v(1) - v1%v(1) * v2%v(3)
  vector_cross_product%v(3) = v1%v(1) * v2%v(2) - v1%v(2) * v2%v(1)
  vector_cross_product%v(4) = 0
  RETURN
END FUNCTION vector_cross_product

! baseado em: http://fuzzyphoton.tripod.com/howtowrt.htm#step1
pure function fdiv(a, b) result (d)
  real, intent(in) :: a, b
  real :: d

  if (a == 0) then
    d = 0
    return
  end if

  if (b == 0) then
    d = sign(huge(d),a)
  else
    if ((a+b) == a) then
      d = sign(huge(d),a*b)
    else
      d =  a / b
    end if
  end if
end function fdiv


! verifica se o numero e' menor do que +- epsilon
! TODO: verificar se não é melhor usar tiny()
pure function is_zero(a) result(b)
  real, intent(in) :: a
  logical :: b
  b = abs(a) < epsilon(a)
end function is_zero


! calcula a raiz de uma equacao linear
pure function calc_root_of_linear(c1, c0) result(ret)
  real, intent(in) :: c1, c0
  type(retval) :: ret
  if (is_zero(c1)) then
    ret%num_val = 0
    ret%value(1) = 0.0
  else
    ret%num_val = 1
    ret%value(1) = fdiv(-c0,c1)
  end if
end function calc_root_of_linear


! calcula duas raizes distintas de uma equacao quadratica
pure function calc_root_of_quadratic(c2, c1, c0) result(ret)
  real, intent(in) :: c2, c1, c0
  real :: d
  type(retval) :: ret
  if (is_zero(c2)) then
    ret = calc_root_of_linear(c1, c0)
  else
    d = c1 * c1 - (4 * c2 * c0)
    if (is_zero(d)) then
      ret%num_val = 1
      ret%value(1) = fdiv(-c1, 2*c2)
      ret%value(2) = 0.0
    else
      if (d > 0) then
        ret%num_val = 2
        ret%value(1) = fdiv(-c1+sqrt(d), 2*c2)
        ret%value(2) = fdiv(-c1-sqrt(d), 2*c2)
      else
        ret%num_val = 0
        ret%value(1) = 0.0
        ret%value(2) = 0.0
      end if
    end if
  end if
end function calc_root_of_quadratic


END MODULE raymath
