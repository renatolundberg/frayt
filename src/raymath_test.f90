PROGRAM raymath_test
  USE raymath
  USE raytest
  IMPLICIT NONE

  TYPE(test_result) :: res = test_result(0, 0)

!  CALL test_pos_vector_transf_ident(res)
!  CALL test_pos_vector_transf_real(res)
!  CALL test_dir_vector_transf_ident(res)
!  CALL test_dir_vector_transf_real(res)
  CALL test_vector_subtract(res)
  CALL test_vector_sum(res)
  CALL test_vector_dot_product(res)
  CALL test_vector_real_product(res)
  CALL test_vector_cross_product(res)
  CALL test_linear_solver(res)
  CALL test_quadratic_solver(res)

  PRINT *, "Teste do modulo raymath finalizado. Ocorreram ", res%failures, " falhas em ", res%assertions, " verificacoes."
  IF (res%failures > 0) THEN
    CALL EXIT(1)
  ELSE
    CALL EXIT(0)
  END IF
CONTAINS

!SUBROUTINE test_pos_vector_transf_ident(res)
!  TYPE(test_result)   :: res
!  TYPE(vector)        :: v = vector((/1, 2, 3/))
!  TYPE(vector)        :: e = vector((/1, 2, 3/))
!  TYPE(matrix)        :: m 
!  TYPE(vector)        :: r
!  LOGICAL             :: failure
!  m%mat(1,1:4) = (/1,0,0)
!  m%mat(2,1:4) = (/0,1,0)
!  m%mat(3,1:4) = (/0,0,1)
!  r = pos_vector_transf(m, v)
!  failure = assertTrue(res, r%v(1) == e%v(1) .AND. r%v(2) == e%v(2) .AND. r%v(3) == e%v(3))
!  IF (failure) THEN
!    PRINT *, "pos_vector_transf com matriz identidade falhou. Esperado", e, " mas encontrado ", r
!  END IF
!  RETURN
!END SUBROUTINE test_pos_vector_transf_ident
!
!SUBROUTINE test_pos_vector_transf_real(res)
!  TYPE(test_result)   :: res
!  TYPE(vector)        :: v = vector((/3, 5, 7/))
!  TYPE(vector)        :: e = vector((/REAL(1*3+2*5+3*7+4), REAL(1.1*3+1.2*5+1.3*7+1.4), REAL(2.5*3+3.01*5+2.75*7+2.5)/))
!  TYPE(matrix)        :: m 
!  TYPE(vector)        :: r
!  LOGICAL             :: failure
!  m%mat(1,1:4) = (/ 1, 2, 3, 4/)
!  m%mat(2,1:4) = (/1.1,1.2,1.3,1.4/)
!  m%mat(3,1:4) = (/2.5,3.01,2.75,2.5/)
!  m%mat(4,1:4) = (/0,0,0,0/)
!  r = pos_vector_transf(m, v)
!  failure = assertTrue(res, r%v(1) == e%v(1) .AND. r%v(2) == e%v(2) .AND. r%v(3) == e%v(3))
!  IF (failure) THEN
!    PRINT *, "pos_vector_transf com matriz real falhou. Esperado", e, " mas encontrado ", r
!  END IF
!  RETURN
!END SUBROUTINE test_pos_vector_transf_real


!SUBROUTINE test_dir_vector_transf_ident(res)
!  TYPE(test_result)   :: res
!  TYPE(vector)        :: v = vector((/1, 2, 3/))
!  TYPE(vector)        :: e = vector((/1, 2, 3/))
!  TYPE(matrix)        :: m 
!  TYPE(vector)        :: r
!  LOGICAL             :: failure
!  m%mat(1,1:4) = (/1,0,0,0/)
!  m%mat(2,1:4) = (/0,1,0,0/)
!  m%mat(3,1:4) = (/0,0,1,0/)
!  m%mat(4,1:4) = (/0,0,0,0/)
!  r = dir_vector_transf(m, v)
!  failure = assertTrue(res, r%v(1) == e%v(1) .AND. r%v(2) == e%v(2) .AND. r%v(3) == e%v(3))
!  IF (failure) THEN
!    PRINT *, "dir_vector_transf com matriz identidade falhou. Esperado", e, " mas encontrado ", r
!  END IF
!  RETURN
!END SUBROUTINE test_dir_vector_transf_ident
!
!SUBROUTINE test_dir_vector_transf_real(res)
!  TYPE(test_result)   :: res
!  TYPE(vector)        :: v = vector((/3, 5, 7/))
!  TYPE(vector)        :: e = vector((/REAL(1*3+2*5+3*7), REAL(1.1*3+1.2*5+1.3*7), REAL(2.5*3+3.01*5+2.75*7)/))
!  TYPE(matrix)        :: m 
!  TYPE(vector)        :: r
!  LOGICAL             :: failure
!  m%mat(1,1:4) = (/ 1, 2, 3, 4/)
!  m%mat(2,1:4) = (/1.1,1.2,1.3,1.4/)
!  m%mat(3,1:4) = (/2.5,3.01,2.75,2.5/)
!  m%mat(4,1:4) = (/0,0,0,0/)
!  r = dir_vector_transf(m, v)
!  failure = assertTrue(res, r%v(1) == e%v(1) .AND. r%v(2) == e%v(2) .AND. r%v(3) == e%v(3))
!  IF (failure) THEN
!    PRINT *, "dir_vector_transf com matriz real falhou. Esperado", e, " mas encontrado ", r
!  END IF
!  RETURN
!END SUBROUTINE test_dir_vector_transf_real

SUBROUTINE test_vector_subtract(res)
  TYPE(test_result)   :: res
  TYPE(vector)        :: v = vector((/3.3, 5.7, 2.8/))
  TYPE(vector)        :: u = vector((/2.1, -5.3, 0.0/))
  TYPE(vector)        :: e = vector((/3.3-2.1,5.7+5.3,2.8/))
  TYPE(vector)        :: r
  LOGICAL             :: failure
  r = vector_subtract(v, u)
  failure = assertTrue(res, r%v(1) == e%v(1) .AND. r%v(2) == e%v(2) .AND. r%v(3) == e%v(3))
  IF (failure) THEN
    PRINT *, "vector_subtract falhou. Esperado", e, " mas encontrado ", r
  END IF
  RETURN
END SUBROUTINE test_vector_subtract

SUBROUTINE test_vector_sum(res)
  TYPE(test_result)    :: res
  TYPE(vector)         :: v = vector((/3.3, 5.7, 2.8/))
  TYPE(vector)         :: u = vector((/2.1, -5.3, 0.0/))
  TYPE(vector)         :: e = vector((/3.3+2.1,5.7-5.3,2.8/))
  TYPE(vector)         :: r
  LOGICAL              :: failure
  r = vector_sum(v, u)
  failure = assertTrue(res, r%v(1) == e%v(1) .AND. r%v(2) == e%v(2) .AND. r%v(3) == e%v(3))
  IF (failure) THEN
    PRINT *, "vector_sum falhou. Esperado", e, " mas encontrado ", r
  END IF
  RETURN
END SUBROUTINE test_vector_sum

SUBROUTINE test_vector_dot_product(res)
  TYPE(test_result)   :: res
  TYPE(vector)        :: v = vector((/3.0, 5.7, 2.75/))
  TYPE(vector)        :: u = vector((/2.1, -5.0, 1.0/))
  REAL                :: e = 3.0*2.1 + 5.7*(-5.0) + 2.75
  REAL                :: r
  LOGICAL             :: failure
  r = vector_dot_product(v, u)
  failure = assertTrue(res, ABS(r - e) < 0.00001)
  IF (failure) THEN
    PRINT *, "vector_dot_product falhou. Esperado", e, " mas encontrado ", r
  END IF
  RETURN
END SUBROUTINE test_vector_dot_product

SUBROUTINE test_vector_real_product(res)
  TYPE(test_result) :: res
  TYPE(vector)      :: v = vector((/3.3, 5.7, 2.8/))
  REAL              :: x = -2.5
  TYPE(vector)      :: e = vector((/-2.5 * 3.3,-2.5 * 5.7,-2.5* 2.8/))
  TYPE(vector)      :: r
  LOGICAL :: failure
  r = vector_real_product(v, x)
  failure = assertTrue(res, r%v(1) == e%v(1) .AND. r%v(2) == e%v(2) .AND. r%v(3) == e%v(3))
  IF (failure) THEN
    PRINT *, "vector_real_product falhou. Esperado", e, " mas encontrado ", r
  END IF
  RETURN
END SUBROUTINE test_vector_real_product

SUBROUTINE test_vector_cross_product(res)
  TYPE(test_result) :: res
  !verificacao atraves das identidades dos vetores fundamentais
  CALL check_vector_cross_product(res, vector((/1, 0, 0/)), vector((/1, 0, 0/)), vector((/0, 0, 0/)))
  CALL check_vector_cross_product(res, vector((/1, 0, 0/)), vector((/0, 1, 0/)), vector((/0, 0, 1/)))
  CALL check_vector_cross_product(res, vector((/1, 0, 0/)), vector((/0, 0, 1/)), vector((/0, -1, 0/)))
  CALL check_vector_cross_product(res, vector((/0, 1, 0/)), vector((/1, 0, 0/)), vector((/0, 0, -1/)))
  CALL check_vector_cross_product(res, vector((/0, 1, 0/)), vector((/0, 1, 0/)), vector((/0, 0, 0/)))
  CALL check_vector_cross_product(res, vector((/0, 1, 0/)), vector((/0, 0, 1/)), vector((/1, 0, 0/)))
  CALL check_vector_cross_product(res, vector((/0, 0, 1/)), vector((/1, 0, 0/)), vector((/0, 1, 0/)))
  CALL check_vector_cross_product(res, vector((/0, 0, 1/)), vector((/0, 1, 0/)), vector((/-1, 0, 0/)))
  CALL check_vector_cross_product(res, vector((/0, 0, 1/)), vector((/0, 0, 1/)), vector((/0, 0, 0/)))
  RETURN 
END SUBROUTINE test_vector_cross_product

SUBROUTINE check_vector_cross_product(res, v, u, e)
  TYPE(test_result) :: res
  TYPE(vector)      :: v, u, e, r
  LOGICAL           :: failure
  r = vector_cross_product(v, u)
  failure = assertTrue(res, r%v(1) == e%v(1) .AND. r%v(2) == e%v(2) .AND. r%v(3) == e%v(3))
  IF (failure) THEN
    PRINT *, "vector_cross_product falhou. Esperado", e, " mas encontrado ", r
  END IF
  RETURN
END SUBROUTINE check_vector_cross_product


! testes de solucao de equacoes lineares
subroutine test_linear_solver(res)
  type(test_result) :: res
  type(retval) :: e
  e%num_val = 1
  e%value(1) = 3.0
  call check_linear_solution(res, 2.0, -6.0, e)
  e%value(1) = -2.0
  call check_linear_solution(res, 3.0, 6.0, e)
  e%value(1) = -5.25
  call check_linear_solution(res, 4.0, 21.0, e)
  e%num_val = 0
  e%value(1) = 0.0
  call check_linear_solution(res, 0.0, -6.0, e)
  return
end subroutine test_linear_solver

subroutine check_linear_solution(res, c1, c0, e)
  type(test_result) :: res
  real :: c1, c0
  type(retval) :: e, r
  logical :: failure
  r = calc_root_of_linear(c1, c0)
  failure = assertTrue(res, r%num_val == e%num_val .AND. r%value(1) == e%value(1))
  if (failure) then
    print *, "calc_root_of_linear falhou. Esperado", e, " mas encontrado ", r
  end if
return
end subroutine check_linear_solution


! testes de solucao de equacoes quadraticas
! exemplos retirados de http://www.purplemath.com/modules/solvquad6.htm
subroutine test_quadratic_solver(res)
  type(test_result) :: res
  type(retval) :: e
  e%num_val = 2
  e%value(1) = 3.0
  e%value(2) = -1.0
  call check_quadratic_solution(res, 1.0, -2.0, -3.0, e)
  e%num_val = 2
  e%value(1) = (-1.0+(sqrt(17.0))) / 2.0
  e%value(2) = (-1.0-(sqrt(17.0))) / 2.0
  call check_quadratic_solution(res, 1.0, 1.0, -4.0, e)
  e%num_val = 1
  e%value(1) = -4.0 / 2.0
  e%value(2) = 0.0
  call check_quadratic_solution(res, 1.0, 4.0, 4.0, e)
  e%num_val = 0
  e%value(1) = 0.0
  e%value(2) = 0.0
  call check_quadratic_solution(res, 1.0, 1.0, 4.0, e)
  return
end subroutine test_quadratic_solver

subroutine check_quadratic_solution(res, c2, c1, c0, e)
  type(test_result) :: res
  real :: c2, c1, c0
  type(retval) :: e, r
  logical :: failure
  r = calc_root_of_quadratic(c2, c1, c0)
  failure = assertTrue(res, r%num_val == e%num_val .AND. r%value(1) == e%value(1) .AND. r%value(2) == e%value(2))
  if (failure) then
    print *, "calc_root_of_quadratic falhou. Esperado", e, " mas encontrado ", r
  end if
return
end subroutine check_quadratic_solution


END PROGRAM raymath_test
