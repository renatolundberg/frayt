PROGRAM raymath_test
  USE ray_math
  USE ray_test
  IMPLICIT NONE

  TYPE(test_result) :: res = test_result(0, 0)

  CALL test_pos_vector_transf_ident(res)
  CALL test_pos_vector_transf_real(res)
  CALL test_dir_vector_transf_ident(res)
  CALL test_dir_vector_transf_real(res)

  PRINT *, "Teste do modulo raymath finalizado. Ocorreram ", res%failures, " falhas em ", res%assertions, " verificacoes."
  IF (res%failures > 0) THEN
    CALL EXIT(1)
  ELSE
    CALL EXIT(0)
  END IF
CONTAINS

SUBROUTINE test_pos_vector_transf_ident(res)
  TYPE(test_result) :: res
  TYPE(vector) :: v = vector(1, 2, 3)
  TYPE(vector) :: e = vector(1, 2, 3)
  TYPE(matrix) :: m 
  TYPE(vector) :: r
  LOGICAL :: failure
  m%mat(1,1:4) = (/1,0,0,0/)
  m%mat(2,1:4) = (/0,1,0,0/)
  m%mat(3,1:4) = (/0,0,1,0/)
  m%mat(4,1:4) = (/0,0,0,1/)
  r = pos_vector_transf(m, v)
  failure = assertTrue(res, r%x == e%x .AND. r%y == e%y .AND. r%z == e%z)
  IF (failure) THEN
    PRINT *, "pos_vector_transf com matriz identidade falhou. Esperado", e, " mas encontrado ", r
  END IF
  RETURN
END SUBROUTINE test_pos_vector_transf_ident

SUBROUTINE test_pos_vector_transf_real(res)
  TYPE(test_result) :: res
  TYPE(vector) :: v = vector(3, 5, 7)
  TYPE(vector) :: e = vector(1*3+2*5+3*7+4, 1.1*3+1.2*5+1.3*7+1.4, 2.5*3+3.01*5+2.75*7+2.5)
  TYPE(matrix) :: m 
  TYPE(vector) :: r
  LOGICAL :: failure
  m%mat(1,1:4) = (/ 1, 2, 3, 4/)
  m%mat(2,1:4) = (/1.1,1.2,1.3,1.4/)
  m%mat(3,1:4) = (/2.5,3.01,2.75,2.5/)
  m%mat(4,1:4) = (/0,0,0,0/)
  r = pos_vector_transf(m, v)
  failure = assertTrue(res, r%x == e%x .AND. r%y == e%y .AND. r%z == e%z)
  IF (failure) THEN
    PRINT *, "pos_vector_transf com matriz real falhou. Esperado", e, " mas encontrado ", r
  END IF
  RETURN
END SUBROUTINE test_pos_vector_transf_real


SUBROUTINE test_dir_vector_transf_ident(res)
  TYPE(test_result) :: res
  TYPE(vector) :: v = vector(1, 2, 3)
  TYPE(vector) :: e = vector(1, 2, 3)
  TYPE(matrix) :: m 
  TYPE(vector) :: r
  LOGICAL :: failure
  m%mat(1,1:4) = (/1,0,0,0/)
  m%mat(2,1:4) = (/0,1,0,0/)
  m%mat(3,1:4) = (/0,0,1,0/)
  m%mat(4,1:4) = (/0,0,0,0/)
  r = dir_vector_transf(m, v)
  failure = assertTrue(res, r%x == e%x .AND. r%y == e%y .AND. r%z == e%z)
  IF (failure) THEN
    PRINT *, "dir_vector_transf com matriz identidade falhou. Esperado", e, " mas encontrado ", r
  END IF
  RETURN
END SUBROUTINE test_dir_vector_transf_ident

SUBROUTINE test_dir_vector_transf_real(res)
  TYPE(test_result) :: res
  TYPE(vector) :: v = vector(3, 5, 7)
  TYPE(vector) :: e = vector(1*3+2*5+3*7, 1.1*3+1.2*5+1.3*7, 2.5*3+3.01*5+2.75*7)
  TYPE(matrix) :: m 
  TYPE(vector) :: r
  LOGICAL :: failure
  m%mat(1,1:4) = (/ 1, 2, 3, 4/)
  m%mat(2,1:4) = (/1.1,1.2,1.3,1.4/)
  m%mat(3,1:4) = (/2.5,3.01,2.75,2.5/)
  m%mat(4,1:4) = (/0,0,0,0/)
  r = dir_vector_transf(m, v)
  failure = assertTrue(res, r%x == e%x .AND. r%y == e%y .AND. r%z == e%z)
  IF (failure) THEN
    PRINT *, "dir_vector_transf com matriz real falhou. Esperado", e, " mas encontrado ", r
  END IF
  RETURN
END SUBROUTINE test_dir_vector_transf_real

END PROGRAM raymath_test
