PROGRAM raymath_test
  USE ray_math

  TYPE test_result
    INTEGER :: assertions, failures
  END TYPE test_result

  TYPE(test_result) :: res = test_result(0, 0)
  PRINT *, 'Iniciando testes...'

  CALL test_mult_m_v(res)
  PRINT *, "Teste finalizado. ", res%assertions, " com um total de ", res%failures, "falhas."
  IF (res%failures > 0) THEN
    CALL EXIT(1)
  ELSE
    CALL EXIT(0)
  END IF
CONTAINS

SUBROUTINE test_mult_m_v(res)
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
  r = mult_m_v(m, v)
  failure = assertTrue(res, r%x == e%x .AND. r%y == e%y .AND. r%z == e%z)
  IF (failure) THEN
    PRINT *, "Multiplicacao de vetor com matrix falhou. Esperado", e, " mas encontrado ", e
  END IF
  RETURN
END SUBROUTINE test_mult_m_v

FUNCTION assertTrue(res, cond)
  TYPE(test_result), INTENT(INOUT) :: res
  LOGICAL          , INTENT(IN)    :: cond
  LOGICAL                          :: assertTrue
  res%assertions = res%assertions + 1
  IF(.NOT. cond) THEN
    res%failures = res%failures + 1
  END IF
  assertTrue = .NOT. cond;
  RETURN;
END FUNCTION assertTrue

END PROGRAM raymath_test
