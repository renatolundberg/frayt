MODULE raytest
  IMPLICIT NONE
  TYPE test_result
    INTEGER :: assertions, failures
  END TYPE test_result
CONTAINS

FUNCTION assertTrue(res, cond)
  TYPE(test_result), INTENT(INOUT) :: res
  LOGICAL          , INTENT(IN)    :: cond
  LOGICAL                          :: assertTrue
  res%assertions = res%assertions + 1
  IF(.NOT. cond) THEN
    res%failures = res%failures + 1
  END IF
  assertTrue = .NOT. cond;
  RETURN
END FUNCTION assertTrue

FUNCTION assertFalse(res, cond)
  TYPE(test_result), INTENT(INOUT) :: res
  LOGICAL          , INTENT(IN)    :: cond
  LOGICAL                          :: assertFalse
  assertFalse = assertTrue(res, .NOT. cond)
  RETURN
END FUNCTION assertFalse

FUNCTION is_equals_r(x, y)
  REAL     :: x, y
  LOGICAL is_equals_r
  is_equals_r = ABS(x - y) < 0.00001
  RETURN
END FUNCTION is_equals_r

END MODULE raytest
