MODULE ray_test
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
  RETURN;
END FUNCTION assertTrue

END MODULE ray_test
