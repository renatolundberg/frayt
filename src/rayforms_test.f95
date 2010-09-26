PROGRAM raymath_test
  USE raymath
  USE raytest
  USE rayforms
  IMPLICIT NONE

  TYPE(test_result) :: res = test_result(0, 0)

  CALL test_create_triangle(res)
  CALL test_intersection_triangle_center(res)
  CALL test_intersection_triangle_not_centered(res)
  CALL test_intersection_triangle_edge(res)
  CALL test_intersection_triangle_vertex(res)
  CALL test_intersection_triangle_miss(res)

  PRINT *, "Teste do modulo rayforms finalizado. Ocorreram ", res%failures, " falhas em ", res%assertions, " verificacoes."
  IF (res%failures > 0) THEN
    CALL EXIT(1)
  ELSE
    CALL EXIT(0)
  END IF
CONTAINS

SUBROUTINE test_create_triangle(res)
  TYPE(test_result)   :: res
  TYPE(vector)        :: a = vector(1, 1, 1)
  TYPE(vector)        :: u = vector(1, 2, 3)
  TYPE(vector)        :: v = vector(3, 2, 1)
  TYPE(vector)        :: en
  REAL                :: euu, euv, evv
  TYPE(geom_form)     :: r
  LOGICAL             :: failure
  en = vector_cross_product(v, u)
  euu = vector_scalar_product(u, u)
  euv = vector_scalar_product(u, v)
  evv = vector_scalar_product(v, v)
  r = create_triangle(a, u, v)
  failure = assertTrue(res, TP_TRIANGLE == r%tp)
  IF (failure) THEN
    PRINT *, "create_triangle gerou tipo. Esperado", TP_TRIANGLE, " mas encontrado ", r%tp
  END IF
  failure = assertTrue(res, is_equals_v(en, r%triangle%n))
  IF (failure) THEN
    PRINT *, "create_triangle gerou normal errada. Esperado", en, " mas encontrado ", r%triangle%n
  END IF
  failure = assertTrue(res, is_equals_r(euu, r%triangle%uu))
  IF (failure) THEN
    PRINT *, "create_triangle gerado com produto escalar uu errado. Esperado", euu, " mas encontrado ", r%triangle%uu
  END IF
  failure = assertTrue(res, is_equals_r(euv, r%triangle%uv))
  IF (failure) THEN
    PRINT *, "create_triangle gerado com produto escalar uv errado. Esperado", euv, " mas encontrado ", r%triangle%uv
  END IF
  failure = assertTrue(res, is_equals_r(evv, r%triangle%vv))
  IF (failure) THEN
    PRINT *, "create_triangle gerado com produto escalar vv errado. Esperado", evv, " mas encontrado ", r%triangle%vv
  END IF
  RETURN
END SUBROUTINE test_create_triangle

SUBROUTINE test_intersection_triangle_center(res)
  TYPE(test_result)   :: res
  TYPE(geom_form)     :: f
  TYPE(vector)        :: a =  vector( 0,-1, 1)
  TYPE(vector)        :: u =  vector( 1, 2, 0)
  TYPE(vector)        :: v =  vector(-1, 2, 0)
  TYPE(vector)        :: en = vector( 0, 0,-4)
  TYPE(vector)        :: ei = vector( 0, 0, 1)
  TYPE(ray)           :: r = ray(vector(0,0,0), vector(0,0,1))
  LOGICAL             :: failure
  f = create_triangle(a, u, v)
  failure = assertTrue(res, is_equals_v(en, f%triangle%n))
  IF (failure) THEN
    PRINT *, "create_triangle gerou normal errada. Esperado", en, " mas encontrado ", f%triangle%n
  END IF
  CALL check_intersection_success(res, "intersection_triangle_center", f, r, ei)
END SUBROUTINE test_intersection_triangle_center

SUBROUTINE test_intersection_triangle_not_centered(res)
  TYPE(test_result)   :: res
  TYPE(geom_form)     :: f
  TYPE(vector)        :: a =  vector( 0.1,-1.2, 1.1)
  TYPE(vector)        :: u =  vector( 1, 2, 0)
  TYPE(vector)        :: v =  vector(-1, 2, 0)
  TYPE(vector)        :: ei = vector( 0, 0, 1.1)
  TYPE(ray)           :: r = ray(vector(0,0,0), vector(0,0,1))
  f = create_triangle(a, u, v)
  CALL check_intersection_success(res, "intersection_triangle_not_centered", f, r, ei)
END SUBROUTINE test_intersection_triangle_not_centered

SUBROUTINE test_intersection_triangle_edge(res)
  TYPE(test_result)   :: res
  TYPE(geom_form)     :: f
  TYPE(vector)        :: a =  vector( 0,-1, 1)
  TYPE(vector)        :: u =  vector( 1, 2, 0)
  TYPE(vector)        :: v =  vector(-1, 2, 0)
  TYPE(vector)        :: ei = vector( 0, 1, 1)
  TYPE(ray)           :: r = ray(vector(0,1,0), vector(0,0,1))
  f = create_triangle(a, u, v)
  CALL check_intersection_success(res, "intersection_triangle_edge", f, r, ei)
END SUBROUTINE test_intersection_triangle_edge

SUBROUTINE test_intersection_triangle_vertex(res)
  TYPE(test_result)   :: res
  TYPE(geom_form)     :: f
  TYPE(vector)        :: a =  vector( 0,-1, 1)
  TYPE(vector)        :: u =  vector( 1, 2, 0)
  TYPE(vector)        :: v =  vector(-1, 2, 0)
  TYPE(vector)        :: ei = vector( 1, 1, 1)
  TYPE(ray)           :: r = ray(vector(1,1,0), vector(0,0,1))
  f = create_triangle(a, u, v)
  CALL check_intersection_success(res, "intersection_triangle_vertex", f, r, ei)
END SUBROUTINE test_intersection_triangle_vertex


SUBROUTINE test_intersection_triangle_miss(res)
  TYPE(test_result)   :: res
  TYPE(geom_form)     :: f
  TYPE(vector)        :: a =  vector( 0,-1, 1)
  TYPE(vector)        :: u =  vector( 1, 2, 0)
  TYPE(vector)        :: v =  vector(-1, 2, 0)
  TYPE(ray)           :: r1 = ray(vector(1.1,1,0), vector(0,0,1))
  TYPE(ray)           :: r2 = ray(vector(-1.1,1,0), vector(0,0,1))
  TYPE(ray)           :: r3 = ray(vector(1,1.1,0), vector(0,0,1))
  TYPE(ray)           :: r4 = ray(vector(0,0,0), vector(0,0,-1))
  TYPE(ray)           :: r5 = ray(vector(0,0,0), vector(-1,-1,1))
  f = create_triangle(a, u, v)
  CALL check_intersection_miss(res, "intersection_triangle_miss r1", f, r1)
  CALL check_intersection_miss(res, "intersection_triangle_miss r2", f, r2)
  CALL check_intersection_miss(res, "intersection_triangle_miss r3", f, r3)
  CALL check_intersection_miss(res, "intersection_triangle_miss r4", f, r4)
  CALL check_intersection_miss(res, "intersection_triangle_miss r5", f, r5)
END SUBROUTINE test_intersection_triangle_miss

SUBROUTINE check_intersection_success(res, prefix, f, r, ei)
  TYPE(test_result)   :: res
  CHARACTER           :: prefix
  TYPE(geom_form)     :: f
  TYPE(ray)           :: r
  TYPE(intersection)  :: i
  TYPE(vector)        :: ei
  LOGICAL             :: failure
  i = find_intersection(f, r)
  failure = assertTrue(res, i%intersects)
  IF (failure) THEN
    PRINT *, prefix, ": find_intersection nao encontrou interseccao entre ", r, " e ", f, ", mas deveria"
  END IF
  failure = assertTrue(res, is_equals_v(ei, i%point))
  IF (failure) THEN
    PRINT *, prefix, ": find_intersection encontrou ponto errado. Esperado", ei, " mas encontrado ", i%point
  END IF
END SUBROUTINE check_intersection_success

SUBROUTINE check_intersection_miss(res, prefix, f, r)
  TYPE(test_result)   :: res
  CHARACTER           :: prefix
  TYPE(geom_form)     :: f
  TYPE(ray)           :: r
  TYPE(intersection)  :: i
  LOGICAL             :: failure
  i = find_intersection(f, r)
  failure = assertFalse(res, i%intersects)
  IF (failure) THEN
    PRINT *, prefix, ": find_intersection encontrou interseccao entre ", r, " e ", f, ", mas nao deveria"
  END IF
END SUBROUTINE check_intersection_miss

FUNCTION is_equals_v(v, u)
  TYPE(vector)      :: v, u
  LOGICAL is_equals_v
  is_equals_v = is_equals_r(v%x, u%x) .AND. is_equals_r(v%y, u%y) .AND. is_equals_r(v%z, u%z)
  RETURN
END FUNCTION is_equals_v

END PROGRAM raymath_test
