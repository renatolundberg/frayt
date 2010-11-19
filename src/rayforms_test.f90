PROGRAM rayforms_test
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
  CALL test_intersection_sphere_center_outside(res)
  CALL test_intersection_sphere_uncentered_outside(res)
  CALL test_intersection_sphere_center_inner(res)

  PRINT *, "Teste do modulo rayforms finalizado. Ocorreram ", res%failures, " falhas em ", res%assertions, " verificacoes."
  IF (res%failures > 0) THEN
    CALL EXIT(1)
  ELSE
    CALL EXIT(0)
  END IF
CONTAINS

SUBROUTINE test_create_triangle(res)
  TYPE(test_result)   :: res
  TYPE(vector)        :: a = vector((/1, 1, 1/))
  TYPE(vector)        :: u = vector((/1, 2, 3/))
  TYPE(vector)        :: v = vector((/3, 2, 1/))
  TYPE(vector)        :: en
  REAL                :: euu, euv, evv
  TYPE(geom_form)     :: r
  LOGICAL             :: failure
  en = vector_to_unit(vector_cross_product(v, u))
  euu = vector_dot_product(u, u)
  euv = vector_dot_product(u, v)
  evv = vector_dot_product(v, v)
  r = create_triangle(a, u, v)
  failure = assertTrue(res, TP_TRIANGLE == r%tp)
  IF (failure) THEN
    PRINT *, "create_triangle gerou tipo. Esperado", TP_TRIANGLE, " mas encontrado ", r%tp
  END IF
  failure = assertTrue(res, is_equal_v(en, r%triangle%n))
  IF (failure) THEN
    PRINT *, "create_triangle gerou normal errada. Esperado", en, " mas encontrado ", r%triangle%n
  END IF
  failure = assertTrue(res, is_equal_r(euu, r%triangle%uu))
  IF (failure) THEN
    PRINT *, "create_triangle gerado com produto escalar uu errado. Esperado", euu, " mas encontrado ", r%triangle%uu
  END IF
  failure = assertTrue(res, is_equal_r(euv, r%triangle%uv))
  IF (failure) THEN
    PRINT *, "create_triangle gerado com produto escalar uv errado. Esperado", euv, " mas encontrado ", r%triangle%uv
  END IF
  failure = assertTrue(res, is_equal_r(evv, r%triangle%vv))
  IF (failure) THEN
    PRINT *, "create_triangle gerado com produto escalar vv errado. Esperado", evv, " mas encontrado ", r%triangle%vv
  END IF
  RETURN
END SUBROUTINE test_create_triangle

SUBROUTINE test_intersection_triangle_center(res)
  TYPE(test_result)   :: res
  TYPE(geom_form)     :: f
  TYPE(vector)        :: a =  vector((/ 0,-1, 1/))
  TYPE(vector)        :: u =  vector((/ 1, 2, 0/))
  TYPE(vector)        :: v =  vector((/-1, 2, 0/))
  TYPE(vector)        :: ei = vector((/ 0, 0, 1/))
  TYPE(vector)        :: en = vector((/ 0, 0,-1/))
  TYPE(ray)           :: r = ray(vector((/0,0,0/)), vector((/0,0,1/)), ONE_VECTOR, 0)
  LOGICAL             :: failure
  f = create_triangle(a, u, v)
  failure = assertTrue(res, is_equal_v(en, f%triangle%n))
  IF (failure) THEN
    PRINT *, "create_triangle gerou normal errada. Esperado", en, " mas encontrado ", f%triangle%n
  END IF
  CALL check_intersection_success(res, "intersection_triangle_center", f, r, ei, en)
END SUBROUTINE test_intersection_triangle_center

SUBROUTINE test_intersection_triangle_not_centered(res)
  TYPE(test_result)   :: res
  TYPE(geom_form)     :: f
  TYPE(vector)        :: a =  vector((/ 0.1,-1.2, 1.1/))
  TYPE(vector)        :: u =  vector((/ 1, 2, 0/))
  TYPE(vector)        :: v =  vector((/-1, 2, 0/))
  TYPE(vector)        :: ei = vector((/ 0.0, 0.0, 1.1/))
  TYPE(vector)        :: en = vector((/ 0, 0, -1/))
  TYPE(ray)           :: r = ray(vector((/0.0,0.0,0.0/)), vector((/0.0,0.0,1.0/)), ONE_VECTOR, 0)
  f = create_triangle(a, u, v)
  CALL check_intersection_success(res, "intersection_triangle_not_centered", f, r, ei, en)
END SUBROUTINE test_intersection_triangle_not_centered

SUBROUTINE test_intersection_triangle_edge(res)
  TYPE(test_result)   :: res
  TYPE(geom_form)     :: f
  TYPE(vector)        :: a =  vector((/ 0,-1, 1/))
  TYPE(vector)        :: u =  vector((/ 1, 2, 0/))
  TYPE(vector)        :: v =  vector((/-1, 2, 0/))
  TYPE(vector)        :: ei = vector((/ 0, 1, 1/))
  TYPE(vector)        :: en = vector((/ 0, 0, -1/))
  TYPE(ray)           :: r = ray(vector((/0,1,0/)), vector((/0,0,1/)), ONE_VECTOR, 0)
  f = create_triangle(a, u, v)
  CALL check_intersection_success(res, "intersection_triangle_edge", f, r, ei, en)
END SUBROUTINE test_intersection_triangle_edge

SUBROUTINE test_intersection_triangle_vertex(res)
  TYPE(test_result)   :: res
  TYPE(geom_form)     :: f
  TYPE(vector)        :: a =  vector((/ 0,-1, 1/))
  TYPE(vector)        :: u =  vector((/ 1, 2, 0/))
  TYPE(vector)        :: v =  vector((/-1, 2, 0/))
  TYPE(vector)        :: ei = vector((/ 1, 1, 1/))
  TYPE(vector)        :: en = vector((/ 0, 0, -1/))
  TYPE(ray)           :: r = ray(vector((/1,1,0/)), vector((/0,0,1/)), ONE_VECTOR, 0)
  f = create_triangle(a, u, v)
  CALL check_intersection_success(res, "intersection_triangle_vertex", f, r, ei, en)
END SUBROUTINE test_intersection_triangle_vertex


SUBROUTINE test_intersection_triangle_miss(res)
  TYPE(test_result)   :: res
  TYPE(geom_form)     :: f
  TYPE(vector)        :: a =  vector((/ 0,-1, 1/))
  TYPE(vector)        :: u =  vector((/ 1, 2, 0/))
  TYPE(vector)        :: v =  vector((/-1, 2, 0/))
  TYPE(ray)           :: r1 = ray(vector((/1.1,1.0,0.0/)), vector((/0.0,0.0,1.0/)), ONE_VECTOR, 0)
  TYPE(ray)           :: r2 = ray(vector((/-1.1,1.0,0.0/)), vector((/0.0,0.0,1.0/)), ONE_VECTOR, 0)
  TYPE(ray)           :: r3 = ray(vector((/1.0,1.1,0.0/)), vector((/0.0,0.0,1.0/)), ONE_VECTOR, 0)
  TYPE(ray)           :: r4 = ray(vector((/.0,.0,.0/)), vector((/0.0,0.0,-1.0/)), ONE_VECTOR, 0)
  TYPE(ray)           :: r5 = ray(vector((/.0,.0,.0/)), vector((/-1.0,-1.0,1.0/)), ONE_VECTOR, 0)
  f = create_triangle(a, u, v)
  CALL check_intersection_miss(res, "intersection_triangle_miss r1", f, r1)
  CALL check_intersection_miss(res, "intersection_triangle_miss r2", f, r2)
  CALL check_intersection_miss(res, "intersection_triangle_miss r3", f, r3)
  CALL check_intersection_miss(res, "intersection_triangle_miss r4", f, r4)
  CALL check_intersection_miss(res, "intersection_triangle_miss r5", f, r5)
END SUBROUTINE test_intersection_triangle_miss

SUBROUTINE test_intersection_sphere_center_outside(res)
  TYPE(test_result)   :: res
  TYPE(geom_form)     :: f
  REAL                :: radius =  1
  TYPE(vector)        :: c =  vector((/ 1, 1, 1/))
  TYPE(vector)        :: ei = vector((/ 2, 1, 1/))
  TYPE(vector)        :: en = vector((/ 1, 0, 0/))
  TYPE(ray)           :: r = ray(vector((/4,1,1/)), vector((/-1,0,0/)), ONE_VECTOR, 0)
  f = create_sphere(radius, c)
  CALL check_intersection_success(res, "test_intersection_sphere_center_outside", f, r, ei, en)
END SUBROUTINE test_intersection_sphere_center_outside

SUBROUTINE test_intersection_sphere_uncentered_outside(res)
  TYPE(test_result)   :: res
  TYPE(geom_form)     :: f
  REAL                :: radius =  1
  TYPE(vector)        :: c =  vector((/ 1, 1, 1/))
  TYPE(vector)        :: ei = vector((/ 1, 2, 1/))
  TYPE(vector)        :: en = vector((/ 0, 1, 0/))
  TYPE(ray)           :: r = ray(vector((/4., 1.999999, 1./)), vector((/-1,0,0/)), ONE_VECTOR, 0)
  f = create_sphere(radius, c)
  CALL check_intersection_success(res, "test_intersection_sphere_uncentered_outside", f, r, ei, en)
END SUBROUTINE test_intersection_sphere_uncentered_outside

SUBROUTINE test_intersection_sphere_center_inner(res)
  TYPE(test_result)   :: res
  TYPE(geom_form)     :: f
  REAL                :: radius =  1
  TYPE(vector)        :: c =  vector((/ 1, 1, 1/))
  TYPE(vector)        :: ei = vector((/ 2, 1, 1/))
  TYPE(vector)        :: en = vector((/ -1, 0, 0/))
  TYPE(ray)           :: r = ray(vector((/1,1,1/)), vector((/1,0,0/)), ONE_VECTOR, 0)
  f = create_sphere(radius, c)
  CALL check_intersection_success(res, 'test_intersection_sphere_center_inner', f, r, ei, en)
END SUBROUTINE test_intersection_sphere_center_inner

SUBROUTINE check_intersection_success(res, prefix, f, r, ei, en)
  TYPE(test_result)   :: res
  CHARACTER           :: prefix*(*)
  TYPE(geom_form)     :: f
  TYPE(ray)           :: r
  TYPE(intersection)  :: i


  TYPE(vector)        :: ei, en
  LOGICAL             :: failure
  i = find_intersection(f, r)
  failure = assertTrue(res, i%intersects)
  IF (failure) THEN
    PRINT *, prefix, ": find_intersection nao encontrou interseccao entre ", r, " e ", f, ", mas deveria"
  END IF
  failure = assertTrue(res, is_equal_v(ei, i%point))
  IF (failure) THEN
    PRINT *, prefix, ": find_intersection encontrou ponto errado. Esperado", ei, " mas encontrado ", i%point
  END IF
  failure = assertTrue(res, is_equal_v(en, i%normal))
  IF (failure) THEN
    PRINT *, prefix, ": find_intersection encontrou normal errada. Esperado", en, " mas encontrado ", i%normal
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

FUNCTION is_equal_v(v, u)
  TYPE(vector)      :: v, u
  LOGICAL is_equal_v
  is_equal_v = is_equal_r(v%v(1), u%v(1)) .AND. is_equal_r(v%v(2), u%v(2)) .AND. is_equal_r(v%v(3), u%v(3))
  RETURN
END FUNCTION is_equal_v

END PROGRAM rayforms_test
