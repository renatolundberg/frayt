MODULE rayforms
  USE raymath
  IMPLICIT NONE

  ! TODO: garantir que direction seja unitario
  TYPE ray
    TYPE(vector) source
    TYPE(vector) direction
  END TYPE ray

  TYPE intersection
    LOGICAL intersects
    TYPE(vector) point
  END TYPE

  !Triângulo
  TYPE triangle
     TYPE(vector) a    !ponto inicial
     TYPE(vector) u, v !lados do triângulo, em ordem anti-horária vistos do lado externo da face (\./ seria vau)
     TYPE(vector) n    !vetor normal do triângulo, 
     REAL uu, uv, vv !produtos escalares dos vetores dos lados
  END TYPE triangle

  !Esfera
  TYPE sphere
     TYPE(vector) c   !ponto central
     REAL r, r2       !raio e raio ao quadrado
  END TYPE sphere

  !Cilindro
  TYPE cylinder
     TYPE(vector) s, i   !pontos, centros da face superior e inferior
     REAL r              !raio
  END TYPE cylinder
  
  !Cone
  TYPE cone
     TYPE(vector) s, i    !pontos, centros da face superior e inferior
     REAL rs, ri          !raios do cone na face superior e inferior
  END TYPE cone

  INTEGER, PARAMETER :: TP_TRIANGLE = 0
  INTEGER, PARAMETER :: TP_SPHERE = 1
  INTEGER, PARAMETER :: TP_CYLINDER = 2
  INTEGER, PARAMETER :: TP_CONE = 3
  REAL, PARAMETER    :: SMALL_NUM = 0.0000001

  TYPE geom_form
    INTEGER tp
    TYPE(triangle) :: triangle
    TYPE(sphere)   :: sphere
    TYPE(cylinder) :: cylinder
    TYPE(cone)     :: cone
    REAL, DIMENSION(3) :: luminosity, reflection, transparency
    REAL :: refraction
  END TYPE geom_form
CONTAINS


! constroi um triangulo
PURE FUNCTION create_triangle(a, u, v)
  TYPE(vector), INTENT(IN) :: a, u, v
  TYPE(geom_form) create_triangle
  create_triangle%tp = TP_TRIANGLE
  create_triangle%triangle%a = a
  create_triangle%triangle%u = u
  create_triangle%triangle%v = v
  create_triangle%triangle%n = vector_cross_product(v, u)
  create_triangle%triangle%uu = vector_dot_product(u, u)
  create_triangle%triangle%uv = vector_dot_product(u, v)
  create_triangle%triangle%vv = vector_dot_product(v, v)
  RETURN
END FUNCTION create_triangle


! constroi uma esfera
!PURE FUNCTION create_sphere(c, r)
!  TYPE(vector), INTENT(IN) :: c
!  REAL, INTENT(IN) :: r
!  TYPE(geom_form) :: create_sphere
!  create_sphere%tp = TP_SPHERE
!  create_sphere%sphere%c = c
!  create_sphere%sphere%r = r
!  create_sphere%sphere%r2 = r * r
!  RETURN
!END FUNCTION create_sphere


! encontra a interseccao de um raio com uma forma geometrica
PURE FUNCTION find_intersection(f, r) 
  TYPE(geom_form), INTENT(IN) :: f
  TYPE(ray), INTENT(IN) :: r
  TYPE(intersection) find_intersection
  SELECT CASE (f%tp)
    CASE (TP_TRIANGLE)
      find_intersection = find_intersection_triangle(f, r)
    CASE (TP_SPHERE)
      find_intersection = find_intersection_sphere(f, r)
!    CASE (TP_CYLINDER)
!      find_intersection = find_intersection_cylinder(f, r)
!    CASE (TP_CONE)
!      find_intersection = find_intersection_cone(f, r)
  END SELECT
END FUNCTION find_intersection


!encontra interseccoes entre um raio e um triangulo
!algoritmo baseado no codigo original em C++ de Dan Sunday
!pode ser encontrado em http://softsurfer.com/Archive/algorithm_0105/algorithm_0105.htm
!Copyright 2001, softSurfer (www.softsurfer.com)
!This code may be freely used and modified for any purpose
!providing that this copyright notice is included with it.
!SoftSurfer makes no warranty for this code, and cannot be held
!liable for any real or imagined damage resulting from its use.
!Users of this code must verify correctness for their application.
PURE FUNCTION find_intersection_triangle(f, r)
  TYPE(geom_form), INTENT(IN) :: f
  TYPE(ray), INTENT(IN) :: r
  TYPE(intersection) find_intersection_triangle
  REAL :: dd, a, b, uu, uv, vv, wu, wv, D, s, t
  TYPE(vector) :: v, u, n, dir, w0, w, I
  u = f%triangle%u
  v = f%triangle%v
  n = f%triangle%n
  dir = r%direction
  w0 = vector_subtract(r%source, f%triangle%a)
  a = -vector_dot_product(w0, n)
  b = vector_dot_product(dir, n)

  IF (ABS(b) < SMALL_NUM) THEN     ! raio é paralelo ao plano
    find_intersection_triangle%intersects = .FALSE.
    find_intersection_triangle%point = ZERO_VECTOR
    RETURN
  END IF

  ! obtém o ponto de interseccao do raio com o plano do triangulo
  dd = a / b;
  IF (dd < 0.0) THEN !raio vai na direcao oposta ao triangulo
    find_intersection_triangle%intersects = .FALSE.
    find_intersection_triangle%point = ZERO_VECTOR
    RETURN
  END IF

  I = vector_sum(r%source, vector_real_product(dir, dd)) !ponto de interseccao do ponto com o plano

  uu = f%triangle%uu
  uv = f%triangle%uv
  vv = f%triangle%vv
  w = vector_subtract(I, f%triangle%a)
  wu = vector_dot_product(w,u)
  wv = vector_dot_product(w,v)
  D = uv * uv - uu * vv

  s = (uv * wv - vv * wu) / D
  IF (s < 0.0 .OR. s > 1.0) THEN
    find_intersection_triangle%intersects = .FALSE.
    find_intersection_triangle%point = ZERO_VECTOR
    RETURN
  END IF
  t = (uv * wu - uu * wv) / D
  IF (t < 0.0 .OR. (s + t) > 1.0) THEN
    find_intersection_triangle%intersects = .FALSE.
    find_intersection_triangle%point = ZERO_VECTOR
    RETURN
  END IF
  find_intersection_triangle%intersects = .TRUE.
  find_intersection_triangle%point = vector_sum(vector_sum(vector_real_product(v, t), vector_real_product(u, s)), f%triangle%a)
  RETURN
END FUNCTION find_intersection_triangle


! Encontra interseccoes entre um raio e uma esfera. Baseado em:
!   http://www.devmaster.net/wiki/Ray-sphere_intersection
!
! float intersectRaySphere(const Ray &ray, const Sphere &sphere) {
!   Vec oc = sphere.c - ray.p;
!   float l2oc = dot(oc,oc);
!   if (l20c < sphere.r2) { // starts inside of the sphere
!     float tca = dot(oc, ray.d) / dot(ray.d, ray.d);
!     // omit division if ray.d is normalized --^
!     float l2hc = (sphere.r2 - l20c) / dot(ray.d, ray.d) + tca*tca;
!     // division ---------------------------^
!     return tca + sqrt(l2hc);
!   } else {
!     float tca = dot(oc, ray.d);
!     if (tca < 0) // points away from the sphere
!       return std::numeric_limits<float>::infinity();
!     float l2hc = (sphere.r2 - l20c)/dot(ray.d, ray.d) + (tca*tca);
!     // division -------------------------^
!     return l2hc > 0 ?
!       tca - sqrt(l2hc) : std::numeric_limits<float>::infinity();
!   }
!}
PURE FUNCTION find_intersection_sphere(f, r)
  TYPE(geom_form), INTENT(IN) :: f
  TYPE(ray), INTENT(IN) :: r
  TYPE(intersection) find_intersection_sphere
  TYPE(vector) :: oc
  REAL :: l2oc, tca, l2hc

  oc = f%sphere%c - r%source
  l2oc = oc.DOT.oc
  tca = oc.DOT.r%direction !TODO: garantir que o vetor direction e' unitario
  l2hc = ( f%sphere%r2 - l2oc ) + ( tca * tca )

  IF ( l2oc < f%sphere%r2 ) THEN ! o raio e' gerado dentro da esfera
    find_intersection_sphere%intersects = .TRUE.
    find_intersection_sphere%point = r%source + ( r%direction * ( tca + SQRT(l2hc) ))
    RETURN
  END IF

  IF ( tca < 0 ) THEN ! o raio aponta para o outro lado
    find_intersection_sphere%intersects = .FALSE.
    RETURN
  END IF

  IF (l2hc > 0 ) THEN ! TODO: verificar limites de erros numericos
    find_intersection_sphere%intersects = .TRUE.
    find_intersection_sphere%point = r%source + ( r%direction * ( tca + SQRT(l2hc) ))
    RETURN
  END IF

  find_intersection_sphere%intersects = .FALSE.
  RETURN
END FUNCTION find_intersection_sphere


!PURE FUNCTION find_intersection_cylinder(f, r)
!  TYPE(geom_form), INTENT(IN) :: f
!  TYPE(ray), INTENT(IN) :: r
!  TYPE(intersection) find_intersection_cylinder
!END FUNCTION find_intersection_cylinder

!PURE FUNCTION find_intersection_cone(f, r)
!  TYPE(geom_form), INTENT(IN) :: f
!  TYPE(ray), INTENT(IN) :: r
!  TYPE(intersection) find_intersection_cone
!END FUNCTION find_intersection_cone

END MODULE rayforms
