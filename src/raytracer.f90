PROGRAM raytracer
  USE raymath
  USE rayforms
  USE raytest
  IMPLICIT NONE

  ! variaveis auxiliares
  CHARACTER(50) :: buffer
  INTEGER :: i, j, nobj
  TYPE(vector) :: color
  TYPE(ray) :: r
  TYPE(vector) :: pov, pid, pie, psd, pse
  TYPE(vector) :: pixel
  TYPE(geom_form), ALLOCATABLE :: objects(:)
  CHARACTER, ALLOCATABLE :: image(:,:,:)

  ! argumentos da linha de comando
  CHARACTER(50) :: worldfile! arquivo de especificacao do mundo
  CHARACTER(50) :: povfile ! arq. com posicoes do ponto de vista e janela
  CHARACTER(50) :: imgfile ! arquivo de saida
  INTEGER :: imgwidth ! largura da imagem
  INTEGER :: imgheight ! altura da imagem (0 mantem proporcao)
  REAL :: threshold ! limiar de visao
  INTEGER :: maxgen ! numero maximo de geracoes de um raio

  CALL read_commandline_args
  CALL read_worldfile
 !CALL list_objects
  CALL read_povfile

  ALLOCATE (image(3, imgwidth, imgheight))

  ! loop principal
!$OMP PARALLEL DO SCHEDULE(DYNAMIC, 10) &
!$OMP                                     PRIVATE(i, j, pixel, r, color) &
!$OMP                                     SHARED(image, pie, pse, pid, psd, pov, imgheight, imgwidth)
  DO i = 1,imgheight
    DO j = 1,imgwidth
      pixel = vector_to_unit(((pie - pse) * (real(i)/imgheight) + (psd - pse) * (real(j)/imgwidth) + pse) - pov)
      r = RAY(pov, pixel, ONE_VECTOR, 0)
      color = raytrace(r, -1, 1.0)
      image(1,j,i) = achar(int(min(1.0, color%v(1)) * 255.))
      image(2,j,i) = achar(int(min(1.0, color%v(2)) * 255.))
      image(3,j,i) = achar(int(min(1.0, color%v(3)) * 255.))
    END DO
  END DO
!$OMP END PARALLEL DO

  ! abre a imagem e escreve o cabecalho PPM:
  !   P6 - Portable Pixmap em Binario
  open (unit = 2, file = imgfile)
  write (2,'(A2)') 'P6'
  write (2,'(I0,A,I0)') imgwidth, ' ', imgheight
  write (2,'(A3)') '255'

!  close(2)
!  open (unit = 2, file = imgfile, form = 'UNFORMATTED', access = 'APPEND')

  DO i = 1,imgheight
    DO j = 1,imgwidth
      WRITE (2, '(A1)',advance='no') image(1, j, i)
      WRITE (2, '(A1)',advance='no') image(2, j, i)
      WRITE (2, '(A1)',advance='no') image(3, j, i)
    END DO
  END DO

  ! fecha o arquivo de imagem
  close(2)

CONTAINS

PURE FUNCTION find_ray_intersection(r, last_form_id) RESULT (inter)
  TYPE(RAY), INTENT(IN) :: r
  INTEGER, INTENT(IN) :: last_form_id
  TYPE(intersection) :: inter, ninter
  TYPE(vector) :: v
  TYPE(geom_form) :: f
  INTEGER :: i
  REAL :: inter_dist
  REAL :: ninter_dist
! calculo da interseccao
  inter_dist = 0.
  inter%intersects = .FALSE.
  DO i = 1,nobj
    IF (last_form_id /= objects(i)%id) THEN
      ninter = find_intersection(objects(i), r)
      IF (ninter%intersects) THEN
        v = ninter%point - r%source
        ninter_dist = vector_dot_product(v, v)
        IF (.NOT. inter%intersects) THEN
          inter = ninter
          inter_dist = ninter_dist
          f = objects(i)
        ELSE IF (ninter_dist < inter_dist) THEN
          inter = ninter
          inter_dist = ninter_dist
          f = objects(i)
        END IF
      END IF
    END IF
  END DO
END FUNCTION

PURE FUNCTION luminosity_color(r, inter) RESULT (color)
  TYPE(RAY), INTENT(IN) :: r
  TYPE(intersection), INTENT(IN) :: inter
  REAL :: cosine
  TYPE(vector) :: color
  cosine = - (r%direction .DOT. inter%normal)
  color = inter%form%luminosity * cosine
  color%v(1) = max(0., color%v(1))
  color%v(2) = max(0., color%v(2))
  color%v(3) = max(0., color%v(3))
END FUNCTION

PURE FUNCTION reflection_color(r, inter, last_refraction_index) RESULT (color)
  TYPE(RAY), INTENT(IN) :: r
  TYPE(intersection), INTENT(IN) :: inter
  REAL, INTENT(IN) :: last_refraction_index
  TYPE(RAY) :: refl
  TYPE(vector) :: color
  REAL p
  refl%filter = r%filter * inter%form%reflection
  IF ((refl%filter .DOT. refl%filter) < threshold) THEN
    color = ZERO_VECTOR
  ELSE
    p = vector_dot_product(inter%normal, r%direction) * 2
    refl%direction = vector_to_unit(r%direction - inter%normal * p)
    refl%source = inter%point
    refl%depth = r%depth + 1
    color = raytrace(refl, inter%form%id, last_refraction_index)
  END IF
END FUNCTION

PURE FUNCTION refraction_color(r, inter, last_refraction_index) RESULT (color)
  TYPE(RAY), INTENT(IN) :: r
  TYPE(intersection), INTENT(IN) :: inter
  REAL, INTENT(IN) :: last_refraction_index
  TYPE(RAY) :: refr
  TYPE(vector) :: color
  REAL :: cos1, cos2, n1n2
  refr%filter = r%filter * inter%form%transparency
  IF ((refr%filter .DOT. refr%filter) < threshold) THEN
    color = ZERO_VECTOR
  ELSE
    ! Lei de snell: sen(teta1)/sen(teta2) = n2/n1
    ! http://en.wikipedia.org/wiki/Snell_law
    n1n2 = last_refraction_index/inter%form%refraction
    cos1 = ((r%direction*(-1.0)) .DOT. inter%normal)
    cos2 = sqrt(1.0-(n1n2*n1n2)*(1-(cos1*cos1)))
    IF (cos1 .LE. 0) THEN
      cos2 = cos2 * (-1.0)
    END IF
    refr%direction = (r%direction*n1n2) + (inter%normal*((n1n2*cos1) - cos2))
    refr%source = inter%point
    refr%depth = r%depth + 1
    color = raytrace(refr, inter%form%id, inter%form%refraction)
  END IF
END FUNCTION

PURE FUNCTION raytrace(r, last_form_id, last_refraction_index) RESULT (color)
  TYPE(RAY), INTENT(IN) :: r
  INTEGER, INTENT(IN) :: last_form_id
  REAL, INTENT(IN) :: last_refraction_index
  TYPE(vector) :: color
  TYPE(intersection) :: inter
  inter = find_ray_intersection(r, last_form_id)
  IF (inter%intersects) THEN
    color = luminosity_color(r, inter)
    IF (r%depth < maxgen) THEN
      color = color + reflection_color(r, inter, last_refraction_index)
      color = color + refraction_color(r, inter, last_refraction_index)
    END IF
    color = r%filter * color
  ELSE
    color = ZERO_VECTOR
  END IF
END FUNCTION

! le os argumentos da linha de comando
SUBROUTINE read_commandline_args
  CALL GETARG(1, worldfile )
  CALL GETARG(2, povfile )
  CALL GETARG(3, imgfile )
  CALL GETARG(4, buffer)
  READ(buffer, *) imgwidth
  CALL GETARG(5, buffer)
  READ(buffer, *) imgheight
  CALL GETARG(6, buffer)
  READ(buffer, *) threshold
  CALL GETARG(7, buffer)
  READ(buffer, *) maxgen
END SUBROUTINE read_commandline_args

! le o arquivo com informacoes sobre ponto de vista e janela
SUBROUTINE read_povfile
  REAL x, y, z
  OPEN (unit = 1, file = povfile)
  READ (1,*) x,y,z
  pov = vector((/x,y,z/))
  READ (1,*) x,y,z
  pie = vector((/x,y,z/))
  READ (1,*) x,y,z
  pse = vector((/x,y,z/))
  READ (1,*) x,y,z
  psd = vector((/x,y,z/))
  READ (1,*) x,y,z
  pid = vector((/x,y,z/))
  CLOSE (1)
END SUBROUTINE

! le o arquivo de entrada com a descricao do mundo
SUBROUTINE read_worldfile
  TYPE(vector) :: a, b, c, luminosity, reflection, transparency
  INTEGER :: ios = 0, formtype, i = 0
  REAL x, y, z, r, refraction
  OPEN (unit = 1, file = worldfile)
  READ (1,*) nobj
  ALLOCATE (objects(nobj))
  objread: DO
    i = i+1
    ! faz a leitura dos dados genericos para uma forma qualquer
    READ (1, *, iostat=ios) formtype
    IF (ios < 0) EXIT objread ! interrompe a leitura caso encontre EOF
    objects(i)%tp = formtype
    READ (1,*) x,y,z
    luminosity = vector((/x,y,z/))
    READ (1,*) x,y,z
    reflection = vector((/x,y,z/))
    READ (1,*) x,y,z
    transparency = vector((/x,y,z/))
    READ (1, *) refraction
    ! le os dados especificos de cada forma
    SELECT CASE (formtype)
      CASE (0) ! triangulo
        READ (1,*) x,y,z
        a = vector((/x,y,z/))
        READ (1,*) x,y,z
        b = vector((/x,y,z/))
        READ (1,*) x,y,z
        c = vector((/x,y,z/))
        objects(i) = create_triangle(b, c - b, a - b)
      CASE (1) ! esfera
        READ (1,*) r, x,y,z
        objects(i) = create_sphere(r, vector((/x,y,z/)))
      CASE DEFAULT
        PRINT *, 'Form type "',formtype,'" not recognized.'
        CALL EXIT(1)
    END SELECT
    objects(i)%luminosity = luminosity
    objects(i)%reflection = reflection
    objects(i)%transparency = transparency
    objects(i)%refraction = refraction
  END DO objread
  CLOSE (1)
END SUBROUTINE

SUBROUTINE list_objects
  INTEGER :: i
  DO i = 1,nobj
    PRINT *, 'OBJ ', i
    PRINT *, '  luminosity: ', objects(i)%luminosity%v(1), objects(i)%luminosity%v(2), objects(i)%luminosity%v(3)
    PRINT *, '  reflection: ', objects(i)%reflection%v(1), objects(i)%reflection%v(2), objects(i)%reflection%v(3)
    PRINT *, '  transparency: ', objects(i)%transparency%v(1), objects(i)%transparency%v(2), objects(i)%transparency%v(3)
    PRINT *, '  refraction: ', objects(i)%refraction
    SELECT CASE (objects(i)%tp)
      CASE (0) ! triangulo
        PRINT *, '  a: ', objects(i)%triangle%a
        PRINT *, '  u: ', objects(i)%triangle%u
        PRINT *, '  v: ', objects(i)%triangle%v
      CASE (1) ! esfera
        PRINT *, '  r: ', objects(i)%sphere%r
        PRINT *, '  c: ', objects(i)%sphere%c
    END SELECT
  END DO
END SUBROUTINE

! escreve a imagem no arquivo de saida
SUBROUTINE write_imgfile

END SUBROUTINE


! escreve

END PROGRAM
