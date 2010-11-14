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
  INTEGER, ALLOCATABLE :: image(:,:,:)

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
! CALL list_objects
  CALL read_povfile

  ALLOCATE (image(3, imgwidth, imgheight))

  ! loop principal
!$OMP PARALLEL DO SCHEDULE(DYNAMIC, 10) PRIVATE(i, j, pixel, r, color) SHARED(image, pie, pse, pid, psd, pov, imgheight, imgwidth)
  DO i = 1,imgheight
    DO j = 1,imgwidth
      pixel = vector_to_unit(((pie - pse) * (real(i)/imgheight) + (psd - pse) * (real(j)/imgwidth) + pse) - pov)
      r = RAY(pov, pixel, ONE_VECTOR, 0)
      color = raytrace(r, -1)
      image(1,j,i) = int(color%v(1) * 255.)
      image(2,j,i) = int(color%v(2) * 255.)
      image(3,j,i) = int(color%v(3) * 255.)
    END DO
  END DO
!$OMP END PARALLEL DO

  ! abre a imagem e escreve o cabecalho PPM:
  !   P6 - Portable Pixmap em Binario
  open (unit = 2, file = imgfile)
  write (2,'(A2)') 'P6'
  write (2,'(I0,A,I0)') imgwidth, ' ', imgheight
  write (2,'(A3)') '255'

  DO i = 1,imgheight
    DO j = 1,imgwidth
      write (2,'(3A1)',advance='no') achar(image(1, j, i)), achar(image(2, j, i)), achar(image(3, j, i))
    END DO
    !write (2,*)
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
  REAL :: inter_dist, ninter_dist
! calculo da interseccao
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

PURE FUNCTION reflection_color(r, inter) RESULT (color)
  TYPE(RAY), INTENT(IN) :: r
  TYPE(intersection), INTENT(IN) :: inter
  TYPE(RAY) :: refl
  TYPE(vector) :: color
  REAL p
  p = vector_dot_product(inter%normal, r%direction) * 2
  refl%direction = vector_to_unit(r%direction - inter%normal * p)
  refl%source = inter%point
  refl%depth = r%depth + 1
  refl%filter = r%filter * inter%form%reflection
  color = raytrace(refl, inter%form%id)
END FUNCTION

PURE FUNCTION refraction_color(r, inter) RESULT (color)
  TYPE(RAY), INTENT(IN) :: r
  TYPE(intersection), INTENT(IN) :: inter
  TYPE(RAY) :: refr
  TYPE(vector) :: color
! TODO calcular o efeito da refracao!!!!!
  refr%direction = r%direction
  refr%source = inter%point
  refr%depth = r%depth + 1
  refr%filter = r%filter * inter%form%transparency
  color = raytrace(refr, inter%form%id)
END FUNCTION

PURE FUNCTION raytrace(r, last_form_id) RESULT (color)
  TYPE(RAY), INTENT(IN) :: r
  INTEGER, INTENT(IN) :: last_form_id
  TYPE(vector) :: color, lum, refl, refr
  TYPE(intersection) :: inter
  IF (r%depth > maxgen .OR. (r%filter .DOT. r%filter) < threshold) THEN
    color = ZERO_VECTOR
  ELSE
    inter = find_ray_intersection(r, last_form_id)
    IF (inter%intersects) THEN
      lum = luminosity_color(r, inter)
      refl = reflection_color(r, inter)
      refr = refraction_color(r, inter)
      color = r%filter * (lum + refl + refr)
    ELSE
      color = ZERO_VECTOR
    END IF
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
  pov = vector((/x,y,z,0./))
  READ (1,*) x,y,z
  pie = vector((/x,y,z,0./))
  READ (1,*) x,y,z
  pse = vector((/x,y,z,0./))
  READ (1,*) x,y,z
  psd = vector((/x,y,z,0./))
  READ (1,*) x,y,z
  pid = vector((/x,y,z,0./))
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
    luminosity = vector((/x,y,z,0./))
    READ (1,*) x,y,z
    reflection = vector((/x,y,z,0./))
    READ (1,*) x,y,z
    transparency = vector((/x,y,z,0./))
    READ (1, *) refraction
    ! le os dados especificos de cada forma
    SELECT CASE (formtype)
      CASE (0) ! triangulo
        READ (1,*) x,y,z
        a = vector((/x,y,z,0./))
        READ (1,*) x,y,z
        b = vector((/x,y,z,0./))
        READ (1,*) x,y,z
        c = vector((/x,y,z,0./))
        objects(i) = create_triangle(b, c - b, a - b)
      CASE (1) ! esfera
        READ (1,*) r, x,y,z
        objects(i) = create_sphere(r, vector((/x,y,z,0./)))
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
