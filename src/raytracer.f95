PROGRAM raytracer
  USE raymath
  USE rayforms
  USE raytest
  IMPLICIT NONE

  ! variaveis auxiliares
  CHARACTER(50) :: buffer
  INTEGER :: i, j, color
  TYPE(RAY) :: r
  TYPE(VECTOR) :: pov
  TYPE(VECTOR) :: pixel
  TYPE(GEOM_FORM), DIMENSION(100) :: objects

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
  CALL read_povfile

  ! loop principal
  DO i = 1,imgheight
    DO j = 1,imgwidth
      r = RAY(pov, pixel - pov)
      color = raytrace(r, 0)
      ! plot color
    END DO
  END DO

  CALL write_imgfile

CONTAINS


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


! programa principal de tracing de um raio
FUNCTION raytrace(r, depth) RESULT (color)
  TYPE(RAY), INTENT(IN) :: r
  INTEGER, INTENT(IN) :: depth
  INTEGER :: color
  color = 0 + depth + r%source%v(1) ! so pra nao dar warning
END FUNCTION


! le o arquivo de entrada com a descricao do mundo
SUBROUTINE read_worldfile
  TYPE(vector) :: a, u, v
  INTEGER :: ios = 0, formtype, i
  REAL x, y, z
  OPEN (unit = 1, file = worldfile)
  objread: DO
    i = i+1
    ! faz a leitura dos dados genericos para uma forma qualquer
    READ (1, *, iostat=ios) formtype
    IF (ios < 0) EXIT objread ! interrompe a leitura caso encontre EOF
    objects(i)%tp = formtype
    READ (1, *) objects(i)%luminosity(1), objects(i)%luminosity(2), objects(i)%luminosity(3)
    READ (1, *) objects(i)%reflection(1), objects(i)%reflection(2), objects(i)%reflection(3)
    READ (1, *) objects(i)%transparency(1), objects(i)%transparency(2), objects(i)%transparency(3)
    READ (1, *) objects(i)%refraction
    ! le os dados especificos de cada forma
    SELECT CASE (formtype)
      CASE (0) ! triangulo
        READ (1,*) x,y,z
        a = vector((/x,y,z/))
        READ (1,*) x,y,z
        u = vector((/x,y,z/))
        READ (1,*) x,y,z
        v = vector((/x,y,z/))
        objects(i) = create_triangle(a,u,v)
      CASE (1) ! esfera
        READ (1,*) objects(i)%sphere%r, x,y,z
        objects(i)%sphere%c = vector((/x,y,z/))
      CASE (2) ! cilindro
        READ (1,*) x,y,z
        objects(i)%cylinder%s = vector((/x,y,z/))
        READ (1,*) x,y,z
        objects(i)%cylinder%i = vector((/x,y,z/))
        READ (1,*) objects(i)%cylinder%r
      CASE (3) ! cone
        READ (1,*) x,y,z
        objects(i)%cone%s = vector((/x,y,z/))
        READ (1,*) x,y,z
        objects(i)%cone%i = vector((/x,y,z/))
        READ (1,*) objects(i)%cone%rs, objects(i)%cone%ri
      CASE DEFAULT
        PRINT *, 'Form type "',formtype,'" not recognized.'
        CALL EXIT(1)
    END SELECT
  END DO objread
  CLOSE (1)
END SUBROUTINE


! le o arquivo com informacoes sobre ponto de vista e janela
SUBROUTINE read_povfile

END SUBROUTINE


! escreve a imagem no arquivo de saida
SUBROUTINE write_imgfile

END SUBROUTINE


! escreve

END PROGRAM
