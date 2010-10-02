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

  ! argumentos da linha de comando
  CHARACTER(50) :: worldfile! arquivo de especificacao do mundo
  CHARACTER(50) :: povfile ! arq. com posicoes do ponto de vista e janela
  CHARACTER(50) :: outfile ! arquivo de saida
  INTEGER :: imgwidth ! largura da imagem
  INTEGER :: imgheight ! altura da imagem (0 mantem proporcao)
  REAL :: threshold ! limiar de visao
  INTEGER :: maxgen ! numero maximo de geracoes de um raio

  CALL read_commandline_args

  ! loop principal
  DO i = 1,imgheight
    DO j = 1,imgwidth
      r = RAY(pov, pixel - pov)
      color = raytrace(r, 0)
      ! plot color
    END DO
  END DO

CONTAINS

! le os argumentos da linha de comando
SUBROUTINE read_commandline_args
  CALL GETARG(1, worldfile )
  CALL GETARG(2, povfile )
  CALL GETARG(3, outfile )
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
  color = 0 + depth + r%source%x ! so pra nao dar warning
END FUNCTION

END PROGRAM
