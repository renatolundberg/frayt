MODULE ray_forms
  USE ray_math
  IMPLICIT NONE

  !Triângulo
  TYPE triangle
     TYPE(vector) a    !ponto inicial
     TYPE(vector) v, u !lados do triângulo, em ordem anti-horária vistos do lado externo da face
  END TYPE triangle

  !Esfera
  TYPE sphere
     TYPE(vector) c   !ponto central
     REAL r           !raio
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

CONTAINS

END MODULE ray_forms
