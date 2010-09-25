COMP = gfortran
COMP_FLAGS = -O3 -Wall -pedantic -J bin

tests : raymath_test
	bin/raymath_test

raymath_test : raymath.o raymath_test.o
	${COMP} ${COMP_FLAGS} -o bin/raymath_test bin/raymath.o bin/raymath_test.o

raymath.o : src/raymath.f95
	${COMP} ${COMP_FLAGS} -c src/raymath.f95 -o bin/raymath.o

raymath_test.o : src/raymath_test.f95
	${COMP} ${COMP_FLAGS} -c src/raymath_test.f95 -o bin/raymath_test.o

clean:
	rm -rf bin/*
