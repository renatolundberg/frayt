COMP = gfortran
COMP_FLAGS = -O3 -Wall -pedantic -Jbin -g

tests : raymath_test rayforms_test
	bin/raymath_test
	bin/rayforms_test

rayforms_test : rayforms.o rayforms_test.o raymath.o ray_test.o
	${COMP} ${COMP_FLAGS} -o bin/rayforms_test bin/rayforms.o bin/rayforms_test.o bin/ray_test.o bin/raymath.o

rayforms.o : src/rayforms.f95 ray_test.o
	${COMP} ${COMP_FLAGS} -c src/rayforms.f95 -o bin/rayforms.o

rayforms_test.o : src/rayforms_test.f95
	${COMP} ${COMP_FLAGS} -c src/rayforms_test.f95 -o bin/rayforms_test.o

raymath_test : raymath.o raymath_test.o ray_test.o
	${COMP} ${COMP_FLAGS} -o bin/raymath_test bin/raymath.o bin/raymath_test.o bin/ray_test.o

raymath.o : src/raymath.f95
	${COMP} ${COMP_FLAGS} -c src/raymath.f95 -o bin/raymath.o

raymath_test.o : src/raymath_test.f95 ray_test.o raymath.o
	${COMP} ${COMP_FLAGS} -c src/raymath_test.f95 -o bin/raymath_test.o

ray_test.o : src/ray_test.f95
	${COMP} ${COMP_FLAGS} -c src/ray_test.f95 -o bin/ray_test.o


clean:
	rm -rf bin/*
