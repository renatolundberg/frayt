FC = gfortran
FCFLAGS = -Wall -pedantic -Jbin -g # TODO: colocar O3 de volta

MODULES = bin/raymath.o bin/rayforms.o bin/raytest.o
SRCFILES = src/*

all: raytracer tests

# regra para o programa principal
raytracer: bin/raytracer.o ${MODULES}
	${FC} ${FCFLAGS} -o $@ $^

# regras para os testes
tests: raytracer bin/raymath_test bin/rayforms_test
	bin/raymath_test
	bin/rayforms_test
	./raytracer doc/entrada1.txt pov.txt saida.txt 800 600 0.1 10

bin/%_test: bin/%_test.o ${MODULES}
	${FC} ${FCFLAGS} -o $@ $^

# regra geral para .f95 -> .o
bin/%.o: src/%.f95
	${FC} ${FCFLAGS} -c $< -o $@

# regra para os testes
bin/%_test: bin/%_test.o ${MODULES}
	${FC} ${FCFLAGS} -o $@ $^

# dependencias entre modulos
bin/rayforms.o: bin/raymath.o
bin/raymath_test.o: bin/raymath.o bin/raytest.o
bin/rayforms_test.o: ${MODULES}
bin/raytracer.o: ${MODULES}

# outras regras
clean:
	rm -rf bin/* raytracer

edit:
	editor -p ${SRCFILES} makefile

.PHONY: all tests clean edit
