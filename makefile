FC = gfortran
FCFLAGS = -Wall -pedantic -Jbin -g -O3 -fopenmp# TODO: colocar O3 de volta

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
	./raytracer cena1/mundo.txt cena1/pov.txt cena1/imagem.pnm 800 800 0.001 10
	./raytracer cena2/mundo.txt cena2/pov.txt cena2/imagem.pnm 800 800 0.001 10
	./raytracer cena3/mundo.txt cena3/pov.txt cena3/imagem.pnm 800 800 0.001 10
	./raytracer cena4/mundo.txt cena4/pov.txt cena4/imagem.pnm 800 800 0.001 10
	./raytracer cena5/mundo.txt cena5/pov.txt cena5/imagem.pnm 800 600 0.01  20

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
	editor -p ${SRCFILES} imagem.ppm makefile

.PHONY: all tests clean edit
