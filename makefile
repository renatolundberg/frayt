# gfortran
FC = gfortran
COMPILER = "gfortran"
FCFLAGS = -Wall -pedantic -Jbin -g
FCOPTFLAGS = -O3 -fopenmp -march=native -mfpmath=sse

# intel fortran
ifeq (${COMPILER},intel)
FCDIR = ~/intel/bin
FC = ${FCDIR}/ifort
FCFLAGS = -xHost
FCOPTFLAGS = -O3 -no-prec-div -static -openmp
endif

# intel fortran no pegrande
ifeq (${COMPILER},"pegrande")
FC = /opt/intel/Compiler/11.1/073/bin/intel64/ifort/bin/ifort
endif

# variaveis do programa
MODULES = bin/raymath.o bin/rayforms.o bin/raytest.o
SRCFILES = src/*

all: raytracer tests

# regra para o programa principal
intel pegrande:
	make COMPILER=$@

raytracer: bin/raytracer.o ${MODULES}
ifeq (${COMPILER},"intel")
	source ${FCDIR}/ifortvars.sh intel64
endif
	${FC} ${FCFLAGS} ${FCOPTFLAGS} -o $@ $^


# regra para o programa principal sem otimizações
noop:
	make FCOPTFLAGS=""

# regras para os testes
tests: raytracer bin/raymath_test bin/rayforms_test
	bin/raymath_test
	bin/rayforms_test

# regras para gerar as imagens
images: raytracer
	for i in cena*; do make $$i/imagem.pnm; cp $$i/imagem.pnm img/$$i.pnm; done

cena%/imagem.pnm: raytracer
	time -a -o tempo.log ./raytracer $(subst imagem.pnm,,$@)mundo.txt $(subst imagem.pnm,,$@)pov.txt $@ 800 800 0.001 10

cena9/imagem.pnm: $(subst imagem.pnm,mundo.txt,$@) $(subst imagem.pnm,pov.txt,$@) raytracer
	time -a -o tempo.log ./raytracer $(subst imagem.pnm,,$@)mundo.txt $(subst imagem.pnm,,$@)pov.txt $@ 1000 800 0.001 20

cena14/imagem.pnm: $(subst imagem.pnm,mundo.txt,$@) $(subst imagem.pnm,pov.txt,$@) raytracer
	time -a -o tempo.log ./raytracer $(subst imagem.pnm,,$@)mundo.txt $(subst imagem.pnm,,$@)pov.txt $@ 1600 1600 0.001 20

bin/%_test: bin/%_test.o ${MODULES}
ifeq (${COMPILER},"intel")
	source ${FCDIR}/ifortvars.sh intel64
endif
	${FC} ${FCFLAGS} ${FCOPTFLAGS} -o $@ $^

# regra geral para .f90 -> .o
bin/%.o: src/%.f90
ifeq (${COMPILER},"intel")
	source ${FCDIR}/ifortvars.sh intel64
endif
	${FC} ${FCFLAGS} ${FCOPTFLAGS} -c $< -o $@

# regra para os testes
bin/%_test: bin/%_test.o ${MODULES}
ifeq (${COMPILER},"intel")
	source ${FCDIR}/ifortvars.sh intel64
endif
	${FC} ${FCFLAGS} ${FCOPTFLAGS} -o $@ $^

# dependencias entre modulos
bin/rayforms.o: bin/raymath.o
bin/raymath_test.o: bin/raymath.o bin/raytest.o
bin/rayforms_test.o: ${MODULES}
bin/raytracer.o: ${MODULES}

# outras regras
clean:
	rm -rf bin/* raytracer cena*/imagem.pnm img/* *.mod

edit:
	editor -p ${SRCFILES} makefile

push: clean
	git push

debug: raytracer
	gdb --args ./$< cena5/mundo.txt cena5/pov.txt cena5/imagem.pnm 800 800 0.0001 10


.PHONY: all tests clean edit images push debug noop
