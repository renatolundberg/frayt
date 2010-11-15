FC = gfortran
FCFLAGS = -Wall -pedantic -Jbin -g -O3 -fopenmp

MODULES = bin/raymath.o bin/rayforms.o bin/raytest.o
SRCFILES = src/*

all: raytracer tests images

# regra para o programa principal
raytracer: bin/raytracer.o ${MODULES}
	${FC} ${FCFLAGS} -o $@ $^

# regras para os testes
tests: raytracer bin/raymath_test bin/rayforms_test
	bin/raymath_test
	bin/rayforms_test

# regras para gerar as imagens
images: raytracer
	for i in cena*; do make $$i/imagem.pnm; cp $$i/imagem.pnm img/$$i.pnm; done

cena%/imagem.pnm: $(subst imagem.pnm,mundo.txt,$@) $(subst imagem.pnm,pov.txt,$@) raytracer
	./raytracer $(subst imagem.pnm,,$@)mundo.txt $(subst imagem.pnm,,$@)pov.txt $@ 800 800 0.001 10

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
	rm -rf bin/* raytracer cena*/imagem.pnm img/*

edit:
	editor -p ${SRCFILES} makefile

push: clean
	git push

debug: raytracer
	gdb --args ./$< cena5/mundo.txt cena5/pov.txt cena5/imagem.pnm 800 800 0.0001 10


.PHONY: all tests clean edit images push debug
