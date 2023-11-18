GCC = g++
FLAGS=
# FLAGS is useful largely for setting debug flag

ifeq ($(OS),Windows_NT)
	RM = del /Q
	RRM = del /s /Q
	EXT = .exe
	MAIN = main.exe
	BUILD = build
else
	RM = rm -f
	RRM = rm -r -f
	EXT = 
	MAIN = main
	BUILD = build/*
endif

15d4:
	${GCC} -c 2015/day4/cpp/md5.cpp -o build/md5.o 
	${GCC} 2015/day4/cpp/day4.cpp build/md5.o -o ${MAIN} ${FLAGS}

15d5:
	${GCC} 2015/day5/day5.cpp -o main

15d6:
	${GCC} -c 2015/day6/cpp/Grid.cpp -o build/Grid.o
	${GCC} 2015/day6/cpp/day6.cpp build/Grid.o  -o ${MAIN} ${FLAGS}

15d7:
	${GCC} 2015/day7/day7.cpp -o ${MAIN} ${FLAGS}

15d8:
	${GCC} 2015/day8/day8.cpp -o ${MAIN} ${FLAGS}

15d9:
	${GCC} 2015/day9/day9.cpp -o ${MAIN} ${FLAGS}

clean: 
	${RM} ${MAIN}
	${RRM} ${BUILD}
