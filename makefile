GCC = g++

2015d4:
	${GCC} -c 2015/day4/cpp/md5.cpp -o build/md5.o
	${GCC} -std=c++20 2015/day4/cpp/day4.cpp build/md5.o  -o main

2015d5:
	${GCC} 2015/day5/day5.cpp -o main

clean: 
	del main.exe /S /q *.o