ifeq ($(shell uname -s), Darwin)
LDFLAGS=-L/opt/ports/lib -framework OpenGL -lglfw -lGLEW -lm
else
LDFLAGS=-lX11 -lGL -lGLU -lglut -lGLEW -lglfw -lm
endif

CC = g++
CPLUS = -std=c++11
CFLAGS = -g -Wall -I/opt/ports/include
SOURCES = struct.cpp object.cpp main.cpp
OBJECTS = $(SOURCES:.cpp=.o)
DEPS = struct.hpp object.hpp main.hpp
EXECUTABLE = hw7

%.o: %.cpp $(DEPS)
		$(CC) $(CPLUS) -c -o $@ $< $(CFLAGS)

$(EXECUTABLE): $(OBJECTS)
		$(CC) -o $@ $^ $(LDFLAGS) $(CFLAGS)

clean:
		rm *.o $(EXECUTABLE) core *.dSYM
