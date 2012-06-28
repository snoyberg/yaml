CC = gcc
CFLAGS = -shared
OBJECTS = api.o dumper.o emitter.o loader.o parser.o reader.o scanner.o writer.o
LIB = libyaml.so
LFLAGS = -shared -Wl,-soname,libyaml.so
%.o: %.c
	$(CC) $(CFLAGS) -c -fPIC $<
yaml: $(OBJECTS)
	$(CC) $(LFLAGS) -o $(LIB) $(OBJECTS)
