#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include "helper.h"

void buffer_init(buffer_t *buffer)
{
	buffer->buff = 0;
	buffer->size = buffer->used = 0;
}

int buffer_append(void *ext, unsigned char *str, size_t size)
{
	buffer_t *b = ext;
	int new_size, new_used;
	char *tmp;

	new_used = b->used + size;
	for (new_size = b->size || 8; new_size < new_used; new_size *= 2);

	if (new_size != b->size) {
		tmp = realloc(b->buff, new_size);
		if (!tmp) return 0;
		b->buff = tmp;
		b->size = new_size;
	}

	memcpy(b->buff + b->used, str, size);
	b->used = new_used;

	return 1;
}

void print_emitter_error(yaml_emitter_t *e)
{
	fprintf(stderr, "%s\n", e->problem);
}

int simple_document_start(yaml_event_t *e)
{
	return yaml_document_start_event_initialize
		(e,
		 0,
		 0,
		 0,
		 1);
}
