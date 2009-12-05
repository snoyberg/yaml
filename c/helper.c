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

unsigned char * get_buffer_buff(buffer_t *b)
{
	return b->buff;
}

unsigned int get_buffer_used(buffer_t *b)
{
	return b->used;
}

void my_emitter_set_output(yaml_emitter_t *e, buffer_t *b)
{
	yaml_emitter_set_output(e, buffer_append, b);
}

unsigned char const * get_parser_error_problem(yaml_parser_t *p)
{
	return p->problem;
}

unsigned char const * get_parser_error_context(yaml_parser_t *p)
{
	return p->context;
}

unsigned int get_parser_error_offset(yaml_parser_t *p)
{
	return p->offset;
}

unsigned char const * get_emitter_error(yaml_emitter_t *e)
{
	return e->problem;
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

int get_event_type(yaml_event_t *e)
{
	return e->type;
}

unsigned char * get_scalar_value(yaml_event_t *e)
{
	return e->data.scalar.value;
}

unsigned long get_scalar_length(yaml_event_t *e)
{
	return e->data.scalar.length;
}
