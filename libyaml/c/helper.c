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
	unsigned char *tmp;

	new_used = b->used + size;
	for (new_size = b->size ? b->size : 8; new_size < new_used; new_size *= 2);

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

char const * get_parser_error_problem(yaml_parser_t *p)
{
	return p->problem;
}

char const * get_parser_error_context(yaml_parser_t *p)
{
	return p->context;
}

const yaml_mark_t * get_parser_error_mark(yaml_parser_t *p)
{
	return &p->problem_mark;
}

char const * get_emitter_error(yaml_emitter_t *e)
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

unsigned char * get_scalar_tag(yaml_event_t *e)
{
	unsigned char *s = e->data.scalar.tag;
	if (!s) s = (unsigned char *) "";
	return s;
}

int get_scalar_style(yaml_event_t *e)
{
	return e->data.scalar.style;
}

unsigned char * get_scalar_anchor(yaml_event_t *e)
{
        return e->data.scalar.anchor;
}

unsigned char * get_sequence_start_anchor(yaml_event_t *e)
{
        return e->data.sequence_start.anchor;
}

int get_sequence_start_style(yaml_event_t *e)
{
	return e->data.sequence_start.style;
}

unsigned char * get_sequence_start_tag(yaml_event_t *e)
{
	unsigned char *s = e->data.sequence_start.tag;
	if (!s) s = (unsigned char *) "";
	return s;
}

unsigned char * get_mapping_start_anchor(yaml_event_t *e)
{
        return e->data.mapping_start.anchor;
}

int get_mapping_start_style(yaml_event_t *e)
{
	return e->data.mapping_start.style;
}

unsigned char * get_mapping_start_tag(yaml_event_t *e)
{
	unsigned char *s = e->data.mapping_start.tag;
	if (!s) s = (unsigned char *) "";
	return s;
}

unsigned char * get_alias_anchor(yaml_event_t *e)
{
        return e->data.alias.anchor;
}

const yaml_mark_t * get_start_mark(yaml_event_t *e)
{
	return &e->start_mark;
}

const yaml_mark_t * get_end_mark(yaml_event_t *e)
{
	return &e->end_mark;
}

unsigned int get_mark_index(yaml_mark_t *m)
{
	return m->index;
}

unsigned int get_mark_line(yaml_mark_t *m)
{
	return m->line;
}

unsigned int get_mark_column(yaml_mark_t *m)
{
	return m->column;
}

int yaml_parser_set_input_filename(yaml_parser_t *parser, const char *filename)
{
	FILE *in = fopen(filename, "r");
	if (!in) return 0;
	yaml_parser_set_input_file(parser, in);
	return 1;
}

int fclose_helper(FILE *file)
{
	if (! file) return 0;
	return fclose(file);
}
