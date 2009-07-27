#ifndef __HELPER_H__
#define __HELPER_H__

#include <yaml.h>

typedef struct buffer_s {
	char *buff;
	unsigned int size, used;
} buffer_t;
void buffer_init(buffer_t *buffer);
int buffer_append(void *ext, unsigned char *str, size_t size);

void print_parser_error(yaml_parser_t *p);
void print_emitter_error(yaml_emitter_t *e);
int simple_document_start(yaml_event_t *e);

#endif /* __HELPER_H__ */
