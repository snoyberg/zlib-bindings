#include <zlib.h>
#include <stdlib.h>

z_stream * create_z_stream(void)
{
	z_stream *ret = malloc(sizeof(z_stream));
	if (ret) {
		ret->zalloc = Z_NULL;
		ret->zfree = Z_NULL;
		ret->opaque = Z_NULL;
		ret->next_in = NULL;
		ret->avail_in = 0;
		ret->next_out = NULL;
		ret->avail_out = 0;
	}
	return ret;
}

z_stream * create_z_stream_inflate (int window_bits)
{
	z_stream *ret = create_z_stream();

	if (inflateInit2(ret, window_bits) != Z_OK)
	    return NULL;
	else
	    return ret;
}

void free_z_stream_inflate (z_stream *stream)
{
	inflateEnd(stream);
	free(stream);
}

void set_avail_in (z_stream *stream, char *buff, unsigned int avail)
{
	stream->next_in = buff;
	stream->avail_in = avail;
}

void set_avail_out (z_stream *stream, char *buff, unsigned int avail)
{
	stream->next_out = buff;
	stream->avail_out = avail;
}

int call_inflate_noflush (z_stream *stream)
{
	return inflate(stream, Z_NO_FLUSH);
}

unsigned int get_avail_in (z_stream *stream)
{
	return stream->avail_in;
}

unsigned int get_avail_out (z_stream *stream)
{
	return stream->avail_out;
}

z_stream * create_z_stream_deflate (int level, int window_bits)
{
	z_stream *ret = create_z_stream();

	if (deflateInit2(ret,
			 level,
			 Z_DEFLATED,
			 window_bits,
			 8,
			 Z_DEFAULT_STRATEGY)
		!= Z_OK)                 return NULL;
	else
	    return ret;
}

void free_z_stream_deflate (z_stream *stream)
{
	deflateEnd(stream);
	free(stream);
}

int call_deflate_noflush (z_stream *stream)
{
	return deflate(stream, Z_NO_FLUSH);
}

int call_deflate_finish (z_stream *stream)
{
	return deflate(stream, Z_FINISH);
}
