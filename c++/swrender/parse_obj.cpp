#include "parse_obj.h"

#include <cerrno>
#include <cstdlib>
#include <cstdio>
#include "vec.h"

static void eat_space(const char** buffer) {
  while (**buffer == ' ')
    (*buffer)++;
}

static void skip_line(const char** buffer) {
  while (**buffer != '\n')
    (*buffer)++;
  (*buffer)++;
}

static void expect_char(const char** buffer, char c) {
  ASSERT(**buffer == c);
  (*buffer)++;
}

static f32 parse_float(const char** buffer) {
  char* endptr;
  errno = 0;
  f32 f = std::strtof(*buffer, &endptr);
  ASSERT(errno == 0);
  ASSERT(*buffer != endptr);
  *buffer = endptr;
  return f;
}

static long parse_int(const char** buffer) {
  char* endptr;
  errno = 0;
  long i = std::strtol(*buffer, &endptr, 10);
  ASSERT(errno == 0);
  ASSERT(*buffer != endptr);
  *buffer = endptr;
  return i;
}

static Vec3 parse_vec3(const char** buffer) {
  Vec3 v;

  eat_space(buffer);

  v.x = parse_float(buffer);
  eat_space(buffer);
  v.y = parse_float(buffer);
  eat_space(buffer);
  v.z = parse_float(buffer);

  return v;
}

static Face parse_face(const char** buffer) {
  Face f;

  usize idx = 0;

  for (int i=0; i < 3; ++i) {
    eat_space(buffer);
    f.vertexIndices[idx++] = ((u32)parse_int(buffer)) - 1;
    expect_char(buffer, '/');
    f.vertexIndices[idx++] = ((u32)parse_int(buffer)) - 1;
    expect_char(buffer, '/');
    f.vertexIndices[idx++] = ((u32)parse_int(buffer)) - 1;
  }

  ASSERT(idx == 9);

  return f;
}

void parse_obj(const char* buffer, usize bufferLength, Obj* obj) {

  const char* bufferEnd = buffer + bufferLength;

  while (buffer < bufferEnd) {
    eat_space(&buffer);

    if (buffer[0] == '#') {
      skip_line(&buffer);
    }
    else if (buffer[0] == '\n') {
      skip_line(&buffer);
    }
    else if (buffer[0] == 'v') {
      if (buffer[1] == 't') {
        expect_char(&buffer, 'v');
        expect_char(&buffer, 't');
        Vec3 v = parse_vec3(&buffer);
        obj->textureVertices.push_back(Vec2(v.x, v.y));
        skip_line(&buffer);
      }
      else if (buffer[1] == 'n') {
        skip_line(&buffer);
      }
      else if (buffer[1] == ' ') {
        expect_char(&buffer, 'v');
        Vec3 v = parse_vec3(&buffer);
        obj->vertices.push_back(v);
        skip_line(&buffer);
      }
      else
        UNREACHABLE();
    }
    else if (buffer[0] == 'g') {
      skip_line(&buffer);
    }
    else if (buffer[0] == 's') {
      skip_line(&buffer);
    }
    else if (buffer[0] == 'f') {
      expect_char(&buffer, 'f');
      Face f = parse_face(&buffer);
      obj->faces.push_back(f);
      skip_line(&buffer);
    }
  }

  printf("obj: %lu vertices %lu texture vertices %lu faces\n", obj->vertices.size(),
         obj->textureVertices.size(),
         obj->faces.size());

  ASSERT(buffer == bufferEnd);
}

void parse_obj_file(const char* filename, Obj *obj) {
  FILE* objFile = fopen(filename, "r");
  ENSURE(objFile != NULL);

  ENSURE(fseek(objFile, 0, SEEK_END) == 0);
  long fileSize = ftell(objFile);
  ENSURE(fseek(objFile, 0, SEEK_SET) == 0);

  char *objData = (char*)malloc(fileSize);
  ENSURE(objData != NULL);
  ENSURE(fread(objData, fileSize, 1, objFile) == 1);
  ENSURE(fclose(objFile) == 0);

  parse_obj(objData, fileSize, obj);

  free(objData);
}

#undef EXPECT
