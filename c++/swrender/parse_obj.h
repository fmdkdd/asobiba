#ifndef PARSE_OBJ_H
#define PARSE_OBJ_H

#include <vector>
#include "vec.h"
#include "utils.h"

struct Face {
  u32 vertexIndices[9];
};

struct Obj {
  std::vector<Vec3> vertices;
  std::vector<Vec2> textureVertices;
  std::vector<Face> faces;
};

void parse_obj(const char* buffer, usize bufferLength, Obj* obj);
void parse_obj_file(const char* filename, Obj *obj);

#endif
