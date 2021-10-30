#include <SDL2/SDL.h>
#include <math.h>

#include "parse_obj.h"
#include "parse_tga.h"
#include "utils.h"
#include "vec.h"

// Vertex color and lerp?
// Texture mapping

static void sdl_die(const char *msg) {
  SDL_Log("%s: %s\n", msg, SDL_GetError());
  exit(1);
}

void clear_screen(SDL_Surface *fb) { memset(fb->pixels, 0, fb->h * fb->pitch); }

void plot_point(SDL_Surface *fb, u32 x, u32 y, u32 color) {
  ASSERT(fb != NULL);
  ASSERT(fb->format->BytesPerPixel == 4);
  u32 *pixels = static_cast<u32 *>(fb->pixels);
  ASSERT((x < (u32)fb->w) && (y < (u32)fb->h));
  pixels[fb->w * y + x] = color;
}

void plot_line(SDL_Surface *fb, u32 x0, u32 y0, u32 x1, u32 y1, u32 color) {
  const f32 xspan = (f32)x1 - x0;
  const f32 yspan = (f32)y1 - y0;

  if (abs(xspan) > abs(yspan)) {
    const f32 ystep = yspan / xspan;
    if (x0 > x1) {
      std::swap(x0, x1);
      std::swap(y0, y1);
    }
    f32 yy = y0;

    for (u32 px = x0; px <= x1; ++px) {
      const u32 py = yy + 0.5;
      plot_point(fb, px, py, color);
      yy += ystep;
    }
  } else {
    const f32 xstep = xspan / yspan;
    if (y0 > y1) {
      std::swap(y0, y1);
      std::swap(x0, x1);
    }
    f32 xx = x0;

    for (u32 py = y0; py <= y1; ++py) {
      const u32 px = xx + 0.5;
      plot_point(fb, px, py, color);
      xx += xstep;
    }
  }
}

void plot_triangle(SDL_Surface *fb, u32 x0, u32 y0, u32 x1, u32 y1, u32 x2,
                   u32 y2, u32 color) {
  plot_line(fb, x0, y0, x1, y1, color);
  plot_line(fb, x1, y1, x2, y2, color);
  plot_line(fb, x2, y2, x0, y0, color);
}

void fill_triangle(SDL_Surface *fb, Vec3 a, Vec3 b, Vec3 c, float *zBuffer,
                   Vec2 at, Vec2 bt, Vec2 ct, TGA &tga, float lightIntensity) {
  i32 minX = floorf(std::min(a.x, std::min(b.x, c.x)));
  i32 minY = floorf(std::min(a.y, std::min(b.y, c.y)));

  i32 maxX = ceilf(std::max(a.x, std::max(b.x, c.x)));
  i32 maxY = ceilf(std::max(a.y, std::max(b.y, c.y)));

  for (i32 px = minX; px <= maxX; ++px) {
    for (i32 py = minY; py <= maxY; ++py) {
      Vec3 bary = barycentricCoordinates2(Vec2(a.x, a.y), Vec2(b.x, b.y),
                                          Vec2(c.x, c.y), Vec2(px, py));

      if (bary.x < 0.0f || bary.y < 0.0f || bary.z < 0.0f)
        continue;

      float pz = bary.x * a.z + bary.y * b.z + bary.z * c.z;
      float *zBuf = &zBuffer[py * 640 + px];

      if (pz < *zBuf) {
        *zBuf = pz;

        float pU = bary.x * at.x + bary.y * bt.x + bary.z * ct.x;
        float pV = bary.x * at.y + bary.y * bt.y + bary.z * ct.y;

        u32 tx = (u32)(pU * (float)tga.width);
        u32 ty = (u32)(pV * (float)tga.height);
        u32 colRGBA = tga.rgbaData[ty * tga.width + tx];
        Vec3 col3((colRGBA >> 24) & 0xff, (colRGBA >> 16) & 0xff,
                  (colRGBA >> 8) & 0xff);

        col3 = col3 * lightIntensity;
        u32 color = SDL_MapRGBA(fb->format, col3.x, col3.y, col3.z, 0);

        plot_point(fb, px, py, color);
      }
    }
  }
}

int main() {
  Obj obj;
  parse_obj_file("head.obj", &obj);

  TGA tga;
  parseTGAFile("african_head_diffuse.tga", &tga);

  const int window_width = 640;
  const int window_height = 640;

  if (SDL_Init(SDL_INIT_VIDEO) != 0)
    sdl_die("Unable to initialize SDL");

  SDL_Window *window =
      SDL_CreateWindow("SWRender", SDL_WINDOWPOS_UNDEFINED,
                       SDL_WINDOWPOS_UNDEFINED, window_width, window_height, 0);
  SDL_Surface *fb = SDL_GetWindowSurface(window);

  u32 frame = 0;
  const f32 pi = 3.14159265;

  float *zBuffer =
      (float *)malloc(sizeof(float) * window_width * window_height);
  ASSERT(zBuffer != NULL);
  const usize zBufferPixelCount = window_width * window_height;

  while (true) {
    bool quit = false;
    SDL_Event e;
    while (SDL_PollEvent(&e)) {
      switch (e.type) {
      case SDL_QUIT:
        quit = true;
        break;
      case SDL_KEYUP:
        if (e.key.keysym.scancode == SDL_SCANCODE_ESCAPE)
          quit = true;
        break;
      }
    }

    if (quit)
      break;

    // Update
    frame++;

    // Draw
    SDL_LockSurface(fb);
    clear_screen(fb);

    Vec3 color(255.0f, 128.0f, 64.0f);
    const Vec2 center(320, 320);
    const float scale = -300.0f;
    const Vec2 zero(0, 0);
    const f32 angle = frame * 0.1 * 2 * pi / 360;
    Vec3 lightDir(0.0f, 0.0f, -0.8f);

    // Clear Z-buffer
    for (usize i = 0; i < zBufferPixelCount; ++i)
      zBuffer[i] = 10000.0f;

    for (usize i = 0; i < obj.faces.size(); ++i) {
      Face f = obj.faces[i];

      Vec3 v0 = obj.vertices[f.vertexIndices[0]];
      Vec3 v1 = obj.vertices[f.vertexIndices[3]];
      Vec3 v2 = obj.vertices[f.vertexIndices[6]];

      v0 = v0.rotateY(angle);
      v1 = v1.rotateY(angle);
      v2 = v2.rotateY(angle);

      v0 = v0.rotateZ(angle);
      v1 = v1.rotateZ(angle);
      v2 = v2.rotateZ(angle);

      Vec2 vt0 = obj.textureVertices[f.vertexIndices[1]];
      Vec2 vt1 = obj.textureVertices[f.vertexIndices[4]];
      Vec2 vt2 = obj.textureVertices[f.vertexIndices[7]];

      Vec3 normal = (v2 - v0).crossProduct(v1 - v0).normalized();
      float lightIntensity = normal.dotProduct(lightDir);

      if (lightIntensity > 0) {
        v0 = (v0 * scale) + center;
        v1 = (v1 * scale) + center;
        v2 = (v2 * scale) + center;

        // Vec3 triangleColor = color * lightIntensity;
        // u32 col = SDL_MapRGBA(fb->format, triangleColor.x, triangleColor.y,
        //                       triangleColor.z, 0);

        fill_triangle(fb, v0, v1, v2, zBuffer, vt0, vt1, vt2, tga,
                      lightIntensity);
      }
    }

    // u32 color = SDL_MapRGBA(fb->format, 255, 128, 64, 0);

    // Vec2 triangle[] = {Vec2(-30,-30), Vec2(0, 45), Vec2(30, -30)};

    // const f32 angle = frame * 0.01 * 2*pi / 360;
    // const Vec2 center(320, 320);

    // for (size_t i=0; i < ARRAY_SIZE(triangle); ++i) {
    //   triangle[i] = triangle[i].rotateAround(Vec2(0,0), angle);
    //   triangle[i] = triangle[i] * 5;
    //   triangle[i] = triangle[i] + center;
    // }

    // plot_triangle(fb,
    //               roundf(triangle[0].x), roundf(triangle[0].y),
    //               roundf(triangle[1].x), roundf(triangle[1].y),
    //               roundf(triangle[2].x), roundf(triangle[2].y),
    //               color);

    // fill_triangle(fb,
    //               roundf(triangle[0].x), roundf(triangle[0].y),
    //               roundf(triangle[1].x), roundf(triangle[1].y),
    //               roundf(triangle[2].x), roundf(triangle[2].y),
    //               color);

    SDL_UnlockSurface(fb);

    // Swap
    SDL_UpdateWindowSurface(window);
  }

  free(zBuffer);

  SDL_DestroyWindow(window);
  SDL_Quit();

  return 0;
}
