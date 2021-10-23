#include <math.h>
#include <SDL2/SDL.h>

#include "parse_obj.h"
#include "vec.h"
#include "utils.h"

// Vertex color and lerp?
// Texture mapping

static void sdl_die(const char *msg) {
  SDL_Log("%s: %s\n", msg, SDL_GetError());
  exit(1);
}

void clear_screen(SDL_Surface *fb) {
  memset(fb->pixels, 0, fb->h * fb->pitch);
}

void plot_point(SDL_Surface *fb, u32 x, u32 y, u32 color) {
  ASSERT(fb != NULL);
  ASSERT(fb->format->BytesPerPixel == 4);
  u32* pixels = static_cast<u32*>(fb->pixels);
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

void plot_triangle(SDL_Surface *fb, u32 x0, u32 y0, u32 x1, u32 y1, u32 x2, u32 y2, u32 color) {
  plot_line(fb, x0, y0, x1, y1, color);
  plot_line(fb, x1, y1, x2, y2, color);
  plot_line(fb, x2, y2, x0, y0, color);
}

void fill_triangle(SDL_Surface *fb, u32 x0, u32 y0, u32 x1, u32 y1, u32 x2, u32 y2, u32 color) {
  // Point with smallest y is p0
  if (y1 < y0 && y1 < y2) {
    std::swap(x0, x1);
    std::swap(y0, y1);
  }
  else if (y2 < y0 && y2 < y1) {
    std::swap(x0, x2);
    std::swap(y0, y2);
  }

  // Point with largest y is p2
  if (y1 > y2) {
    std::swap(x1, x2);
    std::swap(y1, y2);
  }

  f32 dx1 = ((f32)x1 - x0) / (y1 - y0);
  f32 dx2 = ((f32)x2 - x0) / (y2 - y0);

  f32 xx1 = x0;
  f32 xx2 = x0;

  for (u32 l = y0; l <= y2; ++l) {
    plot_line(fb, roundf(xx1), l, roundf(xx2), l, color);

    if (l == y1) {
      xx1 = x1;
      dx1 = ((f32)x2 - x1) / (y2 - y1);
    }

    xx1 += dx1;
    xx2 += dx2;
  }

  // Some pixels are missing on the border, maybe due to imprecision?
  // This draws over them
  plot_triangle(fb, x0, y0, x1, y1, x2, y2, color);
}

int main() {
  // Parse obj
  FILE* objFile = fopen("head.obj", "r");
  ENSURE(objFile != NULL);
  ENSURE(fseek(objFile, 0, SEEK_END) == 0);
  long fileSize = ftell(objFile);
  ENSURE(fseek(objFile, 0, SEEK_SET) == 0);

  char *objData = (char*)malloc(fileSize);
  ENSURE(objData != NULL);
  ENSURE(fread(objData, fileSize, 1, objFile) == 1);
  ENSURE(fclose(objFile) == 0);

  Obj obj;
  parse_obj(objData, fileSize, &obj);

  const int window_width = 640;
  const int window_height = 640;

  if (SDL_Init(SDL_INIT_VIDEO) != 0)
    sdl_die("Unable to initialize SDL");

  SDL_Window *window = SDL_CreateWindow("SWRender", SDL_WINDOWPOS_UNDEFINED, SDL_WINDOWPOS_UNDEFINED,
                                        window_width, window_height, 0);
  SDL_Surface* fb = SDL_GetWindowSurface(window);

  u32 frame = 0;
  const f32 pi = 3.14159265;

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

    u32 color = SDL_MapRGBA(fb->format, 255, 128, 64, 0);
    const Vec2 center(320, 320);
    const float scale = -300.0f;
    const Vec2 zero(0,0);
    const f32 angle = frame * 0.01 * 2*pi / 360;

    for (Face f : obj.faces) {
      Vec3 v0 = obj.vertices[f.vertexIndices[0]];
      Vec3 v1 = obj.vertices[f.vertexIndices[3]];
      Vec3 v2 = obj.vertices[f.vertexIndices[6]];

      v0 = (v0.rotateY(angle) * scale) + center;
      v1 = (v1.rotateY(angle) * scale) + center;
      v2 = (v2.rotateY(angle) * scale) + center;

      plot_triangle(fb,
                    roundf(v0.x), roundf(v0.y),
                    roundf(v1.x), roundf(v1.y),
                    roundf(v2.x), roundf(v2.y),
                    color);
    }

    //u32 color = SDL_MapRGBA(fb->format, 255, 128, 64, 0);

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

    // c = SDL_MapRGBA(fb->format, 128, 255, 64, 0);
    // fill_triangle(fb,
    //               x + triangle[0], y + triangle[1],
    //               x + triangle[2], y + triangle[3],
    //               x + triangle[4], y + triangle[5],
    //               c);


    SDL_UnlockSurface(fb);

    // Swap
    SDL_UpdateWindowSurface(window);
  }

  SDL_DestroyWindow(window);
  SDL_Quit();

  return 0;
}
