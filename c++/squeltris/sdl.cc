#include "sdl.h"

SDLFont::SDLFont(const SDLRenderer &r, const std::string &file,
                 const int width, const int height,
                 const std::string &char_mapping)
  : texture { r.create_texture(SDLSurface(SDL_LoadBMP(file.c_str()))) },
    width {width}, height{height}
{
  for (auto i=0ul; i < char_mapping.length(); ++i) {
    unsigned char c = char_mapping[i];
    mapping[c] = i;
  }
}

void SDLFont::draw(SDLRenderer &r, unsigned char c, int x, int y) {
  int i = mapping[c];
  SDL_Rect src {i*width, 0, width, height};
  SDL_Rect dst {x, y, width, height};
  r.copy(texture, &src, &dst);
}

void SDLRenderer::text(const std::string &s, int x, int y) {
  for (auto c: s) {
    font.draw(*this, c, x, y);
    x += font.width-1;
  }
}
