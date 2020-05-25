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

u32 SDLRenderer::text(const std::string &s, int x, int y) {
  // TODO: Use a custom String type with explicit constructor

  const u32 charWidth = font.width - 2;
  u32 pos = x;

  for (auto c: s) {
    font.draw(*this, c, pos, y);
    pos += charWidth;
  }

  return pos - x + 1;
}

SDL_Rect SDLRenderer::boxed_text(const std::string &s, u32 x, u32 y) {
  const u32 textWidth = text(s, x, y);

  const u32 textHeight = font.height;
  const u32 leftMargin = 2;
  const u32 topMargin = 2;
  const u32 rightMargin = 2;
  const u32 bottomMargin = 2;
  // TODO: use SDL_Rect
  const int box_x = x - leftMargin;
  const int box_y = y - topMargin;
  const int box_w = leftMargin + textWidth + rightMargin;
  const int box_h = topMargin + textHeight + bottomMargin;
  rect(box_x, box_y, box_w, box_h);

  return SDL_Rect {box_x, box_y, box_w, box_h};
}

bool SDLRenderer::button(const std::string &s, u32 x, u32 y) {
  // TODO: handle scale in renderer

  SDL_Rect box = boxed_text(s, x, y);

  // TODO: mouse state should be global to avoid
  // retrieving it in all buttons

  SDL_Point mouse;
  u32 buttons = SDL_GetMouseState(&mouse.x, &mouse.y);
  mouse.x /= 2;
  mouse.y /= 2;

  bool clicked = (SDL_BUTTON(SDL_BUTTON_LEFT) & buttons) && SDL_PointInRect(&mouse, &box);

  return clicked;
}
