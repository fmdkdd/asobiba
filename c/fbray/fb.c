#include <assert.h>
#include <errno.h>
#include <fcntl.h>
#include <linux/kd.h>
#include <linux/fb.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/ioctl.h>
#include <sys/mman.h>
#include <unistd.h>

struct screen {
  int fb_fd;
  struct fb_fix_screeninfo finfo;
  struct fb_var_screeninfo vinfo;
  uint64_t fb_size;
  uint8_t *fbp;
  uint32_t width;
  uint32_t height;

  int tty_fd;
};

// Convert RRGGBB00 color to whatever the framebuffer is using
uint32_t rgb_to_screen_color(struct fb_var_screeninfo* vinfo, uint32_t rgb) {
  uint8_t r = rgb >> 24;
  uint8_t g = rgb >> 16;
  uint8_t b = rgb >>  8;
  return (r << vinfo->red.offset) | (g << vinfo->green.offset) | (b << vinfo->blue.offset);
}

void draw_pixel(struct screen* s, uint32_t x, uint32_t y, uint32_t rgb) {

  uint64_t pixel = y * s->finfo.line_length + x * (s->vinfo.bits_per_pixel / 8);
  // Treat fbp as uin32_t* to write the whole pixel
  *((uint32_t*)(s->fbp + pixel)) = rgb_to_screen_color(&s->vinfo, rgb);
}

void screen_init(struct screen* s) {
  // Open framebuffer device
  if ((s->fb_fd = open("/dev/fb0", O_RDWR)) < 0) {
    perror("Failed to open /dev/fb0");
    exit(1);
  }

  // Get variable screen information in order to know screen resolution and
  // how much memory we need to mmap from the framebuffer
  if (ioctl(s->fb_fd, FBIOGET_VSCREENINFO, &s->vinfo) < 0) {
    perror("Failed to get VSCREENINFO");
    exit(1);
  }

  // Ensure color output and 32bpp
  s->vinfo.grayscale = 0; // 0 = color
  s->vinfo.bits_per_pixel = 32;

  // Set these values
  if (ioctl(s->fb_fd, FBIOPUT_VSCREENINFO, &s->vinfo) < 0) {
    perror("Failed to set VSCREENINFO");
    exit(1);
  }

  // Get them back to make sure they are set
  if (ioctl(s->fb_fd, FBIOGET_VSCREENINFO, &s->vinfo) < 0) {
    perror("Failed to get VSCREENINFO");
    exit(1);
  }

  assert(s->vinfo.grayscale == 0);
  assert(s->vinfo.bits_per_pixel == 32);

  // Get fixed screen information for line_length
  if (ioctl(s->fb_fd, FBIOGET_FSCREENINFO, &s->finfo) < 0) {
    perror("Failed to get FSCREENINFO");
    exit(1);
  }

  s->fb_size = s->vinfo.yres * s->finfo.line_length;

  // Dump info to screen
  printf("Screen res: %u x %u\n", s->vinfo.xres, s->vinfo.yres);
  printf("Screen res (virtual): %u x %d\n", s->vinfo.xres_virtual, s->vinfo.yres_virtual);
  printf("Framebuffer line length: %u bytes\n", s->finfo.line_length);
  printf("Framebuffer size: %ld bytes\n", s->fb_size);

  // Not sure what happens when virtual != visible resolution, but not taking
  // any risks
  assert(s->vinfo.xres == s->vinfo.xres_virtual);
  assert(s->vinfo.yres == s->vinfo.yres_virtual);

  // Map the actual framebuffer memory
  if ((s->fbp = mmap(0, s->fb_size, PROT_READ | PROT_WRITE, MAP_SHARED, s->fb_fd, (off_t)0)) == MAP_FAILED) {
    perror("Failed to mmap framebuffer");
    exit(1);
  }

  s->width = s->vinfo.xres;
  s->height = s->vinfo.yres;

  // Finally, set the TTY to graphics mode, so the prompt doesn't write to the
  // framebuffer while we are.

  // FIXME: should auto-detect tty name
  if ((s->tty_fd = open("/dev/tty2", O_RDWR)) < 0) {
    perror("Failed to open /dev/tty2");
    exit(1);
  }
  if (ioctl(s->tty_fd, KDSETMODE, KD_GRAPHICS) < 0) {
    perror("Failed to set TTY to graphics mode");
    exit(1);
  }
}

void screen_deinit(struct screen* s) {
  if (ioctl(s->tty_fd, KDSETMODE, KD_TEXT) < 0) {
    perror("Failed to restore TTY to text mode");
  }

  if (munmap(s->fbp, s->fb_size) < 0) {
    perror("Failed to munmap framebuffer");
  }

  if (close(s->fb_fd) < 0) {
    perror("Failed to close /dev/fb0");
  }
}

void clear_screen(struct screen* s) {
  // Draw floor and ceiling
  uint32_t mid_screen = s->height / 2;

  for (uint32_t x=0; x < s->width; ++x) {
    for (uint32_t y=0; y < s->height; ++y) {
      if (y > mid_screen) {
        draw_pixel(s, x, y, 0x88888800);
      } else {
        draw_pixel(s, x, y, 0x33333300);
      }
    }
  }
}

uint8_t level[8][8] = {
  {1,1,1,1,1,1,1,1},
  {1,0,0,0,0,0,0,1},
  {1,0,2,2,0,3,0,1},
  {1,0,0,0,0,3,0,1},
  {1,0,0,0,3,3,0,1},
  {1,0,0,0,0,0,0,1},
  {1,0,4,0,0,0,0,1},
  {1,1,1,1,1,1,1,1},
};

void raycast(struct screen* s) {
  // Cast one ray for each screen column, and draw the wall that the ray hits
  // depending on its distance.

  uint32_t player_x = 2;
  uint32_t player_y = 4;

  for (uint32_t x=0; x < s->width; ++x) {
    ray_march();

  }

  uint32_t wall_height[] = {0, 0, 0, 100, 110, 120, 150, 150, 120, 130, 120, 120, 120, 110, 0, 0};
  uint32_t segment_width = 40;

  uint32_t x = 0;
  for (uint32_t segment=0; segment < 16; ++segment) {
    if (wall_height[segment] > 0) {
      for (uint32_t segment_x=0; segment_x < segment_width; ++segment_x) {
        uint32_t y_start = (s->height / 2) - (wall_height[segment] / 2);
        uint32_t y_end = y_start + wall_height[segment];
        for (uint32_t y = y_start; y < y_end; ++y) {
          draw_pixel(s, x + segment_x, y, 0xBBBB0000);
        }
      }
    }
    x += segment_width;
  }
}

int main() {
  struct screen screen;
  screen_init(&screen);

  clear_screen(&screen);
  raycast(&screen);

  while (getchar() != 10) {
    sleep(1);
  }

  screen_deinit(&screen);
  return EXIT_SUCCESS;
}
