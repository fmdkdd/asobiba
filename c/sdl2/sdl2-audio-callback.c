#include <stdint.h>
#include <string.h>
#include <SDL.h>

int16_t data[1024];

void audio_callback(void* user_data, uint8_t* stream, int len) {
  printf("callback called: %u\n", len);
  memcpy(stream, data, len);
}

int main(int argc, char* argv[])
{
  if (SDL_Init(SDL_INIT_AUDIO) != 0) {
    SDL_Log("Unable to initialize SDL: %s\n", SDL_GetError());
    return 1;
  }

  SDL_AudioSpec want, have;
  SDL_AudioDeviceID dev;

  SDL_memset(&want, 0, sizeof(want)); /* or SDL_zero(want) */
  want.freq = 48000;
  want.format = AUDIO_S16LSB;
  want.channels = 1;
  want.samples = 1024;
  want.callback = audio_callback;

  int32_t len = want.samples * want.channels;
  int16_t tone_volume = 1000;
  int32_t period = 128;
  int32_t i;
  for (i=0; i < len; ++i) {
    if ((i/ period) % 2 == 0) {
      data[i] = tone_volume;
    } else {
      data[i] = -tone_volume;
    }
  }

  dev = SDL_OpenAudioDevice(NULL, 0, &want, &have, 0);
  if (dev == 0) {
    SDL_Log("Failed to open audio: %s", SDL_GetError());
  } else {
    SDL_Log("%d %d %d %d", want.samples, have.samples, want.size, have.size);

    SDL_PauseAudioDevice(dev, 0); /* start audio playing. */
    SDL_Delay(1000);
    SDL_CloseAudioDevice(dev);
  }

  SDL_Quit();

  return 0;
}
