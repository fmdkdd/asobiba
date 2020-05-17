#ifndef INPUT_REPLAY_H
#define INPUT_REPLAY_H

#include "common.h"

#include <stdbool.h>

typedef enum
  {
   INPUT_REPLAY_STATE_IDLE,
   INPUT_REPLAY_STATE_REPLAY,
   INPUT_REPLAY_STATE_RECORD,
  } InputReplayState;

typedef struct
{
  InputReplayState state;

  FILE *replay_file;

  u16 *input_data;
  size_t input_data_index;
  size_t input_data_size;

  u16 current_frame;
  u16 last_input_state;
  bool replay_over;

} InputReplay;

void InputReplay_init(InputReplay *p, const char* const replay_path, InputReplayState state);
void InputReplay_update(InputReplay *p, u16 *input_state);
void InputReplay_quit(InputReplay *p);
bool InputReplay_is_over(InputReplay *p);

#endif
