#include <stdbool.h>
#include <stdio.h>
#include <string.h>
#include <unistd.h>

#include "input_replay.h"

void InputReplay_init(InputReplay *p, const char* const replay_path, InputReplayState state) {

  p->state = state;

  if (state == INPUT_REPLAY_STATE_IDLE) {
    p->input_data = NULL;
    return;
  }

  const bool replay = state == INPUT_REPLAY_STATE_REPLAY;

  FILE *replay_file = NULL;
  ASSERT(replay_path != NULL);
  const char* mode = replay ? "rb" : "wb";
  replay_file = fopen(replay_path, mode);
  if (!replay_file)
    die("Cannot open file");

  static const char REPLAY_MAGIC[] = {'R','P','L','Y'};
  static const u8   REPLAY_VERSION = 0x01;

  if (!replay) {
    CHECK(fwrite(REPLAY_MAGIC, sizeof(REPLAY_MAGIC), 1, replay_file) == 1);
    CHECK(fwrite(&REPLAY_VERSION, sizeof(REPLAY_VERSION), 1, replay_file) == 1);

    p->replay_file = replay_file;
  }
  else if (replay) {
    char magic[4];
    u8 version;
    CHECK(fread(magic, sizeof(magic), 1, replay_file) == 1);
    CHECK(memcmp(magic, REPLAY_MAGIC, sizeof(REPLAY_MAGIC)) == 0);
    CHECK(fread(&version, sizeof(version), 1, replay_file) == 1);
    CHECK(version == REPLAY_VERSION);

    long replay_data_offset = ftell(replay_file);
    CHECK(fseek(replay_file, 0, SEEK_END) == 0);
    long file_end_offset = ftell(replay_file);
    long replay_data_size = file_end_offset - replay_data_offset;
    CHECK(fseek(replay_file, replay_data_offset, SEEK_SET) == 0);

    void *replay_data = malloc(replay_data_size);
    CHECK(replay_data != NULL);
    CHECK(fread(replay_data, replay_data_size, 1, replay_file) == 1);
    CHECK(fclose(replay_file) == 0);

    p->input_data = replay_data;
    p->input_data_index = 0;
    CHECK(replay_data_size % 2 == 0);
    p->input_data_size = replay_data_size / 2;
  }

  p->current_frame = 0;
  p->last_input_state = 0;
  p->replay_over = false;
}

void InputReplay_quit(InputReplay *p) {
  if (p->input_data != NULL) {
    free(p->input_data);
    p->input_data = NULL;
  }

  if (p->state == INPUT_REPLAY_STATE_RECORD) {
    CHECK(fclose(p->replay_file) == 0);
  }
}

void InputReplay_update(InputReplay *p, u16 *input_state) {
  switch (p->state) {

  case INPUT_REPLAY_STATE_IDLE:
    break;

  case INPUT_REPLAY_STATE_RECORD:
    if (*input_state != p->last_input_state) {
      u16 input_delta = *input_state ^ p->last_input_state;
      u16 input_frame = p->current_frame;
      CHECK(fwrite(&input_frame, sizeof(input_frame), 1, p->replay_file) == 1);
      CHECK(fwrite(&input_delta, sizeof(input_delta), 1, p->replay_file) == 1);

      p->last_input_state = *input_state;
    }
    break;

  case INPUT_REPLAY_STATE_REPLAY:
    if (p->replay_over)
      break;

    if (p->input_data_index < p->input_data_size) {
      u16 input_frame = p->input_data[p->input_data_index];
      if (p->current_frame == input_frame) {
        u16 input_delta = p->input_data[p->input_data_index + 1];
        p->input_data_index += 2;
        *input_state = p->last_input_state ^ input_delta;
        p->last_input_state = *input_state;
      }
    }
    else {
      p->replay_over = true;
    }
  }

  p->current_frame++;
}


bool InputReplay_is_over(InputReplay *p) {
  return p->replay_over;
}
