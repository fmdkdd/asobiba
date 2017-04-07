// Following http://viewsourcecode.org/snaptoken/kilo/index.html

#define _DEFAULT_SOURCE
#define _BSD_SOURCE
#define _GNU_SOURCE

#include <ctype.h>
#include <errno.h>
#include <fcntl.h>
#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/ioctl.h>
#include <sys/types.h>
#include <termios.h>
#include <time.h>
#include <unistd.h>

//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
// Prototypes

void editor_refresh_screen();
void editor_set_status_message(const char *fmt, ...);
char *editor_prompt(char *prompt);

//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
// Defines

#define KILO_VERSION "0.0.1"
#define KILO_TAB_STOP 8
#define KILO_QUIT_TIMES 3

#define CTRL_KEY(k) ((k) & 0x1f)

// Use large values to avoid conflicting with the 8bit char range
enum editor_key {
  BACKSPACE = 127,
  ARROW_LEFT = 1000,
  ARROW_RIGHT,
  ARROW_UP,
  ARROW_DOWN,
  PAGE_UP,
  PAGE_DOWN,
  HOME_KEY,
  END_KEY,
  DEL_KEY,
};

//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
// Data

typedef struct erow {
  int size;
  int rsize;
  char *chars;
  char *render;

} erow;

struct editor_config {
  int cx, cy;
  int rx;
  int rowoff;
  int coloff;
  int screenrows;
  int screencols;
  int numrows;
  erow *row;
  int dirty;
  char *filename;
  char statusmsg[80];
  time_t statusmsg_time;
  struct termios orig_termios;
} E;

//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
// Terminal

void die(const char *s) {
  editor_refresh_screen();

  perror(s);
  exit(1);
}

void disable_raw_mode() {
  if (tcsetattr(STDIN_FILENO, TCSAFLUSH, &E.orig_termios) == -1)
    die("tcsetattr");
}

// Disable some control sequences and SIGINT being processed by the terminal
// instead of being sent to the program
void enable_raw_mode() {
  if (tcgetattr(STDIN_FILENO, &E.orig_termios) == -1) die("tcgetattr");
  atexit(disable_raw_mode);

  struct termios raw = E.orig_termios;
  raw.c_iflag &= ~(IXON | ICRNL | BRKINT | INPCK | ISTRIP);
  raw.c_oflag &= ~(OPOST);
  raw.c_cflag |= (CS8);
  raw.c_lflag &= ~(ECHO | ICANON | ISIG | IEXTEN);
  raw.c_cc[VMIN] = 0;
  raw.c_cc[VTIME] = 1;
  if (tcsetattr(STDIN_FILENO, TCSAFLUSH, &raw) == -1) die("tcsetattr");
}

int editor_read_key() {
  int nread;
  char c;
  while ((nread = read(STDIN_FILENO, &c, 1)) != 1) {
    if (nread == -1 && errno != EAGAIN) die("read");
  }

  // Handle escape sequences
  if (c == '\x1b') {
    char seq[3];

    if (read(STDIN_FILENO, &seq[0], 1) != 1) return '\x1b';
    if (read(STDIN_FILENO, &seq[1], 1) != 1) return '\x1b';

    if (seq[0] == '[') {
      if (seq[1] >= '0' && seq[1] <= '9') {
        if (read(STDIN_FILENO, &seq[2], 1) != 1) return '\x1b';
        if (seq[2] == '~') {
          switch (seq[1]) {
          case '1': return HOME_KEY;
          case '3': return DEL_KEY;
          case '4': return END_KEY;
          case '5': return PAGE_UP;
          case '6': return PAGE_DOWN;
          case '7': return HOME_KEY;
          case '8': return END_KEY;
          }
        }
      } else {
        switch (seq[1]) {
        case 'A': return ARROW_UP;
        case 'B': return ARROW_DOWN;
        case 'C': return ARROW_RIGHT;
        case 'D': return ARROW_LEFT;
        case 'H': return HOME_KEY;
        case 'F': return END_KEY;
        }
      }
    } else if (seq[0] == 'O') {
      switch (seq[1]) {
      case 'H': return HOME_KEY;
      case 'F': return END_KEY;
      }
    }

    return '\x1b';
  } else {
    return c;
  }
}

// Fallback for when ioctl fails
int get_cursor_position(int *rows, int *cols) {
  char buf[32];
  unsigned int i = 0;

  // Poke the cursor position
  if (write(STDIN_FILENO, "\x1b[6n", 4) != 4) return -1;

  // It's written back to STDIN
  while (i < sizeof(buf) - 1) {
    if (read(STDIN_FILENO, &buf[i], 1) != 1) break;
    if (buf[i] == 'R') break;
    ++i;
  }
  buf[i] = '\0';

  // In this format
  if (sscanf(&buf[0], "\x1b[%d;%d", rows, cols) != 2) return -1;

  return 0;
}

int get_window_size(int *rows, int *cols) {
  struct winsize ws;

  if (ioctl(STDOUT_FILENO, TIOCGWINSZ, &ws) == -1 || ws.ws_col == 0) {
    // If ioctl fails, put the cursor at the bottom-right as far as we can, and
    // count the rows and cols ourselves
    if (write(STDOUT_FILENO, "\x1b[999C\x1b[999B", 12) != 12) return -1;
    return get_cursor_position(rows, cols);
  } else {
    *cols = ws.ws_col;
    *rows = ws.ws_row;
    return 0;
  }
}

//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
// Row operations

// Convert index into real chars to index into rendered chars
int editor_row_cx_to_rx(erow *row, int cx) {
  int rx = 0;
  int j;
  for (j = 0; j < cx; ++j) {
    if (row->chars[j] == '\t')
      rx += (KILO_TAB_STOP - 1) - (rx % KILO_TAB_STOP);
    rx++;
  }
  return rx;
}

void editor_update_row(erow *row) {
  int tabs = 0;
  int j;
  // Count tabs
  for (j = 0; j < row->size; ++j)
    if (row->chars[j] == '\t') tabs++;

  free(row->render);
  row->render = malloc(row->size + tabs*(KILO_TAB_STOP - 1) + 1);

  int idx = 0;
  for (j = 0; j < row->size; ++j) {
    if (row->chars[j] == '\t') {
      row->render[idx++] = ' ';
      while (idx % KILO_TAB_STOP != 0) row->render[idx++] = ' ';
    } else {
      row->render[idx++] = row->chars[j];
    }
  }
  row->render[idx] = '\0';
  row->rsize = idx;
}

void editor_insert_row(int at, char *s, size_t len) {
  if (at < 0 || at > E.numrows) return;

  E.row = realloc(E.row, sizeof(erow) * (E.numrows + 1));
  memmove(&E.row[at + 1], &E.row[at], sizeof(erow) * (E.numrows - at));

  E.row[at].size = len;
  E.row[at].chars = strndup(s, len);

  E.row[at].rsize = 0;
  E.row[at].render = NULL;
  editor_update_row(&E.row[at]);

  E.numrows++;
  E.dirty++;
}

void editor_free_row(erow *row) {
  free(row->render);
  free(row->chars);
}

void editor_del_row(int at) {
  if (at < 0 || at >= E.numrows) return;
  editor_free_row(&E.row[at]);
  memmove(&E.row[at], &E.row[at + 1], sizeof(erow) * (E.numrows - at - 1));
  E.numrows--;
  E.dirty++;
}

void editor_row_insert_char(erow *row, int at, int c) {
  if (at < 0 || at > row->size) at = row->size;
  row->chars = realloc(row->chars, row->size + 2);
  memmove(&row->chars[at + 1], &row->chars[at], row->size - at + 1);
  row->size++;
  row->chars[at] = c;
  editor_update_row(row);
  E.dirty++;
}

void editor_row_append_string(erow *row, char *s, size_t len) {
  row->chars = realloc(row->chars, row->size + len + 1);
  memcpy(&row->chars[row->size], s, len);
  row->size += len;
  row->chars[row->size] = '\0';
  editor_update_row(row);
  E.dirty++;
}

void editor_row_del_char(erow *row, int at) {
  if (at < 0 || at >= row->size) return;
  memmove(&row->chars[at], &row->chars[at + 1], row->size - at);
  row->size--;
  editor_update_row(row);
  E.dirty++;
}

//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
// Editor operations

void editor_insert_char(int c) {
  if (E.cy == E.numrows) {
    editor_insert_row(E.numrows, "", 0);
  }
  editor_row_insert_char(&E.row[E.cy], E.cx, c);
  E.cx++;
}

void editor_insert_newline() {
  if (E.cx == 0) {
    editor_insert_row(E.cy, "", 0);
  } else {
    erow *row = &E.row[E.cy];
    editor_insert_row(E.cy + 1, &row->chars[E.cx], row->size - E.cx);
    row = &E.row[E.cy];
    row->size = E.cx;
    row->chars[row->size] = '\0';
    editor_update_row(row);
  }
  E.cy++;
  E.cx = 0;
}

void editor_del_char() {
  if (E.cy == E.numrows) return;
  if (E.cx == 0 && E.cy == 0) return;

  erow *row = &E.row[E.cy];
  if (E.cx > 0) {
    editor_row_del_char(row, E.cx - 1);
    E.cx--;
  } else {
    E.cx = E.row[E.cy - 1].size;
    editor_row_append_string(&E.row[E.cy - 1], row->chars, row->size);
    editor_del_row(E.cy);
    E.cy--;
  }
}

//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
// File I/O

char *editor_rows_to_string(int *buflen) {
  int totlen = 0;
  int j;
  for (j = 0; j < E.numrows; ++j)
    totlen += E.row[j].size + 1;
  *buflen = totlen;

  char *buf = malloc(totlen);
  char *p = buf;
  for (j = 0; j < E.numrows; ++j) {
    memcpy(p, E.row[j].chars, E.row[j].size);
    p += E.row[j].size;
    *p = '\n';
    p++;
  }

  return buf;
}

void editor_open(char *filename) {
  free(E.filename);
  E.filename = strdup(filename);

  FILE *fp = fopen(filename, "r");
  if (!fp) die("fopen");

  char *line = NULL;
  size_t linecap = 0;
  ssize_t linelen;
  while ((linelen = getline(&line, &linecap, fp)) != -1) {
    // Strip \n or \r since we already print an erow on its own line
    if (linelen > 0 && (line[linelen - 1] == '\n' || line[linelen - 1] == '\r'))
      linelen--;
    editor_insert_row(E.numrows, line, linelen);
  }
  free(line);
  fclose(fp);
  E.dirty = 0;
}

void editor_save() {
  if (E.filename == NULL) {
    E.filename = editor_prompt("Save as: %s");
    if (E.filename == NULL) {
      editor_set_status_message("Save aborted");
      return;
    }
  }

  int len;
  char *buf = editor_rows_to_string(&len);

  int fd = open(E.filename, O_RDWR | O_CREAT, 0644);
  if (fd != -1) {
    if (ftruncate(fd, len) != -1) {
      if (write(fd, buf, len)) {
          close(fd);
          free(buf);
          E.dirty = 0;
          editor_set_status_message("%d bytes written to disk", len);
          return;
        }
    }
    close(fd);
  }
  free(buf);
  editor_set_status_message("Can't save due to I/O error: %s", strerror(errno));
}

//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
// Append buffer

struct abuf {
  char *b;
  int len;
};

#define ABUF_INIT {NULL, 0}

void ab_append(struct abuf *ab, const char *s, int len) {
  char *new = realloc(ab->b, ab->len + len);

  if (new == NULL) return;
  memcpy(&new[ab->len], s, len);
  ab->b = new;
  ab->len += len;
}

void ab_free(struct abuf *ab) {
  free(ab->b);
}

//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
// Output

void editor_scroll() {
  E.rx = 0;
  if (E.cy < E.numrows) {
    E.rx = editor_row_cx_to_rx(&E.row[E.cy], E.cx);
  }

  if (E.cy < E.rowoff) {
    E.rowoff = E.cy;
  }
  if (E.cy >= E.rowoff + E.screenrows) {
    E.rowoff = E.cy - E.screenrows + 1;
  }
  if (E.rx < E.coloff) {
    E.coloff = E.rx;
  }
  if (E.rx >= E.coloff + E.screencols) {
    E.coloff = E.rx - E.screencols + 1;
  }
}

void editor_draw_rows(struct abuf *ab) {
  int y;
  for (y = 0; y < E.screenrows; ++y) {
    int filerow = y + E.rowoff;
    if (filerow >= E.numrows) {
      if (E.numrows == 0 && y == E.screenrows / 3) {
        char welcome[80];
        int welcomelen = snprintf(welcome, sizeof(welcome),
                                  "Kilo editor -- version %s", KILO_VERSION);
        if (welcomelen > E.screencols) welcomelen = E.screencols;
        int padding = (E.screencols - welcomelen) / 2;
        if (padding) {
          ab_append(ab, "~", 1);
          padding--;
        }
        while (padding--) ab_append(ab, " ", 1);
        ab_append(ab, welcome, welcomelen);
      } else {
        ab_append(ab, "~", 1);
      }
    } else {
      int len = E.row[filerow].rsize - E.coloff;
      if (len < 0) len = 0;
      if (len > E.screencols) len = E.screencols;
      ab_append(ab, &E.row[filerow].render[E.coloff], len);
    }

    ab_append(ab, "\x1b[K", 3); // Clear whole line
    ab_append(ab, "\r\n", 2);
  }
}

void editor_draw_status_bar(struct abuf *ab) {
  ab_append(ab, "\x1b[7m]", 4); // draw in inverted colors
  char status[80], rstatus[80];
  int len = snprintf(status, sizeof(status), "%.20s - %d lines %s",
                     E.filename ? E.filename : "[No name]", E.numrows,
                     E.dirty ? "(modified)" : "");
  int rlen = snprintf(rstatus, sizeof(rstatus), "%d/%d",
                      E.cy + 1, E.numrows);
  if (len > E.screencols) len = E.screencols;
  ab_append(ab, status, len);
  while (len < E.screencols) {
    if (E.screencols - len == rlen) {
      ab_append(ab, rstatus, rlen);
      break;
    } else {
      ab_append(ab, " ", 1);
      len++;
    }
  }
  ab_append(ab, "\x1b[m", 3);   // restore normal colors
  ab_append(ab, "\r\n", 2);     // leave space for message
}

void editor_draw_message_bar(struct abuf *ab) {
  ab_append(ab, "\x1b[K", 3);
  int msglen = strlen(E.statusmsg);
  if (msglen > E.screencols) msglen = E.screencols;
  if (msglen && time(NULL) - E.statusmsg_time < 5)
    ab_append(ab, E.statusmsg, msglen);
}

void editor_refresh_screen() {
  editor_scroll();

  // Use a buffer to avoid writing to the screen piece-wise
  struct abuf ab = ABUF_INIT;

  ab_append(&ab, "\x1b[?25l", 6); // Hide cursor so it doesn't pop when drawing on the screen
  ab_append(&ab, "\x1b[H", 3);    // Move cursor to top-left

  editor_draw_rows(&ab);
  editor_draw_status_bar(&ab);
  editor_draw_message_bar(&ab);

  // Move cursor to its saved position
  char buf[32];
  snprintf(buf, sizeof(buf), "\x1b[%d;%dH", (E.cy - E.rowoff) + 1,
                                            (E.rx - E.coloff) + 1);
  ab_append(&ab, buf, strlen(buf));

  ab_append(&ab, "\x1b[?25h", 6); // Restore cursor

  write(STDOUT_FILENO, ab.b, ab.len);
  ab_free(&ab);
}

void editor_set_status_message(const char *fmt, ...) {
  va_list ap;
  va_start(ap, fmt);
  vsnprintf(E.statusmsg, sizeof(E.statusmsg), fmt, ap);
  va_end(ap);
  E.statusmsg_time = time(NULL);
}

//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
// Input

char *editor_prompt(char *prompt) {
  size_t bufsize = 128;
  char *buf = malloc(bufsize);

  size_t buflen = 0;
  buf[0] = '\0';

  while (1) {
    editor_set_status_message(prompt, buf);
    editor_refresh_screen();

    int c = editor_read_key();
    if (c == DEL_KEY || c == CTRL_KEY('h') || c == BACKSPACE) {
      if (buflen != 0) buf[--buflen] = '\0';
    } else if (c == '\x1b') {
      editor_set_status_message("");
      free(buf);
      return NULL;
    } else if (c == '\r') {
      if (buflen != 0) {
        editor_set_status_message("");
        return buf;
      }
    } else if (!iscntrl(c)) {
      if (buflen == bufsize - 1) {
        bufsize *= 2;
        buf = realloc(buf, bufsize);
      }
      buf[buflen++] = c;
      buf[buflen] = '\0';
    }
  }
}

void editor_move_cursor(int key) {
  erow *row = (E.cy >= E.numrows) ? NULL: &E.row[E.cy];

  switch (key) {
  case ARROW_LEFT:
    if (E.cx != 0) {
      E.cx--;
    } else if (E.cy > 0) { // move to the end of the previous line
      E.cy--;
      E.cx = E.row[E.cy].size;
    }
    break;
  case ARROW_RIGHT:
    if (row && E.cx < row->size) {
      E.cx++;
    } else if (row && E.cx == row->size) { // move to the start of the next line
      E.cy++;
      E.cx = 0;
    }
    break;
  case ARROW_UP:
    if (E.cy != 0) {
      E.cy--;
    }
    break;
  case ARROW_DOWN:
    if (E.cy < E.numrows) {
      E.cy++;
    }
    break;
  }

  // Restrict E.cx to rowlen after the move
  row = (E.cy >= E.numrows) ? NULL : &E.row[E.cy];
  int rowlen = row ? row->size : 0;
  if (E.cx > rowlen) {
    E.cx = rowlen;
  }
}

void editor_process_keypress() {
  static int quit_times = KILO_QUIT_TIMES;
  int c = editor_read_key();

  switch (c) {
  case '\r':
    editor_insert_newline();
    break;

  case CTRL_KEY('q'):
    if (E.dirty && quit_times > 0) {
      editor_set_status_message("Warning: File has unsaved changes. "
                                "Press Ctrl-q %d more times to quit.", quit_times);
      quit_times--;
      return;
    }
    editor_refresh_screen();
    exit(0);
    break;

  case CTRL_KEY('s'):
    editor_save();
    break;

  case HOME_KEY:
    E.cx = 0;
    break;

  case END_KEY:
    if (E.cy < E.numrows)
      E.cx = E.row[E.cy].size;
    break;

  case BACKSPACE:
  case CTRL_KEY('h'):
  case DEL_KEY:
    if (c == DEL_KEY) editor_move_cursor(ARROW_RIGHT);
    editor_del_char();
    break;

  case PAGE_UP:
  case PAGE_DOWN:
    {
      if (c == PAGE_UP)
        E.cy = E.rowoff;
      else if (c == PAGE_DOWN)
        E.cy = E.rowoff + E.screenrows - 1;

      int times = E.screenrows;
      while (times--)
        editor_move_cursor(c == PAGE_UP ? ARROW_UP : ARROW_DOWN);
    }
    break;

  case ARROW_LEFT:
  case ARROW_RIGHT:
  case ARROW_UP:
  case ARROW_DOWN:
    editor_move_cursor(c);
    break;

  case CTRL_KEY('l'):
  case '\x1b':
    break;

  default:
    editor_insert_char(c);
    break;
  }

  quit_times = KILO_QUIT_TIMES;
}

//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
// Init

void init_editor() {
  E.cx = 0;
  E.cy = 0;
  E.rx = 0;
  E.rowoff = 0;
  E.coloff = 0;
  E.numrows = 0;
  E.row = NULL;
  E.dirty = 0;
  E.filename = NULL;
  E.statusmsg[0] = '\0';
  E.statusmsg_time = 0;

  if (get_window_size(&E.screenrows, &E.screencols) == -1) die("get_window_size");
  E.screenrows -= 2;            // leave place for status bar and message bar
}

int main(int argc, char *argv[]) {
  enable_raw_mode();
  init_editor();
  if (argc >= 2) {
    editor_open(argv[1]);
  }

  editor_set_status_message("HELP: Ctrl-S = save | Ctrl-Q = quit");

  while (1) {
    editor_refresh_screen();
    editor_process_keypress();
  }
  return 0;
}
