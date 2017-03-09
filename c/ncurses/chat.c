#include <ncurses.h>
#include <string.h>

void recv_msg() {
  printw("blabla\n");
}

int main() {
  WINDOW *msgs, *input, *border;
  int ch;

  initscr();
  cbreak();
  noecho();

  refresh();

  msgs = newwin(LINES - 3, 0, 0, 0);
  scrollok(msgs, TRUE);

  border = newwin(3, 0, LINES - 3, 0);
  box(border, 0, 0);
  wrefresh(border);
  input = newwin(1, COLS - 2, LINES - 2, 1);
  scrollok(input, TRUE);
  keypad(input, TRUE);

  char msg[256];

  while (1) {
    wmove(input, 0, 0);
    wclrtoeol(input);
    msg[0] = '\0';
    wrefresh(input);

    while ((ch = wgetch(input)) != '\n') {
      if (ch == KEY_BACKSPACE || ch == KEY_DC) {
        wprintw(input, "\b \b");
        // TODO: erase char in msg
        wrefresh(input);
      }
      else {
        strncat(msg, (char*)&ch, 1);
        wprintw(input, "%c", ch);
        wrefresh(input);
      }
    }

    wprintw(msgs, "%s\n", msg);
    wrefresh(msgs);
  }

  delwin(input);

  endwin();
}
