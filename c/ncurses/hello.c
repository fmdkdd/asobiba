#include <ncurses.h>

WINDOW *stats;
short cur_stat;

void refresh_stats_win() {
  if (cur_stat == 0)
    wattron(stats, COLOR_PAIR(1) | A_STANDOUT);
  wmove(stats, 1,1);
  wprintw(stats, "Strength\t%2d", 12);
  wattroff(stats, COLOR_PAIR(1) | A_STANDOUT);

  if (cur_stat == 1)
    wattron(stats, COLOR_PAIR(1) | A_STANDOUT);
  wmove(stats, 2,1);
  wprintw(stats, "Magic\t\t%2d", 6);
  wattroff(stats, COLOR_PAIR(1) | A_STANDOUT);

  if (cur_stat == 2)
    wattron(stats, COLOR_PAIR(1) | A_STANDOUT);
  wmove(stats, 3,1);
  wprintw(stats, "Speed\t\t%2d", 5);
  wattroff(stats, COLOR_PAIR(1) | A_STANDOUT);

  wrefresh(stats);
}

void open_stats_win() {
  cur_stat = 0;

  if (!stats)
    stats = newwin(5, 20, 10, 10);
  box(stats, 0, 0);

  refresh_stats_win();
}

void close_stats_win() {
  werase(stats);
  wrefresh(stats);
}

enum {
  MAIN, STATS_WIN
};

int main() {
  int ch;
  int state = MAIN;

  initscr();
  noecho();
  keypad(stdscr, TRUE);
  curs_set(0);                  /* invisible cursor */

  start_color();
  init_pair(1, COLOR_YELLOW, COLOR_BLACK);

  stats = NULL;

  refresh();

  while ((ch = getch()) != 'q') {
    if (state == MAIN) {
      if (ch == 'i') {
        state = STATS_WIN;
        open_stats_win();
      }
    }
    else if (state == STATS_WIN) {
      if (ch == 'i') {
        close_stats_win();
        state = MAIN;
      } else if (ch == KEY_DOWN) {
        ++cur_stat;
        refresh_stats_win();
      } else if (ch == KEY_UP) {
        --cur_stat;
        refresh_stats_win();
      }
    }
  }

  delwin(stats);

  endwin();
}
