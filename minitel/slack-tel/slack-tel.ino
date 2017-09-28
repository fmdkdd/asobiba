#include <SoftwareSerial.h>
#include <Minitel.h>

Minitel m;

void setup() {
  m.clearScreen();
  m.moveCursorTo(1, 0);
  m.graphicMode();
  m.textColor(BLACK);
  m.bgColor(WHITE);
  m.graphic("110011");
  m.repeat(8);

  m.textMode();
  m.text(" MINISLACK 1.0 ");

  m.graphicMode();
  m.graphic("110011");
  m.repeat(8);

  m.textMode();
  m.textColor(WHITE);
  m.bgColor(BLACK);
  m.cursor();
  m.moveCursorTo(1, 24);
}

void loop() {
  m.text("bar> foo!");
  m.moveCursor(DOWN);
  m.moveCursorTo(HOME);
  delay(1000);
}
