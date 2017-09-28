#include <SoftwareSerial.h>
#include <Minitel.h>
#include "image.h"

Minitel m;

// From darkest to lightest
byte colors[] = {0, 4, 1, 5, 2, 6, 3, 7};

void setup() {
  m.graphicMode();
  m.clearScreen();
  m.moveCursorTo(0,0);

  for (int i=0; i < 40*24; ++i) {
    m.bgColor(colors[c[i][0]]);
    m.textColor(colors[c[i][1]]);
    m.serialprint7(c[i][2]); 
  }
}

void loop() {
}
