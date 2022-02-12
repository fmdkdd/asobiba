#ifndef GAME_H
#define GAME_H

#include "cargoGenerator.h"
#include "network.h"
#include "station.h"
#include "track.h"
#include "train.h"
#include "utils.h"

struct Game {
  u32 idCounter;

  static const usize MAX_TRAINS = 256;
  Train trains[MAX_TRAINS];
  usize trainCount;

  Network network;

  static const usize MAX_STATIONS = 256;
  Station stations[MAX_STATIONS];
  usize stationCount;

  static const usize MAX_CARGO_GENERATORS = 256;
  CargoGenerator cargoGenerators[MAX_CARGO_GENERATORS];
  usize cargoGeneratorCount;

  void init();
  u32 genId() { return idCounter++; }
  void update();
  void render();

  void addSegment(Vec2i from, Vec2i to);

  //Track &newTrack();
  Train &newTrain();
  Station &newStation();
  CargoGenerator &newCargoGenerator();
};

#endif
