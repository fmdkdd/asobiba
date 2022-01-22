#ifndef GAME_H
#define GAME_H

#include "cargoGenerator.h"
#include "station.h"
#include "track.h"
#include "train.h"
#include "utils.h"

struct Game {
  u32 idCounter;

  static const usize MAX_TRAINS = 256;
  Train trains[MAX_TRAINS];
  usize trainCount;

  static const usize MAX_TRACKS = 256;
  Track tracks[MAX_TRACKS];
  usize trackCount;

  static const usize MAX_STATIONS = 256;
  Station stations[MAX_STATIONS];
  usize stationCount;

  static const usize MAX_CARGO_GENERATORS = 256;
  CargoGenerator cargoGenerators[MAX_CARGO_GENERATORS];
  usize cargoGeneratorCount;

  void init();
  u32 genId() { return idCounter++; }
  Track &newTrack();
  Train &newTrain();
  Station &newStation();
  CargoGenerator &newCargoGenerator();
  void update();
  void render();
};

#endif
