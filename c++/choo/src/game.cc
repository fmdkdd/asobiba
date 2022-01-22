#include "game.h"

void Game::init() {
  idCounter = 0;
  trainCount = 0;
  trackCount = 0;
  cargoGeneratorCount = 0;
}

Track &Game::newTrack() {
  ASSERT(trackCount < MAX_TRACKS);
  Track &t = tracks[trackCount];
  t.init(genId());
  trackCount++;
  return t;
}

Train &Game::newTrain() {
  ASSERT(trainCount < MAX_TRAINS);
  Train &t = trains[trainCount];
  t.init(genId());
  trainCount++;
  return t;
}

Station &Game::newStation() {
  ASSERT(stationCount < MAX_STATIONS);
  Station &s = stations[stationCount];
  s.init(genId());
  stationCount++;
  return s;
}

CargoGenerator &Game::newCargoGenerator() {
  ASSERT(cargoGeneratorCount < MAX_CARGO_GENERATORS);
  CargoGenerator &s = cargoGenerators[cargoGeneratorCount];
  s.init(genId());
  cargoGeneratorCount++;
  return s;
}

void Game::update() {
  for (usize i = 0; i < cargoGeneratorCount; ++i)
    cargoGenerators[i].update();
  for (usize i = 0; i < trackCount; ++i)
    tracks[i].update();
  for (usize i = 0; i < trainCount; ++i)
    trains[i].update();
  for (usize i = 0; i < stationCount; ++i)
    stations[i].update();
}

void Game::render() {
  for (usize i = 0; i < cargoGeneratorCount; ++i)
    cargoGenerators[i].render();
  for (usize i = 0; i < trackCount; ++i)
    tracks[i].render();
  // for (usize i = 0; i < trainCount; ++i)
  //   trains[i].render();
  // for (usize i = 0; i < stationCount; ++i)
  //   stations[i].render();
}
