#include "game.h"

void Game::init() {
  idCounter = 0;
  trainCount = 0;
  cargoGeneratorCount = 0;

  trackNetwork.init();
}

PointId Game::addPoint(Vec2i p) {
  return trackNetwork.addPoint(p);
}

void Game::addSegment(Vec2i from, Vec2i to) {
  u32 idA = trackNetwork.addPoint(from);
  u32 idB = trackNetwork.addPoint(to);
  trackNetwork.addEdge(idA, idB);
}

void Game::addSegment(PointId from, Vec2i to) {
  u32 idB = trackNetwork.addPoint(to);
  trackNetwork.addEdge(from, idB);
}

void Game::addSegment(Vec2i from, PointId to) {
  u32 idA = trackNetwork.addPoint(from);
  trackNetwork.addEdge(idA, to);
}

void Game::addSegment(PointId from, PointId to) {
  trackNetwork.addEdge(from, to);
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
  for (usize i = 0; i < trainCount; ++i)
    trains[i].update();
  for (usize i = 0; i < stationCount; ++i)
    stations[i].update();
}

void Game::render() {
  trackNetwork.render();

  for (usize i = 0; i < cargoGeneratorCount; ++i)
    cargoGenerators[i].render();
  for (usize i = 0; i < trainCount; ++i)
    trains[i].render();
  // for (usize i = 0; i < stationCount; ++i)
  //   stations[i].render();
}
