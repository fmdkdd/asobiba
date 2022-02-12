#include "game.h"

void Game::init() {
  idCounter = 0;
  trainCount = 0;
  //trackCount = 0;
  cargoGeneratorCount = 0;

  trackNetwork.init();
}

// Track &Game::newTrack() {
//   ASSERT(trackCount < MAX_TRACKS);
//   Track &t = tracks[trackCount];
//   t.init(genId());
//   trackCount++;
//   return t;
// }

PointId Game::addPoint(Vec2i p) {
  Network::Point a;
  a.x = p.x;
  a.y = p.y;

  return trackNetwork.addPoint(a);
}

void Game::addSegment(Vec2i from, Vec2i to) {
  Network::Point a;
  a.x = from.x;
  a.y = from.y;

  Network::Point b;
  b.x = to.x;
  b.y = to.y;

  u32 idA = trackNetwork.addPoint(a);
  u32 idB = trackNetwork.addPoint(b);
  trackNetwork.addEdge(idA, idB);
}

void Game::addSegment(PointId from, Vec2i to) {
  Network::Point b;
  b.x = to.x;
  b.y = to.y;

  u32 idB = trackNetwork.addPoint(b);
  trackNetwork.addEdge(from, idB);
}

void Game::addSegment(Vec2i from, PointId to) {
  Network::Point a;
  a.x = from.x;
  a.y = from.y;

  u32 idA = trackNetwork.addPoint(a);
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
  // for (usize i = 0; i < trackCount; ++i)
  //   tracks[i].update();
  for (usize i = 0; i < trainCount; ++i)
    trains[i].update();
  for (usize i = 0; i < stationCount; ++i)
    stations[i].update();
}

void Game::render() {
  trackNetwork.render();

  for (usize i = 0; i < cargoGeneratorCount; ++i)
    cargoGenerators[i].render();
  // for (usize i = 0; i < trackCount; ++i)
  //   tracks[i].render();
  // for (usize i = 0; i < trainCount; ++i)
  //   trains[i].render();
  // for (usize i = 0; i < stationCount; ++i)
  //   stations[i].render();
}
