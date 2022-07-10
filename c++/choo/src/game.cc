#include "game.h"

void Game::init() {
  idCounter = 0;
  trainCount = 0;
  factoryCount = 0;

  trackNetwork.init();

  setupTestLevel();
}

void Game::setupTestLevel() {
  {
    Factory &f = newFactory();
    f.outputCargo[0].type = COAL;
    f.outputCargo[0].count = 0;
    f.outputCargo[0].capacity = 1000;
    f.outputCargo[0].generateCounter = 0;
    f.outputCargo[0].productionRule = &defaultProductionRules[COAL_MINE];
  }

  {
    Factory &f = newFactory();
    f.pos.x = 200.0f;
    f.inputCargo[0].type = COAL;
    f.inputCargo[0].count = 0;
    f.inputCargo[0].capacity = 1000;

    f.outputCargo[0].type = IRON;
    f.outputCargo[0].count = 0;
    f.outputCargo[0].capacity = 1000;
    f.outputCargo[0].generateCounter = 0;
    f.outputCargo[0].productionRule = &defaultProductionRules[IRON_PLANT];
  }
}

PointId Game::addPoint(Vec2i p) { return trackNetwork.addPoint(p); }

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

Factory &Game::newFactory() {
  ASSERT(factoryCount < MAX_FACTORIES);
  Factory &f = factories[factoryCount];
  f.init(genId());
  factoryCount++;
  return f;
}

void Game::update() {
  for (usize i = 0; i < factoryCount; ++i)
    factories[i].update();
  for (usize i = 0; i < trainCount; ++i)
    trains[i].update();
  for (usize i = 0; i < stationCount; ++i)
    stations[i].update();
}

void Game::render() {
  trackNetwork.render();

  for (usize i = 0; i < factoryCount; ++i)
    factories[i].render();
  for (usize i = 0; i < trainCount; ++i)
    trains[i].render();
  // for (usize i = 0; i < stationCount; ++i)
  //   stations[i].render();
}
