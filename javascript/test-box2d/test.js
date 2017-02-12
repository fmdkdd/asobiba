document.addEventListener('DOMContentLoaded', init)

var world

function init() {
  var b2Vec2 = Box2D.Common.Math.b2Vec2,
      b2World = Box2D.Dynamics.b2World,
      b2Body = Box2D.Dynamics.b2Body,
      b2BodyDef = Box2D.Dynamics.b2BodyDef,
      b2FixtureDef = Box2D.Dynamics.b2FixtureDef,
      b2CircleShape = Box2D.Collision.Shapes.b2CircleShape,
      b2DebugDraw = Box2D.Dynamics.b2DebugDraw

  world = new b2World(
    new b2Vec2(0, 10)    //gravity
    ,  true                 //allow sleep
  )

  var fixDef = new b2FixtureDef
  fixDef.density = 1.0
  fixDef.friction = 0.5
  fixDef.restituion = 0.2
  fixDef.shape = new b2CircleShape(Math.random() + 0.1)

  var bodyDef = new b2BodyDef
  bodyDef.type = b2Body.b2_staticBody
  bodyDef.position.x = Math.random() * 10
  bodyDef.position.y = Math.random() * 10
  world.CreateBody(bodyDef).CreateFixture(fixDef)

  var debugDraw = new b2DebugDraw()
  debugDraw.SetSprite(document.getElementById('canvas').getContext('2d'))
	debugDraw.SetDrawScale(30.0)
  debugDraw.SetFillAlpha(0.3)
	debugDraw.SetLineThickness(1.0)
	debugDraw.SetFlags(b2DebugDraw.e_shapeBit | b2DebugDraw.e_jointBit)
  world.SetDebugDraw(debugDraw)

  loop()
}

function loop() {
  world.Step(
    1 / 60   //frame-rate
    ,  10       //velocity iterations
    ,  10       //position iterations
  )
  world.DrawDebugData()
  world.ClearForces()

  requestAnimationFrame(loop)
}
