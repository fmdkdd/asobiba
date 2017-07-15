window.addEventListener('DOMContentLoaded', init);

function init() {
  var B = BABYLON;

  var canvas = document.getElementById('renderCanvas');
  var engine = new B.Engine(canvas, true);

  var player;

  function createScene() {
    var scene = new B.Scene(engine);

    // var light = new BABYLON.PointLight("Omni", new BABYLON.Vector3(40, 0, -100), scene);
    var camera = new BABYLON.TargetCamera("Camera", new B.Vector3(0, 0, -1), scene);
    camera.mode = 1;
    camera.orthoTop = 450;
    camera.orthoBottom = -450;
    camera.orthoLeft = -300;
    camera.orthoRight = 300;
    camera.setTarget(B.Vector3.Zero());

    var plane = B.Mesh.CreatePlane("bg", 1, scene);
    plane.scaling = new B.Vector3(600, 900, 1);
    plane.material = new B.StandardMaterial("texturePlane", scene);
    plane.material.emissiveColor = new B.Color3(0, .3, .3);
    plane.material.disableLighting = true;

    player = B.Mesh.CreatePlane("player", 1, scene);
    player.scaling = new B.Vector3(30, 40, 1);
    player.material = new B.StandardMaterial("texturePlane", scene);
    player.material.emissiveColor = new B.Color3(.9, .8, .3);
    player.material.disableLighting = true;

    scene.enablePhysics(new B.Vector3(0,0,0));

    player.physicsImpostor = new B.PhysicsImpostor(player,
                                                   B.PhysicsImpostor.BoxImpostor,
                                                   {mass: 1,
                                                    friction: 0,
                                                    restitution: 0});

    scene.actionManager = new B.ActionManager(scene);
    scene.actionManager.registerAction(new B.ExecuteCodeAction(B.ActionManager.OnKeyDownTrigger, function(e){
      if (e.sourceEvent.key == 'w' ||
          e.sourceEvent.key == 'a' ||
          e.sourceEvent.key == 'r' ||
          e.sourceEvent.key == 's') {
        keys[e.sourceEvent.key] = true;
      }
    }));
    scene.actionManager.registerAction(new B.ExecuteCodeAction(B.ActionManager.OnKeyUpTrigger, function(e){
      if (e.sourceEvent.key == 'w' ||
          e.sourceEvent.key == 'a' ||
          e.sourceEvent.key == 'r' ||
          e.sourceEvent.key == 's') {
        keys[e.sourceEvent.key] = false;
      }
    }));

    return scene;
  }

  var keys = {};
  var scene = createScene();

  var drag = 0.55;
  var speed = 100;

  engine.runRenderLoop(function() {
    var impulse = new B.Vector3(0,0,0);
    if (keys['w']) {
      impulse.y = 1;
    }
    if (keys['a']) {
      impulse.x = -1;
    }
    if (keys['r']) {
      impulse.y = -1;
    }
    if (keys['s']) {
      impulse.x = 1;
    }
    impulse.scaleInPlace(speed);
    // player.physicsImpostor.setLinearVelocity(impulse);
    player.physicsImpostor.applyImpulse(impulse, player.getAbsolutePosition());
    player.physicsImpostor.setLinearVelocity(player.physicsImpostor.getLinearVelocity().scale(drag));

    scene.render();
  });

  window.addEventListener('resize', function() {
    engine.resize();
  });

  //scene.debugLayer.show();
}
