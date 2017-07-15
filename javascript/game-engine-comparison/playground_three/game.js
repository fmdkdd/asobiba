var STATES = {}

STATES.Main = {
  enter() {
    this.scene = new THREE.Scene();

	  this.camera = new THREE.PerspectiveCamera(75, this.app.width / this.app.height, 1, 10000);
	  this.camera.position.z = 1000;

	  var geometry = new THREE.BoxGeometry(200, 200, 200);
	  var material = new THREE.MeshBasicMaterial({color: 0xff0000, wireframe: true});

	  this.mesh = new THREE.Mesh(geometry, material);
	  this.scene.add(this.mesh);
  },

  step(dt) {
    this.mesh.rotation.x += 0.01
    this.mesh.rotation.y += 0.02
  },

  render() {
    this.app.renderer.render(this.scene, this.camera)
  },
}

// Skip the loading screen.  It always lasts at least 500ms, even without
// assets.
PLAYGROUND.LoadingScreen = {}

window.addEventListener('DOMContentLoaded', function main() {
  new PLAYGROUND.Application({
    width: 600,
    height: 600,

    ready() {
      this.renderer = new THREE.WebGLRenderer()
      this.renderer.setClearColor(0x272822)
      this.renderer.setSize(this.width, this.height)
      this.container.appendChild(this.renderer.domElement)

      this.setState(STATES.Main)
    },
  })
})

// Local Variables:
// js2-additional-externs: ("THREE" "PLAYGROUND")
// End:
