var STATES = {}

STATES.Main = {
  enter() {
    this.scene = new THREE.Scene()

	  this.camera = new THREE.OrthographicCamera(-this.app.width/2, this.app.width/2,
                                               this.app.height/2, -this.app.height/2,
                                               1, 10000)
    this.camera.position.z = 1000

	  var geometry = new THREE.BoxGeometry(1, 1, 1)
	  var material = new THREE.MeshBasicMaterial({color: 0xff0000, wireframe: true})
	  this.mesh = new THREE.Mesh(geometry, material)
    this.mesh.scale.multiplyScalar(50)
	  this.scene.add(this.mesh)

    var text = 'Some other text'

    var canvas = document.createElement('canvas')
    var ctxt = canvas.getContext('2d')

    ctxt.font = '8pt Dina'
    var textWidth = Math.ceil(ctxt.measureText(text).width)
    var textHeight = 9
    canvas.width = THREE.Math.nextPowerOfTwo(textWidth)
    canvas.height = THREE.Math.nextPowerOfTwo(textHeight)
    ctxt.font = '8pt Dina'
    ctxt.fillStyle = 'rgba(255,255,255,1)'
    ctxt.textAlign = 'center'
    ctxt.textBaseline = 'middle'
    ctxt.fillText(text, canvas.width/2, canvas.height/2)

    var texture = new THREE.CanvasTexture(canvas)
    texture.magFilter = THREE.NearestFilter
    texture.minFilter = THREE.NearestFilter
    var textMaterial = new THREE.SpriteMaterial({ map: texture,
                                                  side: THREE.DoubleSide })
    this.plane = new THREE.Sprite(textMaterial)
    this.plane.scale.set(canvas.width, canvas.height, 1)
    this.plane.position.y = 50
    this.scene.add(this.plane)
  },

  step(dt) {
    this.mesh.rotation.x += 1 * dt
    this.mesh.rotation.y += 2 * dt
  },

  render() {
    this.app.renderer.render(this.scene, this.camera, undefined, true)
  },
}

// Skip the loading screen.  It always lasts at least 500ms, even without
// assets.
delete PLAYGROUND.LoadingScreen

window.addEventListener('DOMContentLoaded', function main() {
  new PLAYGROUND.Application({
    // dimensions of the WebGL buffer
    width: 320,
    height: 180,
    // scaled to screen dimensions
    scale: 3,

    smoothing: false,

    ready() {
      if (Detector.webgl) {
        this.renderer = new THREE.WebGLRenderer({
          antialias: this.smoothing,
          alpha: true,
        })
        this.renderer.setClearColor(0x272822)
        this.renderer.setSize(this.width, this.height, false)
        this.renderer.domElement.style.width = this.width * this.scale + 'px'
        this.renderer.domElement.style.height = this.height * this.scale + 'px'
        this.container.appendChild(this.renderer.domElement)

        this.setState(STATES.Main)
      } else {
        this.container.appendChild(Detector.getWebGLErrorMessage())
      }
    },

    pointerup(ev) {
      var fullscreen = document.fullscreenElement ||
          document.mozFullScreenElement ||
          document.webkitFullscreenElement ||
          document.msFullscreenElement
      if (!fullscreen) {
        var request = this.container.requestFullscreen ||
            this.container.mozRequestFullScreen ||
            this.container.webkitRequestFullscreen ||
            this.container.msRequestFullscreen
        request.apply(this.container)
      }
    },
  })
})

// Local Variables:
// js2-additional-externs: ("THREE" "PLAYGROUND")
// End:
