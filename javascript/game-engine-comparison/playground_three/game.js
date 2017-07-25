var STATES;
(function (STATES) {
    STATES.Main = {
        enter: function () {
            this.scene = new THREE.Scene();
            // this.camera = new THREE.OrthographicCamera(-this.app.width/2, this.app.width/2,
            //                                            this.app.height/2, -this.app.height/2,
            //                                            1, 10000)
            this.camera = new THREE.OrthographicCamera(-1, 1, 1, -1, -1000, 1000);
            this.camera.position.z = 10;
            var geometry = new THREE.BoxGeometry(1, 1, 1);
            var material = new THREE.MeshPhongMaterial({ color: 0x727070 });
            material.shading = THREE.FlatShading;
            material.reflectivity = 0;
            material.shininess = 15;
            this.mesh = new THREE.Mesh(geometry, material);
            this.mesh.scale.multiplyScalar(0.3);
            this.scene.add(this.mesh);
            var text = 'Some other text';
            var canvas = document.createElement('canvas');
            var ctxt = canvas.getContext('2d');
            ctxt.font = '8pt Dina';
            var textWidth = Math.ceil(ctxt.measureText(text).width);
            var textHeight = 9;
            canvas.width = THREE.Math.nextPowerOfTwo(textWidth);
            canvas.height = THREE.Math.nextPowerOfTwo(textHeight);
            ctxt.font = '8pt Dina';
            ctxt.fillStyle = 'rgba(255,255,255,1)';
            ctxt.textAlign = 'center';
            ctxt.textBaseline = 'middle';
            ctxt.fillText(text, canvas.width / 2, canvas.height / 2);
            var texture = new THREE.CanvasTexture(canvas);
            texture.magFilter = THREE.NearestFilter;
            texture.minFilter = THREE.NearestFilter;
            var textMaterial = new THREE.SpriteMaterial({ map: texture,
                color: 0xffff00,
                side: THREE.DoubleSide });
            this.plane = new THREE.Sprite(textMaterial);
            this.plane.scale.set(1, .2, 1);
            this.plane.position.y = 0.3;
            this.scene.add(this.plane);
            this.light = new THREE.DirectionalLight(0xffffff);
            this.light.position.set(1, 1, 1);
            this.scene.add(this.light);
            var light = new THREE.AmbientLight(0x404040);
            this.scene.add(light);
            this.app.gui.add(this.light, 'intensity', 0, 2);
            this.app.gui.add(material, 'shininess', 0, 100);
            var colorData = {
                light: this.light.color.getHex(),
                cube: material.color.getHex(),
                specular: material.specular.getHex()
            };
            this.app.gui.addColor(colorData, 'light').onChange(handleColorChange(this.light.color));
            this.app.gui.addColor(colorData, 'cube').onChange(handleColorChange(material.color));
            this.app.gui.addColor(colorData, 'specular').onChange(handleColorChange(material.specular));
            this.mouse = new THREE.Vector2();
            this.raycaster = new THREE.Raycaster();
            this.point = new THREE.Points(new THREE.Geometry());
            this.point.geometry.vertices.push(new THREE.Vector3(0, 0, 0));
            this.point.material.size = .3;
            this.scene.add(this.point);
        },
        step: function (dt) {
            this.mesh.rotation.x += 0.5 * dt;
            this.mesh.rotation.y += 1 * dt;
            this.camera.rotation.x += 0.05 * dt;
            // this.camera.lookAt(this.scene.position)
            // this.camera.updateMatrixWorld()
            this.point.position.set(this.mouse.x, this.mouse.y, 1);
            console.log(this.mouse);
            this.raycaster.setFromCamera(this.mouse, this.camera);
            // for (var i = 0; i < this.scene.children.length; ++i) {
            //   if (this.scene.children[i].material)
            //     this.scene.children[i].material.color.set(0x707070)
            // }
            var intersects = this.raycaster.intersectObject(this.mesh);
            console.log(intersects.length);
            for (var i = 0; i < intersects.length; i++) {
                intersects[i].object.material.color.set(0xffff00);
            }
        },
        mousemove: function (ev) {
            // Translate mouse coordinates to camera coordinates
            this.mouse.x = (ev.original.clientX / this.app.renderer.domElement.clientWidth) * 2 - 1;
            this.mouse.y = -(ev.original.clientY / this.app.renderer.domElement.clientHeight) * 2 + 1;
        },
        render: function () {
            this.app.renderer.render(this.scene, this.camera);
        }
    };
})(STATES || (STATES = {}));
function handleColorChange(color) {
    return function (value) {
        color.setHex(value);
    };
}
// Skip the loading screen.  It always lasts at least 500ms, even without
// assets.
delete PLAYGROUND.LoadingScreen;
window.addEventListener('DOMContentLoaded', function main() {
    new PLAYGROUND.Application({
        // dimensions of the WebGL buffer
        width: 320,
        height: 180,
        // scaled to screen dimensions
        scale: 3,
        smoothing: false,
        preload: function () {
            // Put FPS counter to bottom right
            this.stats = new Stats();
            this.stats.dom.style.left = '';
            this.stats.dom.style.top = '';
            this.stats.dom.style.right = 0;
            this.stats.dom.style.bottom = 0;
            document.body.appendChild(this.stats.dom);
            // Init dat.gui
            this.gui = new dat.GUI();
        },
        ready: function () {
            if (Detector.webgl) {
                // Init WebGL renderer
                this.renderer = new THREE.WebGLRenderer({
                    antialias: this.smoothing,
                    alpha: true
                });
                this.renderer.setClearColor(0x272822);
                this.renderer.setSize(this.width, this.height, false);
                this.renderer.domElement.style.width = this.width * this.scale + 'px';
                this.renderer.domElement.style.height = this.height * this.scale + 'px';
                this.container.appendChild(this.renderer.domElement);
                document.getElementById('fullscreen')
                    .addEventListener('click', goFullscreen.bind(this));
                // Go to main application
                this.setState(STATES.Main);
            }
            else {
                // WebGL not supported: abort and report error
                this.container.appendChild(Detector.getWebGLErrorMessage());
            }
        },
        // Record FPS through the prerender and postrender events
        postrender: function () { this.stats.update(); }
    });
});
// Go to fullscreen.  User can press Esc to exit
function goFullscreen() {
    var fullscreen = document.fullscreenElement ||
        document.mozFullScreenElement ||
        document.webkitFullscreenElement ||
        document.msFullscreenElement;
    if (!fullscreen) {
        var request = this.container.requestFullscreen ||
            this.container.mozRequestFullScreen ||
            this.container.webkitRequestFullscreen ||
            this.container.msRequestFullscreen;
        request.apply(this.container);
    }
}
