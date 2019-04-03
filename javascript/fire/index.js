let W, H
let ctx
let pixels
let firePixels
const palette = [
  0x070707,
  0x1f0707,
  0x2f0f07,
  0x470f07,
  0x571707,
  0x671f07,
  0x771f07,
  0x8f2707,
  0x9f2f07,
  0xaf3f07,
  0xbf4707,
  0xc74707,
  0xDF4F07,
  0xDF5707,
  0xDF5707,
  0xD75F07,
  0xD7670F,
  0xcf6f0f,
  0xcf770f,
  0xcf7f0f,
  0xCF8717,
  0xC78717,
  0xC78F17,
  0xC7971F,
  0xBF9F1F,
  0xBF9F1F,
  0xBFA727,
  0xBFA727,
  0xBFAF2F,
  0xB7AF2F,
  0xB7B72F,
  0xB7B737,
  0xCFCF6F,
  0xDFDF9F,
  0xEFEFC7,
  0xFFFFFF
]

function spreadFire(src) {
  const rand = Math.round(Math.random() * 3.0) & 3;
  const dst = src - rand + 1;
  firePixels[dst - W] = firePixels[src] - (rand & 1);
}

function animate() {
  for(let x=0; x < W; ++x) {
    for (y=1; y < H; ++y) {
      spreadFire(y * W + x);
    }
  }
}

function put(pixels, x, y, val) {
  const data = pixels.data

  const xy = y * W * 4 + x * 4
  data[xy + 0] = (val >> 16) & 0xFF
  data[xy + 1] = (val >>  8) & 0xFF
  data[xy + 2] = (val >>  0) & 0xFF
}

function render() {
  for (let x=0; x < W; ++x) {
    for (let y=0; y < H; ++y) {
      const v = firePixels[y * W + x]
      put(pixels, x, y, palette[v] || 0)
    }
  }

  ctx.putImageData(pixels, 0, 0)
}

function loop() {
  animate()
  render()
  requestAnimationFrame(loop)
}

function start() {
  const canvas = document.getElementById('canvas')
  ctx = canvas.getContext('2d', { alpha: false })

  // Init pixels and fire pixels
  W = ctx.canvas.width
  H = ctx.canvas.height

  pixels = ctx.getImageData(0, 0, W, H)
  firePixels = new Array(W * H)

  // Bottom line is white
  for (let x=0; x < W; ++x) {
    firePixels[(H-1) * W + x] = palette.length - 1
  }

  // In 10sec, remove the bottom line
  setTimeout(function() {
    for (let x=0; x < W; ++x) {
      firePixels[(H-1) * W + x] = 0
    }
  }, 10000)

  loop()
}

start()
