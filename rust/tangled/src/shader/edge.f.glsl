#version 100

precision mediump float;
precision mediump int;

uniform vec3 color;

void main() {
  gl_FragColor = vec4(color, 1.0);
}
