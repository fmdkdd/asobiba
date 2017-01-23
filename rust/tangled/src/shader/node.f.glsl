#version 100

precision mediump float;
precision mediump int;

uniform vec2 center;
uniform float radius;
uniform vec3 color;

void main() {
  if (distance(gl_FragCoord.xy, center) > radius) discard;
  gl_FragColor = vec4(color, 1.0);
}
