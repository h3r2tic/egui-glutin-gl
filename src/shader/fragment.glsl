#version 450

uniform sampler2D u_sampler;

in vec4 v_rgba;
in vec2 v_tc;
out vec4 f_color;
// a dirty hack applied to support webGL2
#define gl_FragColor f_color
#define texture2D texture

void main() {
  // The texture sampler is sRGB aware, and OpenGL already expects linear rgba
  // output so no need for any sRGB conversions here:
  gl_FragColor = v_rgba * texture2D(u_sampler, v_tc);
}
