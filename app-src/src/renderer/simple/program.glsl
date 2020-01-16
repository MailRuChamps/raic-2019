varying vec4 v_color;
varying vec2 v_pos;

#ifdef VERTEX_SHADER
attribute vec2 a_pos;

attribute vec2 i_pos;
attribute vec2 i_size;

attribute vec4 i_color;

uniform mat4 u_projection_matrix;
uniform mat4 u_view_matrix;

void main() {
    v_pos = a_pos;
    v_color = i_color;
    gl_Position = u_projection_matrix * u_view_matrix * vec4(i_pos + a_pos * i_size, 0.0, 1.0);
}
#endif

#ifdef FRAGMENT_SHADER
uniform sampler2D u_texture;
void main() {
    gl_FragColor = texture2D(u_texture, vec2(v_pos.x, 1.0 - v_pos.y)) * v_color;
}
#endif