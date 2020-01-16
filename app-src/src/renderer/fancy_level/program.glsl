varying vec4 v_color;
varying vec2 v_uv;

#ifdef VERTEX_SHADER
attribute vec2 a_pos;

attribute vec2 i_pos;
attribute vec4 i_color;
attribute vec2 i_uv_pos;
attribute vec2 i_uv_size;

uniform mat4 u_projection_matrix;
uniform mat4 u_view_matrix;

void main() {
    v_color = i_color;
    v_uv = i_uv_pos + vec2(a_pos.x, 1.0 - a_pos.y) * i_uv_size;
    gl_Position = u_projection_matrix * u_view_matrix * vec4(i_pos + a_pos, 0.0, 1.0);
}
#endif

#ifdef FRAGMENT_SHADER
uniform sampler2D u_texture;
void main() {
    gl_FragColor = texture2D(u_texture, v_uv) * v_color;
}
#endif