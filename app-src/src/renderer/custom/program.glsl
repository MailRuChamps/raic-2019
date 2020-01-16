varying vec4 v_color;

#ifdef VERTEX_SHADER
attribute vec2 position;
attribute vec4 color;

uniform mat4 u_projection_matrix;
uniform mat4 u_view_matrix;

void main() {
    v_color = color;
    gl_Position = u_projection_matrix * u_view_matrix * vec4(position, 0.0, 1.0);
}
#endif

#ifdef FRAGMENT_SHADER
void main() {
    gl_FragColor = v_color;
}
#endif