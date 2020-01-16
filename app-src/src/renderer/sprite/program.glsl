varying vec2 v_vt;

#ifdef VERTEX_SHADER
attribute vec2 a_pos;

uniform mat4 u_projection_matrix;
uniform mat4 u_view_matrix;

uniform vec2 u_texture_size;
uniform vec2 u_origin;
uniform vec2 u_pos;
uniform float u_rotation;
uniform float u_flip;

vec2 rotate_vec(vec2 v, float angle) {
    float s = sin(angle);
    float c = cos(angle);
    return vec2(v.x * c - v.y * s, v.x * s + v.y * c);
}

void main() {
    v_vt = vec2(a_pos.x * u_flip + (1.0 - a_pos.x) * (1.0 - u_flip), a_pos.y);
    vec2 world_pos = u_pos + rotate_vec((a_pos - u_origin) * u_texture_size, u_rotation);
    gl_Position = u_projection_matrix * u_view_matrix * vec4(world_pos, 0.0, 1.0);
}
#endif

#ifdef FRAGMENT_SHADER
uniform sampler2D u_texture;
uniform vec4 u_color;
void main() {
    gl_FragColor = texture2D(u_texture, vec2(v_vt.x, 1.0 - v_vt.y)) * u_color;
}
#endif