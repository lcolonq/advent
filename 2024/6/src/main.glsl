#version 430
layout(local_size_x = 1, local_size_y=1) in;
layout(rgba32f, binding = 0) uniform image2D img_output;

void main() {
    imageStore(img_output, ivec2(0, 0), vec4(17.0, 18.0, 42.0, 1337.0));
}
