#version 430
layout(local_size_x = 1, local_size_y=1) in;
layout(r32i, binding = 0) uniform iimage2D img_output;
layout(binding = 1) uniform isampler2D img_input;
layout(location = 4) uniform int width;
layout(location = 5) uniform int height;

bool visited[130][130];

int get_tile(ivec2 loc) {
    return texelFetch(img_input, loc, 0).r;
}

void write2d(ivec2 pos, int v) {
    imageStore(img_output, pos, ivec4(v, 0, 0, 0));
}

void write(int pos, int v) {
    write2d(ivec2(pos, 0), v);
}

ivec2 find_start() {
    for (int x = 0; x < width; ++x) {
        for (int y = 0; y < height; ++y) {
            if (get_tile(ivec2(x, y)) == 3) {
                return ivec2(x, y);
            }
        }
    }
    return ivec2(-1, -1);
}

ivec2 rotate_90_right(ivec2 old) {
    ivec2 ret = ivec2(0, 0);
    ret.x = old.y;
    ret.y = old.x;
    if (ret.x != 0) ret.x = -ret.x;
    return ret;
}

bool oob(ivec2 i) {
    return i.x < 0 || i.x >= width || i.y < 0 || i.y >= height;
}

bool ever_arrives_back(ivec2 obs) {
    int counter = 0;
    ivec2 pos = find_start();
    ivec2 vel = ivec2(0, -1);
    while (!oob(pos)) {
        ivec2 next = pos + vel;
        if (get_tile(next) == 2 || next == obs) {
            vel = rotate_90_right(vel);
        } else {
            counter += 1;
            if (counter >= width * height) return true;
            pos = next;
        }
    }
    return false;
}

void p1() {
    ivec2 pos = find_start();
    ivec2 vel = ivec2(0, -1);
    while (!oob(pos)) {
        ivec2 next = pos + vel;
        if (get_tile(next) == 2) {
            vel = rotate_90_right(vel);
        } else {
            visited[pos.x][pos.y] = true;
            pos = next;
        }
    }
    int count = 0;
    for (int x = 0; x < width; ++x) {
        for (int y = 0; y < height; ++y) {
            if (visited[x][y]) {
                count += 1;
            }
        }
    }
    write2d(ivec2(0, height), count);
}

void p2() {
    int x = int(gl_GlobalInvocationID.x);
    int y = int(gl_GlobalInvocationID.y);
    if (ever_arrives_back(ivec2(x, y))) {
        write2d(ivec2(x, y), 1);
    } else {
        write2d(ivec2(x, y), 0);
    }
}

void main() {
    p1();
    p2();
}
