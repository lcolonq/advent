use std::ffi::CStr;

use gl_headless::gl_headless;

fn check_compile_error(shader: gl::types::GLuint) {
    unsafe {
        let mut success: gl::types::GLint = 0;
        gl::GetShaderiv(shader, gl::COMPILE_STATUS, &mut success as *mut _);
        if success == 0 {
            let mut buf: [i8; 2048] = [0; 2048];
            gl::GetShaderInfoLog(shader, 2048, 0 as _, &mut buf as *mut i8);
            let msg: &CStr = CStr::from_ptr(&buf as *const i8);
            let strmsg = String::from_utf8_lossy(msg.to_bytes()).to_string();
            println!("failed to compile shader: {}", strmsg);
        }
    }
}

fn check_link_error(prog: gl::types::GLuint) {
    unsafe {
        let mut success: gl::types::GLint = 0;
        gl::GetProgramiv(prog, gl::LINK_STATUS, &mut success as *mut _);
        if success == 0 {
            let mut buf: [i8; 2048] = [0; 2048];
            gl::GetProgramInfoLog(prog, 2048, 0 as _, &mut buf as *mut i8);
            let msg: &CStr = CStr::from_ptr(&buf as *const i8);
            let strmsg = String::from_utf8_lossy(msg.to_bytes()).to_string();
            println!("failed to link shader: {}", strmsg);
        }
    }
}

fn check_error(loc: &str) {
    unsafe {
        let err = gl::GetError();
        if err != gl::NO_ERROR {
            println!("OpenGL error at {}: {}", loc, err);
        }
    }
}

fn load_shader() -> gl::types::GLuint {
    unsafe {
        let shader = gl::CreateShader(gl::COMPUTE_SHADER);
        let mut src: Vec<i8> = include_bytes!("main.glsl").iter().map(|x| *x as i8).collect();
        src.push(0);
        gl::ShaderSource(shader, 1, &src.as_ptr(), std::ptr::null());
        gl::CompileShader(shader);
        check_compile_error(shader);
        check_error("compile");
        let ret = gl::CreateProgram();
        gl::AttachShader(ret, shader);
        gl::LinkProgram(ret);
        check_link_error(ret);
        check_error("link");
        ret
    }
}

#[gl_headless]
fn main() {
    let (mut major, mut minor) = (0, 0);
    unsafe {
        gl::GetIntegerv(gl::MAJOR_VERSION, &mut major);
        gl::GetIntegerv(gl::MINOR_VERSION, &mut minor);
    }
    println!("OpenGL {major}.{minor}");

    let inp = include_str!("../input.txt");
    let mut inpbuf: Vec<i32> = Vec::new();
    let mut height = 0;
    for c in inp.chars() {
        if c != '\n' {
            inpbuf.push(match c {
                '.' => 1,
                '#' => 2,
                '^' => 3,
                _ => 31337,
            });
        } else {
            height += 1;
        }
    }
    let width = inpbuf.len() / height;
    println!("{:?}", (width, height));

    unsafe {
        let mut tex: gl::types::GLuint = 0;
        gl::GenTextures(1, &mut tex);
        gl::ActiveTexture(gl::TEXTURE1);
        gl::BindTexture(gl::TEXTURE_2D, tex);
        gl::TexParameteri(gl::TEXTURE_2D, gl::TEXTURE_WRAP_S, gl::CLAMP_TO_EDGE as _);
        gl::TexParameteri(gl::TEXTURE_2D, gl::TEXTURE_WRAP_T, gl::CLAMP_TO_EDGE as _);
        gl::TexParameteri(gl::TEXTURE_2D, gl::TEXTURE_MAG_FILTER, gl::NEAREST as _);
        gl::TexParameteri(gl::TEXTURE_2D, gl::TEXTURE_MIN_FILTER, gl::NEAREST as _);
        gl::TexImage2D(gl::TEXTURE_2D, 0, gl::R32I as _, width as _, height as _, 0, gl::RED_INTEGER, gl::INT, inpbuf.as_ptr() as _);
        check_error("input texture creation");
    }

    unsafe {
        let mut tex: gl::types::GLuint = 0;
        gl::GenTextures(1, &mut tex);
        gl::ActiveTexture(gl::TEXTURE0);
        gl::BindTexture(gl::TEXTURE_2D, tex);
        gl::TexParameteri(gl::TEXTURE_2D, gl::TEXTURE_WRAP_S, gl::CLAMP_TO_EDGE as _);
        gl::TexParameteri(gl::TEXTURE_2D, gl::TEXTURE_WRAP_T, gl::CLAMP_TO_EDGE as _);
        gl::TexParameteri(gl::TEXTURE_2D, gl::TEXTURE_MAG_FILTER, gl::NEAREST as _);
        gl::TexParameteri(gl::TEXTURE_2D, gl::TEXTURE_MIN_FILTER, gl::NEAREST as _);
        gl::TexImage2D(gl::TEXTURE_2D, 0, gl::R32I as _, width as _, (height + 1) as _, 0, gl::RED_INTEGER, gl::INT, std::ptr::null());
        gl::BindImageTexture(0, tex, 0, gl::FALSE, 0, gl::WRITE_ONLY, gl::R32I);
        check_error("texture bind");
    }

    let prog = load_shader();
    let mut buf: Vec<i32> = vec![0xbeef; width * (height + 1)];

    unsafe {
        gl::UseProgram(prog);
        check_error("use program");

        gl::Uniform1i(4, width as _);
        gl::Uniform1i(5, height as _);

        gl::DispatchCompute(width as _, height as _, 1);
        check_error("dispatch compute");
        gl::MemoryBarrier(gl::SHADER_IMAGE_ACCESS_BARRIER_BIT);
        check_error("memory barrier");
        gl::GetTexImage(gl::TEXTURE_2D, 0, gl::RED_INTEGER, gl::INT, buf.as_mut_ptr() as _);
        check_error("get texture");
    }

    let mut sum: i32 = 0;
    for y in 0..height {
        for x in 0..width {
            sum += buf[y * width + x];
        }
    }
    println!("p1: {:?}", buf[width * height]);
    println!("p2: {:?}", sum);
}
