use gl_headless::gl_headless;

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
        let src: Vec<i8> = include_bytes!("main.glsl").iter().map(|x| *x as i8).collect();
        gl::ShaderSource(shader, 1, &src.as_ptr(), std::ptr::null());
        gl::CompileShader(shader);
        check_error("compile");
        let ret = gl::CreateProgram();
        gl::AttachShader(ret, shader);
        gl::LinkProgram(ret);
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

    unsafe {
        let mut tex: gl::types::GLuint = 0;
        gl::GenTextures(1, &mut tex);
        gl::ActiveTexture(gl::TEXTURE0);
        gl::BindTexture(gl::TEXTURE_2D, tex);
        gl::TexParameteri(gl::TEXTURE_2D, gl::TEXTURE_WRAP_S, gl::CLAMP_TO_EDGE as _);
        gl::TexParameteri(gl::TEXTURE_2D, gl::TEXTURE_WRAP_T, gl::CLAMP_TO_EDGE as _);
        gl::TexParameteri(gl::TEXTURE_2D, gl::TEXTURE_MAG_FILTER, gl::LINEAR as _);
        gl::TexParameteri(gl::TEXTURE_2D, gl::TEXTURE_MIN_FILTER, gl::LINEAR as _);
        gl::TexImage2D(gl::TEXTURE_2D, 0, gl::RGBA32F as _, 4, 4, 0, gl::RGBA, gl::FLOAT, std::ptr::null());
        gl::BindImageTexture(0, tex, 0, gl::FALSE, 0, gl::WRITE_ONLY, gl::RGBA32F);
        check_error("texture bind");
    }

    let prog = load_shader();
    let mut buf: Vec<f32> = vec![0.0; 4 * 4 * 4];

    unsafe {
        gl::UseProgram(prog);
        check_error("use program");
        gl::DispatchCompute(1, 1, 1);
        check_error("dispatch compute");
        gl::MemoryBarrier(gl::SHADER_IMAGE_ACCESS_BARRIER_BIT);
        check_error("memory barrier");
        gl::GetTexImage(gl::TEXTURE_2D, 0, gl::RGBA, gl::FLOAT, buf.as_mut_ptr() as _);
        check_error("get texture");
    }

    println!("res: {}", buf[3]);
}
