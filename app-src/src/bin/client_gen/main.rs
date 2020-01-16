use aicup2019::*;
use std::fs::File;
use std::path::{Path, PathBuf};

const VERSION: &str = "0.1.0";

#[derive(StructOpt)]
struct Opt {
    path: PathBuf,
}

fn write_file<P: AsRef<Path>>(path: P, content: &str) -> std::io::Result<()> {
    if let Some(dir) = path.as_ref().parent() {
        std::fs::create_dir_all(dir)?;
    }
    File::create(path)?.write_all(content.as_bytes())?;
    Ok(())
}

fn gen_python(opt: &Opt) -> std::io::Result<()> {
    let path = opt.path.join("python");
    let mut gen = trans_gen::Python::new("aicup2019", VERSION);
    gen.add(&trans_schema::schema::<codegame::PlayerMessage<model::Game>>());
    gen.add(&trans_schema::schema::<codegame::ServerMessage<model::Game>>());
    gen.write_to(&path)?;
    write_file(path.join("Dockerfile"), include_str!("python/Dockerfile"))?;
    write_file(path.join("compile.sh"), include_str!("python/compile.sh"))?;
    write_file(path.join("run.sh"), include_str!("python/run.sh"))?;
    write_file(path.join("main.py"), include_str!("python/main.py"))?;
    write_file(path.join("debug.py"), include_str!("python/debug.py"))?;
    write_file(
        path.join("my_strategy.py"),
        include_str!("python/my_strategy.py"),
    )?;
    Ok(())
}

fn gen_javascript(opt: &Opt) -> std::io::Result<()> {
    let path = opt.path.join("javascript");
    let mut gen = trans_gen::JavaScript::new("aicup2019", VERSION);
    gen.add(&trans_schema::schema::<codegame::PlayerMessage<model::Game>>());
    gen.add(&trans_schema::schema::<codegame::ServerMessage<model::Game>>());
    gen.write_to(&path)?;
    write_file(
        path.join("Dockerfile"),
        include_str!("javascript/Dockerfile"),
    )?;
    write_file(
        path.join("compile.sh"),
        include_str!("javascript/compile.sh"),
    )?;
    write_file(path.join("run.sh"), include_str!("javascript/run.sh"))?;
    write_file(path.join("index.js"), include_str!("javascript/index.js"))?;
    write_file(path.join("debug.js"), include_str!("javascript/debug.js"))?;
    write_file(
        path.join("my-strategy.js"),
        include_str!("javascript/my-strategy.js"),
    )?;
    Ok(())
}

fn gen_ruby(opt: &Opt) -> std::io::Result<()> {
    let path = opt.path.join("ruby");
    let mut gen = trans_gen::Ruby::new("aicup2019", VERSION);
    gen.add(&trans_schema::schema::<codegame::PlayerMessage<model::Game>>());
    gen.add(&trans_schema::schema::<codegame::ServerMessage<model::Game>>());
    gen.write_to(&path)?;
    write_file(path.join("Dockerfile"), include_str!("ruby/Dockerfile"))?;
    write_file(path.join("compile.sh"), include_str!("ruby/compile.sh"))?;
    write_file(path.join("run.sh"), include_str!("ruby/run.sh"))?;
    write_file(path.join("main.rb"), include_str!("ruby/main.rb"))?;
    write_file(path.join("debug.rb"), include_str!("ruby/debug.rb"))?;
    write_file(
        path.join("my_strategy.rb"),
        include_str!("ruby/my_strategy.rb"),
    )?;
    Ok(())
}

fn gen_rust(opt: &Opt) -> std::io::Result<()> {
    let path = opt.path.join("rust");
    let mut gen = trans_gen::Rust::new("aicup2019-model", VERSION);
    gen.add(&trans_schema::schema::<codegame::PlayerMessage<model::Game>>());
    gen.add(&trans_schema::schema::<codegame::ServerMessage<model::Game>>());
    gen.write_to(path.join("model"))?;
    write_file(path.join("Dockerfile"), include_str!("rust/Dockerfile"))?;
    write_file(path.join("compile.sh"), include_str!("rust/compile.sh"))?;
    write_file(path.join("run.sh"), include_str!("rust/run.sh"))?;
    write_file(
        path.join("Cargo.toml"),
        &include_str!("rust/Cargo.toml.template").replace("$version", VERSION),
    )?;
    write_file(path.join("src/main.rs"), include_str!("rust/src/main.rs"))?;
    write_file(
        path.join("src/my_strategy.rs"),
        include_str!("rust/src/my_strategy.rs"),
    )?;
    Ok(())
}

fn gen_java(opt: &Opt) -> std::io::Result<()> {
    let path = opt.path.join("java");
    let src_path = path.join("src").join("main").join("java");
    let mut gen = trans_gen::Java::new("aicup2019", VERSION);
    gen.add(&trans_schema::schema::<codegame::PlayerMessage<model::Game>>());
    gen.add(&trans_schema::schema::<codegame::ServerMessage<model::Game>>());
    gen.write_to(&src_path)?;
    write_file(path.join("Dockerfile"), include_str!("java/Dockerfile"))?;
    write_file(path.join("compile.sh"), include_str!("java/compile.sh"))?;
    write_file(path.join("run.sh"), include_str!("java/run.sh"))?;
    write_file(
        src_path.join("MyStrategy.java"),
        &include_str!("java/MyStrategy.java"),
    )?;
    write_file(
        src_path.join("Debug.java"),
        &include_str!("java/Debug.java"),
    )?;
    write_file(
        src_path.join("Runner.java"),
        &include_str!("java/Runner.java"),
    )?;
    write_file(path.join("pom.xml"), include_str!("java/pom.xml"))?;
    Ok(())
}

fn gen_scala(opt: &Opt) -> std::io::Result<()> {
    let path = opt.path.join("scala");
    let src_path = path.join("src").join("main").join("scala");
    let mut gen = trans_gen::Scala::new("aicup2019", VERSION);
    gen.add(&trans_schema::schema::<codegame::PlayerMessage<model::Game>>());
    gen.add(&trans_schema::schema::<codegame::ServerMessage<model::Game>>());
    gen.write_to(&src_path)?;
    write_file(path.join("Dockerfile"), include_str!("scala/Dockerfile"))?;
    write_file(path.join("compile.sh"), include_str!("scala/compile.sh"))?;
    write_file(path.join("run.sh"), include_str!("scala/run.sh"))?;
    write_file(
        src_path.join("MyStrategy.scala"),
        &include_str!("scala/MyStrategy.scala"),
    )?;
    write_file(
        src_path.join("Debug.scala"),
        &include_str!("scala/Debug.scala"),
    )?;
    write_file(
        src_path.join("Runner.scala"),
        &include_str!("scala/Runner.scala"),
    )?;
    write_file(path.join("pom.xml"), include_str!("scala/pom.xml"))?;
    Ok(())
}

fn gen_kotlin(opt: &Opt) -> std::io::Result<()> {
    let path = opt.path.join("kotlin");
    let src_path = path.join("src").join("main").join("kotlin");
    let mut gen = trans_gen::Kotlin::new("aicup2019", VERSION);
    gen.add(&trans_schema::schema::<codegame::PlayerMessage<model::Game>>());
    gen.add(&trans_schema::schema::<codegame::ServerMessage<model::Game>>());
    gen.write_to(&src_path)?;
    write_file(path.join("Dockerfile"), include_str!("kotlin/Dockerfile"))?;
    write_file(path.join("compile.sh"), include_str!("kotlin/compile.sh"))?;
    write_file(path.join("run.sh"), include_str!("kotlin/run.sh"))?;
    write_file(
        src_path.join("MyStrategy.kt"),
        &include_str!("kotlin/MyStrategy.kt"),
    )?;
    write_file(src_path.join("Debug.kt"), &include_str!("kotlin/Debug.kt"))?;
    write_file(
        src_path.join("Runner.kt"),
        &include_str!("kotlin/Runner.kt"),
    )?;
    write_file(path.join("pom.xml"), include_str!("kotlin/pom.xml"))?;
    Ok(())
}

fn gen_csharp(opt: &Opt) -> std::io::Result<()> {
    let path = opt.path.join("csharp");
    let mut gen = trans_gen::CSharp::new("AiCup2019", VERSION);
    gen.add(&trans_schema::schema::<codegame::PlayerMessage<model::Game>>());
    gen.add(&trans_schema::schema::<codegame::ServerMessage<model::Game>>());
    gen.write_to(&path)?;
    write_file(path.join("Dockerfile"), include_str!("csharp/Dockerfile"))?;
    write_file(path.join("compile.sh"), include_str!("csharp/compile.sh"))?;
    write_file(path.join("run.sh"), include_str!("csharp/run.sh"))?;
    write_file(path.join("Runner.cs"), &include_str!("csharp/Runner.cs"))?;
    write_file(
        path.join("MyStrategy.cs"),
        &include_str!("csharp/MyStrategy.cs"),
    )?;
    write_file(path.join("Debug.cs"), &include_str!("csharp/Debug.cs"))?;
    write_file(
        path.join("aicup2019.csproj"),
        &include_str!("csharp/aicup2019.csproj"),
    )?;
    Ok(())
}

fn gen_fsharp(opt: &Opt) -> std::io::Result<()> {
    let path = opt.path.join("fsharp");
    let mut gen = trans_gen::FSharp::new("AiCup2019", VERSION);
    gen.add(&trans_schema::schema::<codegame::PlayerMessage<model::Game>>());
    gen.add(&trans_schema::schema::<codegame::ServerMessage<model::Game>>());
    gen.write_to(&path)?;
    write_file(path.join("Dockerfile"), include_str!("fsharp/Dockerfile"))?;
    write_file(path.join("compile.sh"), include_str!("fsharp/compile.sh"))?;
    write_file(path.join("run.sh"), include_str!("fsharp/run.sh"))?;
    write_file(path.join("Runner.fs"), &include_str!("fsharp/Runner.fs"))?;
    write_file(
        path.join("MyStrategy.fs"),
        &include_str!("fsharp/MyStrategy.fs"),
    )?;
    write_file(path.join("Debug.fs"), &include_str!("fsharp/Debug.fs"))?;
    write_file(
        path.join("aicup2019.fsproj"),
        &include_str!("fsharp/aicup2019.fsproj"),
    )?;
    Ok(())
}

fn gen_dlang(opt: &Opt) -> std::io::Result<()> {
    let path = opt.path.join("dlang");
    let mut gen = trans_gen::Dlang::new("aicup2019", VERSION);
    gen.add(&trans_schema::schema::<codegame::PlayerMessage<model::Game>>());
    gen.add(&trans_schema::schema::<codegame::ServerMessage<model::Game>>());
    gen.write_to(path.join("source"))?;
    write_file(path.join("Dockerfile"), include_str!("dlang/Dockerfile"))?;
    write_file(path.join("compile.sh"), include_str!("dlang/compile.sh"))?;
    write_file(path.join("run.sh"), include_str!("dlang/run.sh"))?;
    write_file(path.join("dub.json"), &include_str!("dlang/dub.json"))?;
    write_file(
        path.join("source").join("app.d"),
        &include_str!("dlang/app.d"),
    )?;
    write_file(
        path.join("source").join("debugger.d"),
        &include_str!("dlang/debugger.d"),
    )?;
    write_file(
        path.join("source").join("my_strategy.d"),
        &include_str!("dlang/my_strategy.d"),
    )?;
    Ok(())
}

fn gen_go(opt: &Opt) -> std::io::Result<()> {
    let path = opt.path.join("go");
    let mut gen = trans_gen::Go::new("aicup2019", VERSION);
    gen.add(&trans_schema::schema::<codegame::PlayerMessage<model::Game>>());
    gen.add(&trans_schema::schema::<codegame::ServerMessage<model::Game>>());
    gen.write_to(&path)?;
    write_file(path.join("Dockerfile"), include_str!("go/Dockerfile"))?;
    write_file(path.join("compile.sh"), include_str!("go/compile.sh"))?;
    write_file(path.join("run.sh"), include_str!("go/run.sh"))?;
    write_file(path.join("go.mod"), &include_str!("go/go.mod"))?;
    write_file(path.join("main.go"), &include_str!("go/main.go"))?;
    write_file(path.join("debug.go"), &include_str!("go/debug.go"))?;
    write_file(
        path.join("my_strategy.go"),
        &include_str!("go/my_strategy.go"),
    )?;
    Ok(())
}

fn gen_cpp(opt: &Opt) -> std::io::Result<()> {
    let path = opt.path.join("cpp");
    let mut gen = trans_gen::Cpp::new("aicup2019", VERSION);
    gen.add(&trans_schema::schema::<codegame::PlayerMessage<model::Game>>());
    gen.add(&trans_schema::schema::<codegame::ServerMessage<model::Game>>());
    gen.write_to(&path)?;
    write_file(path.join("Dockerfile"), include_str!("cpp/Dockerfile"))?;
    write_file(path.join("compile.sh"), include_str!("cpp/compile.sh"))?;
    write_file(path.join("run.sh"), include_str!("cpp/run.sh"))?;
    write_file(
        path.join("CMakeLists.txt"),
        &include_str!("cpp/CMakeLists.txt"),
    )?;
    write_file(
        path.join("TcpStream.hpp"),
        &include_str!("cpp/TcpStream.hpp"),
    )?;
    write_file(
        path.join("TcpStream.cpp"),
        &include_str!("cpp/TcpStream.cpp"),
    )?;
    write_file(
        path.join("MyStrategy.hpp"),
        &include_str!("cpp/MyStrategy.hpp"),
    )?;
    write_file(
        path.join("MyStrategy.cpp"),
        &include_str!("cpp/MyStrategy.cpp"),
    )?;
    write_file(path.join("Debug.hpp"), &include_str!("cpp/Debug.hpp"))?;
    write_file(path.join("Debug.cpp"), &include_str!("cpp/Debug.cpp"))?;
    write_file(path.join("main.cpp"), &include_str!("cpp/main.cpp"))?;
    Ok(())
}

fn run(opt: Opt) -> std::io::Result<()> {
    std::fs::remove_dir_all(&opt.path)?;
    gen_python(&opt)?;
    gen_javascript(&opt)?;
    gen_ruby(&opt)?;
    gen_rust(&opt)?;
    gen_java(&opt)?;
    gen_kotlin(&opt)?;
    gen_scala(&opt)?;
    gen_csharp(&opt)?;
    gen_fsharp(&opt)?;
    gen_dlang(&opt)?;
    gen_cpp(&opt)?;
    gen_go(&opt)?;
    Ok(())
}

fn main() -> std::io::Result<()> {
    let opt = Opt::from_args();
    run(opt)
}

#[cfg(test)]
mod tests {
    use crate::*;
    #[test]
    fn test_client_gen() -> std::io::Result<()> {
        let dir = tempfile::tempdir()?;
        run(crate::Opt {
            path: dir.path().to_owned(),
        })
    }

    fn command(cmd: &str) -> std::process::Command {
        let mut parts = cmd.split_whitespace();
        let mut command = if cfg!(windows) {
            let mut command = std::process::Command::new("cmd");
            command.arg("/C").arg(parts.next().unwrap());
            command
        } else {
            std::process::Command::new(parts.next().unwrap())
        };
        for part in parts {
            command.arg(part);
        }
        command
    }

    trait CommandExt {
        fn run(&mut self) -> std::io::Result<()>;
    }

    impl CommandExt for std::process::Command {
        fn run(&mut self) -> std::io::Result<()> {
            let status = self.status()?;
            if status.success() {
                Ok(())
            } else {
                Err(std::io::Error::new(
                    std::io::ErrorKind::Other,
                    status.to_string(),
                ))
            }
        }
    }

    fn test_client(
        build_command: impl FnOnce(&std::path::Path) -> std::io::Result<()>,
        command: impl FnOnce(&std::path::Path, u16) -> std::io::Result<()>,
    ) -> std::io::Result<()> {
        let _ = logger::init();
        let dir = tempfile::tempdir()?;
        run(crate::Opt {
            path: dir.path().to_owned(),
        })?;
        build_command(dir.path())?;
        if let Ok(dir) = std::env::var("CARGO_MANIFEST_DIR") {
            std::env::set_current_dir(dir)?;
        }
        struct AppHandle {
            handle: Option<std::thread::JoinHandle<()>>,
        }
        impl Drop for AppHandle {
            fn drop(&mut self) {
                if let Some(handle) = self.handle.take() {
                    let _ = handle.join();
                }
            }
        }
        static NEXT_PORT: once_cell::sync::Lazy<std::sync::atomic::AtomicU16> =
            once_cell::sync::Lazy::new(|| std::sync::atomic::AtomicU16::new(31001));
        let port = NEXT_PORT.fetch_add(1, std::sync::atomic::Ordering::Relaxed);
        let mut app_handle = AppHandle {
            handle: Some(std::thread::spawn(move || {
                aicup2019::run_with(aicup2019::Opt {
                    actual_config: Some(codegame::FullOptions {
                        seed: None,
                        options_preset: OptionsPreset::Custom(model::Options {
                            properties: Some(model::Properties {
                                max_tick_count: 1000,
                                ..default()
                            }),
                            ..default()
                        }),
                        players: vec![
                            PlayerOptions::Quickstart,
                            PlayerOptions::Tcp(codegame::TcpPlayerOptions {
                                host: None,
                                port,
                                accept_timeout: Some(5.0),
                                timeout: Some(1.0),
                                token: Some("token".to_owned()),
                            }),
                        ],
                    }),
                    batch_mode: true,
                    ..default()
                })
            })),
        };
        std::thread::sleep(std::time::Duration::from_secs(1));
        info!("Starting client");
        let start = std::time::Instant::now();
        command(dir.path(), port)?;
        if let Err(_) = app_handle.handle.take().unwrap().join() {
            return Err(std::io::Error::new(std::io::ErrorKind::Other, "App failed"));
        }
        let passed = std::time::Instant::now().duration_since(start).as_millis();
        info!("Finished in {}.{:03} secs", passed / 1000, passed % 1000);
        Ok(())
    }

    #[test]
    #[ignore]
    fn test_client_python() -> std::io::Result<()> {
        test_client(
            |_| Ok(()),
            |dir, port| {
                command(if cfg!(windows) { "py -3" } else { "python3" })
                    .arg(dir.join("python").join("main.py"))
                    .arg("127.0.0.1")
                    .arg(port.to_string())
                    .arg("token")
                    .run()
            },
        )
    }

    #[test]
    #[ignore]
    fn test_client_javascript() -> std::io::Result<()> {
        test_client(
            |_| Ok(()),
            |dir, port| {
                command("node")
                    .arg("index.js")
                    .arg("127.0.0.1")
                    .arg(port.to_string())
                    .arg("token")
                    .current_dir(dir.join("javascript"))
                    .run()
            },
        )
    }

    #[test]
    #[ignore]
    fn test_client_ruby() -> std::io::Result<()> {
        test_client(
            |_| Ok(()),
            |dir, port| {
                command("ruby")
                    .arg(dir.join("ruby").join("main.rb"))
                    .arg("127.0.0.1")
                    .arg(port.to_string())
                    .arg("token")
                    .run()
            },
        )
    }

    #[test]
    #[ignore]
    fn test_client_rust() -> std::io::Result<()> {
        test_client(
            |dir| {
                command("cargo")
                    .arg("build")
                    .arg("--release")
                    .current_dir(dir.join("rust"))
                    .run()
            },
            |dir, port| {
                command("cargo")
                    .arg("run")
                    .arg("--release")
                    .arg("--")
                    .arg("127.0.0.1")
                    .arg(port.to_string())
                    .arg("token")
                    .current_dir(dir.join("rust"))
                    .run()
            },
        )
    }

    #[test]
    #[ignore]
    fn test_client_java() -> std::io::Result<()> {
        test_client(
            |dir| {
                command("mvn")
                    .arg("package")
                    .arg("--batch-mode")
                    .current_dir(dir.join("java"))
                    .run()
            },
            |dir, port| {
                command("java")
                    .arg("-jar")
                    .arg("target/aicup2019-jar-with-dependencies.jar")
                    .arg("127.0.0.1")
                    .arg(port.to_string())
                    .arg("token")
                    .current_dir(dir.join("java"))
                    .run()
            },
        )
    }

    #[test]
    #[ignore]
    fn test_client_kotlin() -> std::io::Result<()> {
        test_client(
            |dir| {
                command("mvn")
                    .arg("--batch-mode")
                    .arg("package")
                    .current_dir(dir.join("kotlin"))
                    .run()
            },
            |dir, port| {
                command("java")
                    .arg("-jar")
                    .arg("target/aicup2019-jar-with-dependencies.jar")
                    .arg("127.0.0.1")
                    .arg(port.to_string())
                    .arg("token")
                    .current_dir(dir.join("kotlin"))
                    .run()
            },
        )
    }

    #[test]
    #[ignore]
    fn test_client_scala() -> std::io::Result<()> {
        test_client(
            |dir| {
                command("mvn")
                    .arg("--batch-mode")
                    .arg("package")
                    .current_dir(dir.join("scala"))
                    .run()
            },
            |dir, port| {
                command("java")
                    .arg("-jar")
                    .arg("target/aicup2019-jar-with-dependencies.jar")
                    .arg("127.0.0.1")
                    .arg(port.to_string())
                    .arg("token")
                    .current_dir(dir.join("scala"))
                    .run()
            },
        )
    }

    #[test]
    #[ignore]
    fn test_client_csharp() -> std::io::Result<()> {
        test_client(
            |dir| {
                command("dotnet")
                    .arg("build")
                    .arg("-c")
                    .arg("Release")
                    .current_dir(dir.join("csharp"))
                    .run()
            },
            |dir, port| {
                command("dotnet")
                    .arg("run")
                    .arg("-c")
                    .arg("Release")
                    .arg("127.0.0.1")
                    .arg(port.to_string())
                    .arg("token")
                    .current_dir(dir.join("csharp"))
                    .run()
            },
        )
    }

    #[test]
    #[ignore]
    fn test_client_fsharp() -> std::io::Result<()> {
        test_client(
            |dir| {
                command("dotnet")
                    .arg("build")
                    .arg("-c")
                    .arg("Release")
                    .current_dir(dir.join("fsharp"))
                    .run()
            },
            |dir, port| {
                command("dotnet")
                    .arg("run")
                    .arg("-c")
                    .arg("Release")
                    .arg("127.0.0.1")
                    .arg(port.to_string())
                    .arg("token")
                    .current_dir(dir.join("fsharp"))
                    .run()
            },
        )
    }

    #[test]
    #[ignore]
    fn test_client_dlang() -> std::io::Result<()> {
        test_client(
            |dir| {
                command("dub")
                    .arg("build")
                    .arg("-b")
                    .arg("release")
                    .current_dir(dir.join("dlang"))
                    .run()
            },
            |dir, port| {
                command("dub")
                    .arg("run")
                    .arg("-b")
                    .arg("release")
                    .arg("--")
                    .arg("127.0.0.1")
                    .arg(port.to_string())
                    .arg("token")
                    .current_dir(dir.join("dlang"))
                    .run()
            },
        )
    }

    #[test]
    #[ignore]
    fn test_client_go() -> std::io::Result<()> {
        test_client(
            |dir| {
                command("go")
                    .arg("build")
                    .arg("-o")
                    .arg(if cfg!(windows) {
                        "aicup2019.exe"
                    } else {
                        "aicup2019"
                    })
                    .current_dir(dir.join("go"))
                    .run()
            },
            |dir, port| {
                command(
                    std::path::PathBuf::from(".")
                        .join(if cfg!(windows) {
                            "aicup2019.exe"
                        } else {
                            "aicup2019"
                        })
                        .to_str()
                        .unwrap(),
                )
                .arg("127.0.0.1")
                .arg(port.to_string())
                .arg("token")
                .current_dir(dir.join("go"))
                .run()
            },
        )
    }

    fn test_client_cpp_with_standard(standard: &str) -> std::io::Result<()> {
        let _ = logger::init();
        info!("Testing with C++{}", standard);
        test_client(
            |dir| {
                command("cmake")
                    .arg(format!("-DCMAKE_CXX_STANDARD={}", standard))
                    .arg("-DCMAKE_BUILD_TYPE=RELEASE")
                    .arg("-DCMAKE_VERBOSE_MAKEFILE=ON")
                    .arg(".")
                    .current_dir(dir.join("cpp"))
                    .run()?;
                command("cmake")
                    .arg("--build")
                    .arg(".")
                    .arg("--config")
                    .arg("Release")
                    .current_dir(dir.join("cpp"))
                    .run()
            },
            |dir, port| {
                command(
                    std::path::PathBuf::from(if cfg!(windows) { "Release" } else { "." })
                        .join(if cfg!(windows) {
                            "aicup2019.exe"
                        } else {
                            "aicup2019"
                        })
                        .to_str()
                        .unwrap(),
                )
                .arg("127.0.0.1")
                .arg(port.to_string())
                .arg("token")
                .current_dir(dir.join("cpp"))
                .run()
            },
        )
    }

    #[test]
    #[ignore]
    fn test_client_cpp11() -> std::io::Result<()> {
        test_client_cpp_with_standard("11")
    }

    #[test]
    #[ignore]
    fn test_client_cpp14() -> std::io::Result<()> {
        test_client_cpp_with_standard("14")
    }

    #[test]
    #[ignore]
    fn test_client_cpp17() -> std::io::Result<()> {
        test_client_cpp_with_standard("17")
    }
}
