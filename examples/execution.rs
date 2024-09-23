use corosensei::Execution;

struct Yield {
    exec: Execution<Option<Yield>>,
    input: i32,
}

fn main() {
    println!("[main] creating coroutine");

    let mut exec = Execution::new(|exec: Execution<Option<Yield>>| {
        let Yield {
            mut exec,
            mut input,
        } = exec.switch(|exec| Some(Yield { exec, input: 0 })).unwrap();
        println!("[coroutine] coroutine started with input {}", input);
        for i in 0..5 {
            println!("[coroutine] yielding {}", i);
            Yield { exec, input } = exec
                .switch(move |exec| Some(Yield { exec, input: i }))
                .unwrap();
            println!("[coroutine] got {} from parent", input)
        }
        println!("[coroutine] exiting coroutine");
        (exec, |_| None)
    });

    let mut counter = 100;
    loop {
        println!("[main] resuming coroutine with argument {}", counter);
        match exec.switch(move |exec| {
            Some(Yield {
                exec,
                input: counter,
            })
        }) {
            Some(Yield { exec: new, input }) => {
                exec = new;
                println!("[main] got {:?} from coroutine", input)
            }
            None => break,
        }

        counter += 1;
    }

    println!("[main] exiting");
}

#[test]
fn execution() {
    main()
}
