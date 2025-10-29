main := 
    let Alice.choice := Alice.read_line Alice.(); in
    let handler1 := fun Bob.x -> Bob.(x * 2); in 