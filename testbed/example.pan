effect Reader {
  ask : {} -> Int
}

main = (with {
    ask = |x| |k| k(52),
    return = |x| x
} do Reader.ask({}))

