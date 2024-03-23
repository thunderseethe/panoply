effect Reader {
  ask : {} -> Int
}

f = |e| (with {
    ask = |x| |k| k(e),
    return = |x| x,
} do Reader.ask({}))

main = f(16777215)
