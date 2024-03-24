effect State {
  get : {} -> Int,
  put : Int -> {}
}

effect Reader {
  ask : {} -> Int
}

f env = with {
  ask = |x| |k| k(env),
  return = |x| x
} do (with {
  ask = |x| |k| k(5678),
  return = |x| Reader.ask({})
} do Reader.ask({}))

main = f 1234