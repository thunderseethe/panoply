effect State {
  get : {} -> Int,
  put : Int -> {}
}

effect Reader {
  ask : {} -> Int
}

foo = |env| with  {
  get = |x| |k| |s| k(s)(s),
  put = |x| |k| |s| k({})(x),
  return = |x| |s| { state = s, value = x },
} do (with {
  ask = |x| |k| k(env),
  return = |x| State.put(x)
} do Reader.ask({}))

main = foo(78543)(16763).state
