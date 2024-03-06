effect State {
    get : {} -> Int,
    put : Int -> {}
}

effect Reader {
    ask : {} -> Int
}

f = |env| with {
  ask = |x| |k| k(env),
  return = |x| x,
} do State.put(Reader.ask({}))

main = (with  { 
  get = |x| |k| |s| k(s)(s),
  put = |x| |k| |s| k({})(x),
  return = |x| |s| { state = s, value = x },
} do w = f(16777215); State.get({}))(14).value
