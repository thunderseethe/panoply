effect State {
    get : {} -> Int,
    put : Int -> {}
}

effect Reader {
    ask : {} -> Int
}

main = (with  {
  get = |x| |k| |s| k(s)(s),
  put = |x| |k| |s| k({})(x),
  return = |x| |s| { state = s, value = x },
} do (with {
  ask = |x| |k| k(16777215),
  return = |x| x,
} do w = State.put(Reader.ask({})); State.get({})))(14).value
