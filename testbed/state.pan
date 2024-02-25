effect State {
    get : {} -> Int,
    put : Int -> {}
}

f = |x| (with {
    get = |x| |k| |s| k(s)(s),
    put = |x| |k| |s| k({})(x),
    return = |x| |s| { state = s, value = x },
} do State.get({}))(x)

main = f(16777215).value
