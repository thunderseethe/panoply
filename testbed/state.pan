effect State {
    get : {} -> Int,
    put : Int -> {}
}

main = (with {
    get = |x| |k| |s| k(s)(s),
    put = |x| |k| |s| k({})(x),
    return = |x| |s| { state = s, value = x },
} do State.get({}))(16777215).value
