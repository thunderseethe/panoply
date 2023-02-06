use aiahr_core::id::IrVarId;
use rustc_hash::FxHashMap;

struct Prompt(usize);

enum Value {
    Lam { env: FxHashMap<IrVarId, Value> },
}
