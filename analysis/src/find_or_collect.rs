pub(crate) trait FindOrCollect: Iterator {
    fn find_or_collect<B, E, F, C>(self, mut f: F) -> Result<B, C>
    where
        Self: Sized,
        F: FnMut(Self::Item) -> Result<B, E>,
        C: FromIterator<E>,
    {
        let mut ok = None;
        let errs = self
            .filter_map(|x| match f(x) {
                Ok(b) => {
                    ok.get_or_insert(b);
                    None
                }
                Err(e) => Some(e),
            })
            .collect();
        ok.ok_or(errs)
    }
}
impl<I> FindOrCollect for I where I: Iterator {}
