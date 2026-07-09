pub(super) fn xor<T, U>(mut left: T, right: U) -> T
where
    T: AsMut<[u8]>,
    U: AsRef<[u8]>,
{
    left.as_mut()
        .iter_mut()
        .zip(right.as_ref().iter())
        .map(|(l, r)| *l ^= r)
        .last();
    left
}

pub(super) fn to_u8_32(bytes: impl AsRef<[u8]>) -> [u8; 32] {
    let mut out = [0; 32];
    out[..].copy_from_slice(bytes.as_ref());
    out
}
